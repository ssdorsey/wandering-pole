import os
import zipfile
import shutil
import logging
import pandas as pd
from pandas.core.indexes import multi
from tqdm import tqdm
import urllib.request

PATH_TO_REPS = r"D:\Dropbox\Twitter\US Congress Handles Master List.csv"

def download_data(year):
    """
    function to do a bulk download from the FEC
    note: this could probably be done more effeciently though the FEC API

    :param year: the year we're interested in

    """
    year = str(year)
    # build the url
    url = f'https://www.fec.gov/files/bulk-downloads/{year}/indiv{year[-2:]}.zip'
    # execute download
    urllib.request.urlretrieve(url, f'indiv{year[-2:]}.zip')
    # unzip
    with zipfile.ZipFile(f'indiv{year[-2:]}.zip', 'r') as zip_ref:
        zip_ref.extractall(f'indiv{year[-2:]}')


def create_rep_frame(year):
    """
    creates a pandas dataframe for the dates and reps we're interested in

    :param year: the year we're interested in
    :return: a multi-indexed dataframe with all 0 values
    """
    # first, read in the representatives we want
    reps = pd.read_csv(PATH_TO_REPS)

    # pull just the FEC ids
    fec = reps['FECcommittee']
    # create a date range
    days = list(
        pd.date_range(start=f'1/1/{int(year)-1}', end=f'31/12/{year}', freq='D')
    )
    # track the unique day count
    unique_days = days
    # repeat days for data frame
    days = days * len(fec)

    # repeat the FEC ids for each day
    fec = [ff for ff in fec for i in range(len(unique_days))]
    fec_set = set(fec)

    # create MultiIndex
    multiindex = pd.MultiIndex.from_tuples(zip(fec, days))
    # drop MultiIndex duplicates
    multiindex = multiindex.drop_duplicates()

    return pd.DataFrame({'amount': 0, 'donors': 0}, index=multiindex)


def process_contributions(year, df):
    """
    script to go through the downloaded contributions and tally them up

    :param year: the year we're interested in
    """
    # get some general info from the df
    fec_set = set(df.index.get_level_values(0))
    unique_days = set(df.index.get_level_values(1))

    # go through (line by line) and get their daily contributions
    names = ['CMTE_ID', 'ENTITY_TP', 'STATE', 'TRANSACTION_DT', 'TRANSACTION_AMT']
    changed = 0
    with open(f'indiv{year[-2:]}\itcont.txt', 'r') as f:

        for line in tqdm(f):
            try:
                _split = line.split('|')
                contrib = dict(zip(names, [_split[0], _split[6], _split[9], _split[13], _split[14]]))
                # convert to datetime
                contrib['TRANSACTION_DT'] = pd.to_datetime(
                    contrib['TRANSACTION_DT'],
                    format='%m%d%Y'
                )
                # remove 0 and negative donations
                if contrib['TRANSACTION_AMT'] == '0' or float(contrib['TRANSACTION_AMT']) < 0:
                    continue
                # remove self-donations (CAN)
                if contrib['ENTITY_TP'] == 'CAN':
                    continue
                # continue past contributions I'm not interested in
                if contrib['CMTE_ID'] not in fec_set:
                    continue
                # only use contributions from the dates I care about
                if contrib['TRANSACTION_DT'] not in unique_days:
                    continue
                # count the contribution
                df.loc[
                    (contrib['CMTE_ID'], contrib['TRANSACTION_DT']),
                    'amount'] += float(contrib['TRANSACTION_AMT'])
                changed += 1

                # count the donor
                df.loc[(contrib['CMTE_ID'], contrib['TRANSACTION_DT']),
                'donors'] += 1
        
            except ValueError or IndexError:
                logging.error(f'ERROR ON: {line}')
                continue
    logging.info(f'CONTRIBUTIONS PROCESSED: {changed}')
    return df

if __name__ == '__main__':
    # get year from input
    year = input('Enter the latter year of a pair to download.' 
                    'For example, for 2015-2016, enter 2016: ')
    # set up logging
    logging.basicConfig(filename=f'{year}_contributions.log', level=logging.INFO)    
    # download data
    download_data(year)
    # create dataframe
    rep_frame = create_rep_frame(year)
    # read/process the data
    rep_frame = process_contributions(year, rep_frame)
    # save the counts
    rep_frame.to_csv(f'../data/contributions_{int(year)-1}_{year}.csv')
    # delete the source data
    os.remove(f'indiv{year[-2:]}.zip') # delete compressed file
    shutil.rmtree(f'indiv{year[-2:]}') # delete unzipped files
