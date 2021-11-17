"""
This here's the code for pulling data from the twitter api
code for pulling:
    1. User information
    2. Users's first 3200 (or so) tweets
    3. Lists of tweet ids not caught in first round and collected in selenium

@ S.S. Dorsey
"""
from typing import List, Dict
import pandas as pd
from datetime import datetime
import json
from time import sleep
import math
import tweepy
from tqdm import tqdm, trange
import os
from pathlib import Path

from pymongo import MongoClient
from pymongo.errors import BulkWriteError

# ------------------------------------------------------------------------------
# credentials
# ------------------------------------------------------------------------------
# mongo
_ROOT = Path(os.path.abspath(os.path.dirname(__file__))).as_posix()

def get_cred_path(path):
    return Path('/'.join(_ROOT.split('/')[:-2]), 'wanderingpole', path).joinpath()


with open(get_cred_path('mongo_uri.txt'), 'r') as _file:
    mongo_uri = _file.read()

db = MongoClient(mongo_uri).wanderingpole

#Twitter API credentials
with open(get_cred_path('keys_secrets.json'), 'r') as _file:
    keys = json.load(_file)

auth = tweepy.AppAuthHandler(keys['consumer_key'], keys['consumer_secret'])

api = tweepy.API(auth, wait_on_rate_limit=True)

# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------
def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]


def convert_date(date_string: str) -> datetime:
    """
    converts the twitter datestring to a python datetime
    """
    return datetime.strptime(date_string, '%a %b %d %H:%M:%S %z %Y')


def user_details(handles: list) -> list:
    """
    get the counts of all listed users
    """
    user_info = []
    chunked_handles = chunks(handles, 100)
    for ch in chunked_handles:
        users = api.lookup_users(screen_names=ch)
        for user in users:
            user_info.append(user._json)
    return user_info


def process_tweet(tweet):
    """
    takes the api return and turns it into something useful

    input: single tweet from tweepy
    output: dictionary of the data I want
    """
    _json = tweet._json
    _json['date_collected'] = datetime.now()
    if not isinstance(_json['created_at'], datetime):
        _json['created_at'] = convert_date(_json['created_at'])
    if not isinstance(_json['user']['created_at'], datetime):
        _json['user']['created_at'] = convert_date(_json['user']['created_at'])
    return _json


def store_tweets(statuses):
    """
    Stores the tweet in the mongoDB
    :param tweet: dict or list of dicts to store
    """
    if isinstance(statuses, dict):
        statuses = [statuses]

    try:
        result = db.tweets.insert_many(statuses, ordered=False)
        return result

    except BulkWriteError as e_sum:
        return e_sum


def get_tweets_list(ids: list) -> list:
    """
    download a list of tweets based on ids
    """
    # set up batching for the Twitter api
    cids = chunks(ids, 100)
    # get the data
    processed = []
    for id_batch in cids:
        # pull the tweets from the api
        tweets = api.statuses_lookup(id_batch, tweet_mode='extended')
        # process the tweets
        for tweet in tweets:
            processed.append(process_tweet(tweet))
    return processed


def get_tweets_user(screen_name: str, smart_stop=False, return_result=False) -> list:
    """
    use the functions to download and save the most recent 3240 tweets
    """
    # get new
    print('# --------------------------------')
    print('{} via api'.format(screen_name))
    print('# --------------------------------')
    # set up the cursor to call the tweets
    cursor = tweepy.Cursor(api.user_timeline,
                           screen_name=screen_name,
                           tweet_mode='extended', 
                           count=16100
                           ).pages()

    all_processed = []
    pbar = tqdm()
    for page in cursor:
        # process
        sup_processed = []
        for tweet in page:
            processed = process_tweet(tweet)
            sup_processed.append(processed)
        all_processed.extend(sup_processed)
        # insert
        res = store_tweets(sup_processed)
        # update the bar
        pbar.update(len(sup_processed))
        # check for duplicates
        if smart_stop:
            if isinstance(res, BulkWriteError):
                break
    pbar.close()
    
    if return_result:
        return all_processed
