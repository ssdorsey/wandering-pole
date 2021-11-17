import pandas as pd
# my own scripts
from wanderingpole.collectingtweets import new_twitter_api
import os
import json
from tweepy.errors import TweepyException
from pathlib import Path

if not os.path.exists('data/'):
    os.makedirs('data/')

# import tools
_ROOT = Path(os.path.abspath(os.path.dirname(__file__))).as_posix()

def get_cred_path(path):
    return Path('/'.join(_ROOT.split('/')[:-2]), 'wanderingpole', path).joinpath()

# ------------------------------------------------------------------------------
# import rep data
# ------------------------------------------------------------------------------
# twdf = pd.read_csv('data/master_handles.csv')
# twdf.loc[:,'twitter'] = twdf['twitter'].str.lower()

# # figure out who needs selenium, send rest to api
# handles = list(twdf['twitter'])

# if not os.path.isfile('data/sub_handles.csv'):
#     # tweet_counts = collect_api.user_details(handles)
#     df_count = pd.DataFrame(data=tweet_counts, columns=['handle',
#                                                         'count',
#                                                         'start_date',
#                                                         'protected'])
#     df_count.to_csv('data/sub_handles.csv', index=False)
# else:
#     df_count = pd.read_csv('data/sub_handles.csv')

# Import all the different lists of handles
congresses = [pd.read_csv(get_cred_path(f'data/{cc}thTwitterInfo.csv'), 
    encoding='latin') for cc in range(111, 118)]
cong_df = pd.concat(congresses)

# old master files
master = pd.read_csv(get_cred_path('data/master_handles.csv'))
masterFEC = pd.read_csv(get_cred_path('data/master_handlesFEC_Official.csv'))
master_users = list(set(pd.concat([master, masterFEC])['twitter']))


# make sure we only keep unique of each handle
handles = list(set(cong_df['twitter']))
handles = list(set(handles + master_users))


# ------------------------------------------------------------------------------
# get as much as I can through the api
# ------------------------------------------------------------------------------
# api_reps = df_count[df_count['protected']==False]

# api_reps.head()

if __name__ == "__main__": 
    # for screen_name in api_reps['handle']:
    for screen_name in handles:
        try:
            new_twitter_api.get_tweets_user(screen_name)
        except TweepyException:
            print(f'{screen_name} UNAVAILABLE')

