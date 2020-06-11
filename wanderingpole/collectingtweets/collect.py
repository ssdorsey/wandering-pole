import pandas as pd
# my own scripts
from wanderingpole.collectingtweets import collect_api
import os
import json

# ------------------------------------------------------------------------------
# import rep data
# ------------------------------------------------------------------------------
twdf = pd.read_csv('wanderingpole/data/master_handles.csv')
twdf.loc[:,'twitter'] = twdf['twitter'].str.lower()

# figure out who needs selenium, send rest to api
handles = list(twdf['twitter'])

if not os.path.isfile('wanderingpole/data/sub_handles.csv'):
    tweet_counts = collect_api.user_details(handles)
    df_count = pd.DataFrame(data=tweet_counts, columns=['handle',
                                                        'count',
                                                        'start_date',
                                                        'protected'])
    df_count.to_csv('wanderingpole/data/sub_handles.csv', index=False)
else:
    df_count = pd.read_csv('wanderingpole/data/sub_handles.csv')

# ------------------------------------------------------------------------------
# get as much as I can through the api
# ------------------------------------------------------------------------------
api_reps = df_count[df_count['protected']==False]

api_reps.head()

for screen_name in api_reps['handle']:
    collect_api.get_tweets_user(screen_name)

