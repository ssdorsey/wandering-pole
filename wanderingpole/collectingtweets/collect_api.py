"""
This here's the code for pulling data from the twitter api
code for pulling:
    1. User information
    2. Users's first 3200 (or so) tweets
    3. Lists of tweet ids not caught in first round and collected in selenium

@ S.S. Dorsey
"""
import pandas as pd
import datetime
import json
from time import sleep
import math
import tweepy
import progressbar

from pymongo.errors import DuplicateKeyError
from pymongo import MongoClient


# ------------------------------------------------------------------------------
# credentials
# ------------------------------------------------------------------------------
# mongo
with open('mongo_uri.txt', 'r') as _file:
    mongo_uri = _file.read()

db = MongoClient(mongo_uri).wanderingpole

#Twitter API credentials
with open('keys_secrets.json', 'r') as _file:
    keys_secrets = json.load(_file)

auth = tweepy.OAuthHandler(keys_secrets['consumer_key'], keys_secrets['consumer_secret'])
auth.set_access_token(keys_secrets['access_key'], keys_secrets['access_secret'])
api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)

# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------

def user_details(handles):
    """
    get the counts of all listed users
    """
    user_info = []
    chunked_handles = [handles[ii:ii + 100] for ii in range(0, len(handles), 100)]
    for ch in chunked_handles:
        users = api.lookup_users(screen_names=ch)
        for user in users:
            user_info.append([user.screen_name,
                              user.statuses_count,
                              user.created_at,
                              user.protected
                              ])
    return user_info


def process_tweet(tweet):
    """
    takes the api return and turns it into something useful

    input: single tweet from tweepy
    output: dictionary of the data I want
    """
    _json = tweet._json
    _json['date_collected'] = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")

    try:
        db.tweets.insert_one(
            _json
        )
    except DuplicateKeyError:
        print(f'Error inserting: {_json["id"]}')
        pass
    return _json


def get_tweets_list(ids):
    """
    download a list of tweets based on ids
    """
    # set up batching for the Twitter api
    start = 0
    end = 100
    limit = len(ids)
    i = math.ceil(limit / 100)
    # progressbar
    my_bar = progressbar.ProgressBar(max_value=i)
    # get the data
    for go in range(i):
        id_batch = ids[start:end]
        # pull the tweets from the api
        tweets = api.statuses_lookup(id_batch)
        # process the tweets
        for tweet in tweets:
            process_tweet(tweet)
        # move to next batch
        start += 100
        end += 100
        # update the progress bar
        my_bar.update(go)


def get_tweets_user(screen_name):
    """
    use the functions to download and save the most recent 3240 tweets
    """
    old_cursor = db.tweets.find(
        {'user.screen_name': screen_name}
    )
    old_ids = set([doc['id'] for doc in old_cursor])
    # get new
    print('# --------------------------------')
    print('{} via api'.format(screen_name))
    print('# --------------------------------')
    all_new_tweets = []
    # set up the cursor to call the tweets
    cursor = tweepy.Cursor(api.user_timeline,
                           screen_name=screen_name,
                           tweet_mode='extended'
                           ).items()
    # collect everything
    my_bar = progressbar.ProgressBar(max_value=progressbar.UnknownLength)
    while True:
        try:
            new_tweet = cursor.next()
            # process the tweets
            processed = process_tweet(new_tweet)
            all_new_tweets.append(processed)
            # break if I already have a tweet(caught up to history)
            if processed['id'] in old_ids:
                break
            # update progress
            my_bar.update(len(all_new_tweets))
        except tweepy.TweepError:
            print('Tweepy error, moving to next handle')
            # sleep(60 * 15)
            # continue
            break
        except StopIteration:
            break

