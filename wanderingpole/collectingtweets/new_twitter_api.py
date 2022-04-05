import requests
import os
import json
import logging
from pathlib import Path
from dateutil.parser import parse
from pymongo import MongoClient
from pymongo.errors import BulkWriteError

from searchtweets import ResultStream, gen_request_parameters, load_credentials, collect_results

logger = logging.getLogger(__name__)

# mongo
_ROOT = Path(os.path.abspath(os.path.dirname(__file__))).as_posix()

def get_cred_path(path):
    return Path('/'.join(_ROOT.split('/')[:-2]), 'wanderingpole', path).joinpath()


with open(get_cred_path('mongo_uri.txt'), 'r') as _file:
    mongo_uri = _file.read()

db = MongoClient(mongo_uri).wanderingpole

search_args = load_credentials(get_cred_path('.twitter_keys.yaml'),
                                       yaml_key="search_tweets_v2",
                                       env_overwrite=True)


def store_tweets(statuses, collection):
    """
    Stores the tweet in the mongoDB
    :param tweet: dict or list of dicts to store
    """
    if isinstance(statuses, dict):
        statuses = [statuses]

    try:
        result = db[collection].insert_many(statuses, ordered=False)
        return result

    except BulkWriteError as e_sum:
        return e_sum


def get_latest_tweet(screen_name, mdb_collection):
    """
    gets the latest tweet from a given screen name in the collection
    """
    try:
        latest = db[mdb_collection].find({'author_username': screen_name}, sort=[('created_at', -1)])[0]
        return latest

    except:
        return None
    

def get_tweets_user(screen_name: str, smart_stop=False, mdb_collection='tweets2', return_result=False) -> list:
    """
    function to pull *ALL* the tweets of a specific user
    """
    # create the query
    # TODO - Check the DB and use a start at ID system
    # get most latest (in time) tweet as starting point
    latest = get_latest_tweet(screen_name, mdb_collection)

    if latest:
        query = gen_request_parameters(
            query=f"from:{screen_name}",
            tweet_fields='id,text,created_at,conversation_id,entities,geo,in_reply_to_user_id,lang,public_metrics,author_id',
            expansions='referenced_tweets.id.author_id', #referenced_tweets.id,
            since_id=latest['id'],
            results_per_call=500
        )
    else:
        query = gen_request_parameters(
            query=f"from:{screen_name}",
            tweet_fields='id,text,created_at,conversation_id,entities,geo,in_reply_to_user_id,lang,public_metrics,author_id',
            expansions='referenced_tweets.id.author_id', #referenced_tweets.id,
            start_time='2006-04-01',
            results_per_call=500
        )
    # execute the query
    tweets = collect_results(query,
                         max_tweets=1000000,
                         result_stream_args=search_args)
    # clean out the pagination artifacts
    tweets = [tt for tt in tweets if 'id' in tt]
    # put the username in for ease of access
    for tweet in tweets:
        tweet.update({'author_username': screen_name})

    # fix the dates
    for tweet in tweets:
        tweet.update({'created_at': parse(tweet['created_at'])})

    # insert into the db
    if mdb_collection:
        try:
            insertion = store_tweets(tweets, mdb_collection)
        except:
            logger.error(f'Error on {screen_name}')

    print(f'Collected {len(tweets)} tweets for {screen_name}')

    if return_result:
        return tweets

    