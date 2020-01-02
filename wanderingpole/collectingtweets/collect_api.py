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

# ------------------------------------------------------------------------------
# credentials
# ------------------------------------------------------------------------------
#Twitter API credentials
consumer_key =
consumer_secret =
access_key =
access_secret =

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
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
    # dictionary for holding everything
    hold_dict = {}
    # pulling all the values
    hold_dict['screen_name'] = tweet.user.screen_name
    hold_dict['tweet_id'] = tweet.id_str
    hold_dict['date'] = tweet.created_at.strftime("%Y-%m-%d %H:%M")
    hold_dict['date_collected'] = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    if 'full_text' in dir(tweet):
        hold_dict['text'] = tweet.full_text
    else:
        hold_dict['text'] = tweet.text
    hold_dict['retweets'] = tweet.retweet_count
    hold_dict['favorites'] = tweet.favorite_count
    hold_dict['mentions'] = [user['screen_name'] for user in tweet._json['entities']['user_mentions']]
    hold_dict['retweeted'] = tweet.retweeted
    hold_dict['reply_to'] = tweet.in_reply_to_screen_name
    if hold_dict['reply_to'] is None:
        hold_dict['reply_to'] = '' # check for compatibility
    if 'hashtags' in tweet._json['entities']:
        hold_dict['hashes'] = [hash['text'] for hash in tweet._json['entities']['hashtags']]
    else: hold_dict['hashes'] = []
    if 'urls' in tweet._json['entities']:
        hold_dict['gen_links'] = [link['expanded_url'] for link in tweet._json['entities']['urls']]
    else: hold_dict['gen_links'] = []
    # some exception handling below because many posts don't have pictures/videos
    try:
        if tweet._json['extended_entities']['media'][0]['type'] == 'photo':
            hold_dict['photo_link'] = tweet._json['extended_entities']['media'][0]['media_url']
    except KeyError:
        hold_dict['photo_link'] = ''
    try:
        if tweet._json['extended_entities']['media'][0]['type'] == 'video':
            hold_dict['video_link'] = (tweet._json['extended_entities']['media']
                                       [0]['video_info']['variants'][0]['url'])
    except KeyError:
        hold_dict['video_link'] = ''
    return hold_dict


def get_tweets_list(ids, screen_name):
    """
    download a list of tweets based on id
    """
    print('# --------------------------------')
    print(f'{screen_name} via api')
    print('# --------------------------------')
    #initialize a list to hold all the tweepy Tweets
    all_data = []
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
            all_data.append(process_tweet(tweet))
        # move to next batch
        start += 100
        end += 100
        # update the progress bar
        my_bar.update(go)
    # load what I already had and write in new# 
    all_df = pd.DataFrame(all_data)
    prev = pd.read_json(f'data/{screen_name}_tweets.json')
    prev = pd.concat([prev, all_df])
    prev = prev.drop_duplicates(subset=['tweet_id'])
    prev.to_json(f'data/{screen_name}_tweets.json')

    # with open(f'data/{screen_name}_tweets.json') as f:
    #     prev_json = json.load(f)
    # prev_json += all_data
    # with open(f'data/{screen_name}_tweets.json', 'w') as f:
    #     json.dump(prev_json, f)


def get_tweets_user(screen_name):
    """
    use the functions to download and save the most recent 3240 tweets
    """
    # load in the data I already have
    outname = 'data/{}_tweets.json'.format(screen_name)
    try:
        old_tweets = pd.read_json(outname)
        old_ids = set(old_tweets['tweet_id'])
        # with open(outname) as f:
        #     old_tweets = json.load(f)
        # old_ids = [t['tweet_id'] for t in old_tweets]
    except:
        old_tweets = []
        old_ids = []
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
            # break if I already have a tweet(caught up to history)
            # if processed['tweet_id'] in old_ids:
            #     all_new_tweets.append(processed)
            #     break
            # else:
            all_new_tweets.append(processed)
            # update progress
            my_bar.update(len(all_new_tweets))
        except tweepy.TweepError:
            print('Tweepy error, moving to next handle')
            # sleep(60 * 15)
            # continue
            break
        except StopIteration:
            break
    if len(all_new_tweets) > 0:
        # append the new tweets
        all_new_tweets_df = pd.DataFrame(all_new_tweets)
        if len(old_tweets) > 0:
            tweets = pd.concat([old_tweets, all_new_tweets_df], sort=True)
        else:
            tweets = all_new_tweets_df
        # write to disc
        if len(tweets) > 0:
            tweets = tweets.drop_duplicates(subset=['tweet_id'])
            tweets = tweets.reset_index(drop=True)
            tweets.to_json(outname)
        # with open(outname, 'w') as fout:
        #     json.dump(tweets, fout)
    else:
        # if no new tweets, sleep for a minute to avoid rate limits
        print('No new tweets')
        # sleep(60)
