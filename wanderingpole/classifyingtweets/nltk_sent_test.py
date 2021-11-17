import pandas as pd
import numpy as np
import json
from simpletransformers.classification import ClassificationModel
import sklearn
import torch
import re
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import os
from tqdm import tqdm
from nltk.sentiment.vader import SentimentIntensityAnalyzer

np.random.seed(619)

# # import the data
# tweets = pd.read_csv('data/postIR_final.csv')
# # restrict to tweets with coding
# tweets = tweets[tweets['divisive'].isin([0,1])]

# # delete links, (convert emoticons)


# # fix columns names
# tweets = tweets.loc[:, ['text', 'divisive']]
# tweets.columns = ['text', 'labels']

# # delete links, (convert emoticons)
# re_url = r"(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))"
# tweets['text'] = tweets['text'].replace(re_url, '', regex=True)

# # split train/test
# tweets.loc[: , 'split'] = np.random.choice([0,1], len(tweets), p=[.8, .2])
# train = tweets.loc[tweets.split==0 ,['text', 'labels']]
# test = tweets.loc[tweets.split==1 ,['text', 'labels']]

# train = train[train['text'].str.len()>0]

tweets = pd.read_csv('data/new_model_training.csv', encoding='utf-8')
tweets = tweets.rename(columns={'uncivil': 'divisive'})
# restrict to tweets with coding
tweets = tweets[tweets['divisive'].isin([0,1])]

# fix columns names
tweets = tweets.loc[:, ['text', 'divisive']]
tweets.columns = ['text', 'labels']

# import other batch
mike = pd.read_excel('data/new_pull_Michael.xls')
mike = mike[['full_text', 'uncivil']]
mike = mike.rename(columns={'full_text': 'text', 'uncivil': 'labels'})

# extra
mike_extra = pd.read_csv('data/michael_extra.csv')
mike_extra = mike_extra.rename(columns={'full_text': 'text', 'uncivil': 'labels'})

# pull a bunch of old 0's
old_model = pd.read_csv("D:/Dropbox/Twitter/allMCtweets.csv", encoding='latin1')
old_0 = old_model[old_model['polarizing']==0].sample(7432, random_state=619)
old_0 = old_0[['text']]
old_0['labels'] = 0

# combine the new data
tweets = pd.concat([tweets, mike, mike_extra, old_0])

# drop incomplete data
tweets = tweets[tweets['labels'].isin([0,1])]

# drop duplicates
tweets = tweets.drop_duplicates(subset=['text'])

# delete links
# TODO: convert emoticons
re_url = r"(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))"
tweets['text'] = tweets['text'].replace(re_url, '', regex=True)

# remove retweet header
re_retweet = r"RT\s@\w+:"
tweets['text'] = tweets['text'].replace(re_retweet, '', regex=True)

# double-check for weird excel handling of ampersand 
re_amp = r'&amp;'
tweets['text'] = tweets['text'].replace(re_amp, '', regex=True)

# split train/test
# tweets.loc[: , 'split'] = np.random.choice(['train','validate','test'], len(tweets), p=[.85, .15])
# train = tweets.loc[tweets.split=='train']
# validate = tweets.loc[tweets.split=='validate']
# test = tweets.loc[tweets.split=='test']
tweets.loc[: , 'split'] = np.random.choice(['train','test'], len(tweets), p=[.85, .15])
train = tweets.loc[tweets.split=='train']
test = tweets.loc[tweets.split=='test']


sid = SentimentIntensityAnalyzer()
res = []
for sentence in test['text']:
    sent = 0
    ss = sid.polarity_scores(sentence)

    if ss['compound'] < -0.05:
        sent = 1

    res.append(sent)

print(sklearn.metrics.classification_report(y_true=list(test['labels']), y_pred=res))
