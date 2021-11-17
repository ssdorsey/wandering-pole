import pandas as pd
import numpy as np
from pathlib import Path
import json
from simpletransformers.classification import ClassificationModel, ClassificationArgs
import sklearn
from sklearn.model_selection import train_test_split
import torch
import re
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import os
from tqdm import tqdm

from pymongo import MongoClient, DESCENDING

np.random.seed(2)


def pull_clean_text(username, text):
      """
      pulls clean text + ID from the mongoDB
      THIS BASICALLY DOESN'T WORK
      """
      cur = db.tweets.find(
            {
                  'author_username': username,
                  '$text': {'$search': text}
            }, {
                  'score': { '$meta': "textScore" }
            }
      ).sort([('score', {'$meta': 'textScore'})])

      res = cur[0]
      # deal with low scores
      if res['score'] < 9:
            return
      else:
            return res['text']

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

# -----------------
# import the data
# -----------------
# The old data
old = pd.read_csv(r"D:\Dropbox\wandering-pole\wanderingpole\data\combined_training_ab.csv")
# old = old.sort_values(by='screen_name')
# text_clean = []
# for ii in tqdm(old.index):
#       try:
#             text_clean.append(pull_clean_text(old['screen_name'].iloc[ii], old['text'].iloc[ii]))
#       except IndexError:
#             text_clean.append(None)

# old['text_clean'] = text_clean
# old.to_csv(r"D:\Dropbox\wandering-pole\wanderingpole\data\combined_training_ab.csv")
old = old[['text', 'uncivil_final']]
old = old.rename(columns={'uncivil_final': 'labels'})


# negative bias
neg = pd.read_csv(r"D:\Dropbox\Twitter\training_data\training_final.csv", encoding='latin1')
# neg = neg.sort_values(by='screen_name')
# text_clean = []
# for ii in tqdm(neg.index):
#       try:
#             text_clean.append(pull_clean_text(neg['screen_name'].iloc[ii], neg['text'].iloc[ii]))
#       except IndexError:
#             text_clean.append(None)
# neg['text_clean'] = text_clean
# neg.to_csv(r"D:\Dropbox\Twitter\training_data\training_final.csv")
neg = neg[['text', 'uncivil_final']]
neg = neg.rename(columns={'uncivil_final': 'labels'})

# neutral bias
neutral = pd.read_excel(r"D:\Dropbox\wandering-pole\wanderingpole\data\neutral_sample.xls")
neutral = neutral[['text', 'hand_code']]
neutral = neutral[neutral['hand_code']==0]
neutral = neutral.rename(columns={'hand_code': 'labels'})

# combine the new data
tweets = pd.concat([neg, neutral])
tweets['labels'] = pd.to_numeric(tweets['labels'])

# drop incomplete data
tweets = tweets[tweets['labels'].isin([0,1])]

# drop duplicates
tweets = tweets.drop_duplicates(subset=['text'])

tweets = pd.concat([old, tweets])

# -----------------
# deal with weird chatacters and whatnot
# -----------------
# pull from DB



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


# build / train

# weights 
counts = train['labels'].value_counts().sort_index()
weights = [(1-(ii/len(train)))*10 for ii in counts] 


model_args = ClassificationArgs()
# model_args.use_early_stopping = True
# model_args.early_stopping_delta = 0.01
# model_args.early_stopping_metric = "mcc"
# model_args.early_stopping_metric_minimize = False
# model_args.early_stopping_patience = 5
# model_args.evaluate_during_training_verbose = True
# model_args.evaluate_during_training_steps = 1000
model_args.output_dir = r'Model_berttweet/'
model_args.cache_dir = r'Model_berttweet/'
model_args.overwrite_output_dir = True
model_args.training_batch_size = 728
model_args.eval_batch_size = 728
model_args.num_train_epochs = 5


model = ClassificationModel(
            'bertweet'
            , 'vinai/bertweet-base'
            , num_labels=len(tweets['labels'].unique())
            # , weight=weights # DO help
            , weight=[.8,11]
            , use_cuda=True 
            , args=model_args
      ) 


model.train_model(train)


# Evaluate the model
model = ClassificationModel('bertweet'
                        , r'D:/Dropbox/wandering-pole/wanderingpole/model_berttweet/'
                        , num_labels=2
                        , args={'eval_batch_size':512})

result, model_outputs, wrong_predictions = model.eval_model(test)

y_t = list(test.labels)
y_hat = [np.argmax(a) for a in model_outputs]
print(sklearn.metrics.classification_report(y_true=y_t, y_pred=y_hat))
sklearn.metrics.confusion_matrix(y_true=y_t, y_pred=y_hat)

# put out the results 
test.loc[:, 'predicted'] = y_hat


# # --------------
# # try balanced
# # --------------
# # split the training sample
# train_neg = train[train['labels']==1]
# train_pos = train[train['labels']==0]

# # oversample the uncivil tweets
# train_neg_expanded = train_neg.sample(n=len(train_pos), replace=True)

# # recombine + shuffle
# train_balanced = pd.concat([train_pos, train_neg_expanded])
# train_balanced = train_balanced.sample(frac=1).reset_index(drop=True)


# model_args = ClassificationArgs()
# # model_args.use_early_stopping = True
# # model_args.early_stopping_delta = 0.01
# # model_args.early_stopping_metric = "mcc"
# # model_args.early_stopping_metric_minimize = False
# # model_args.early_stopping_patience = 5
# # model_args.evaluate_during_training_verbose = True
# # model_args.evaluate_during_training_steps = 1000
# model_args.output_dir = r'Model_berttweet_balanced/'
# model_args.cache_dir = r'Model_berttweet_balanced/'
# model_args.overwrite_output_dir = True
# model_args.training_batch_size = 728
# model_args.eval_batch_size = 728
# model_args.num_train_epochs = 5


# model = ClassificationModel(
#             'bertweet'
#             , 'vinai/bertweet-base'
#             , num_labels=len(tweets['labels'].unique())
#             , use_cuda=True 
#             , args=model_args
#       ) 

# model.train_model(train_balanced)


# # Evaluate the model
# model = ClassificationModel('bertweet'
#                         , 'Model_berttweet/'
#                         , num_labels=2
#                         , args={'eval_batch_size':512})

# result, model_outputs, wrong_predictions = model.eval_model(test)

# y_t = list(test.labels)
# y_hat = [np.argmax(a) for a in model_outputs]
# print(sklearn.metrics.classification_report(y_true=y_t, y_pred=y_hat))
# sklearn.metrics.confusion_matrix(y_true=y_t, y_pred=y_hat)

# # put out the results 
# test.loc[:, 'predicted'] = y_hat

# test.to_csv('DELETE_test.csv')