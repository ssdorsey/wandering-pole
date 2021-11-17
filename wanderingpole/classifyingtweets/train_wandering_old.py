import pandas as pd
import numpy as np
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

np.random.seed(2)

# import the data
# tweets = pd.read_csv('data/postIR_final.csv')
# os.chdir('..')
tweets = pd.read_csv('D:/Dropbox/Twitter/training_data/training_final.csv', encoding='latin1')

# restrict to tweets with coding
tweets = tweets[tweets['uncivil_final'].isin([0,1])]

# subset to just text and labels, fix columns names
tweets = tweets.loc[:, ['text', 'uncivil_final']]
tweets.columns = ['text', 'labels']

# import other batch
mike = pd.read_excel(r'D:\Dropbox\wandering-pole\wanderingpole\data\new_pull_Michael.xls')
mike = mike[['full_text', 'uncivil']]
mike = mike.rename(columns={'full_text': 'text', 'uncivil': 'labels'})

# extra
mike_extra = pd.read_csv(r'D:\Dropbox\wandering-pole\wanderingpole\data\michael_extra.csv')
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
model_args.training_batch_size = 1024
model_args.eval_batch_size = 1024
model_args.num_train_epochs = 5


model = ClassificationModel(
            'bertweet'
            , 'vinai/bertweet-base'
            , num_labels=len(tweets['labels'].unique())
            # , weight=weights # DO help
            , weight=[.8,10]
            , use_cuda=True 
            , args=model_args
      ) 


model.train_model(train)


# Evaluate the model
# model = ClassificationModel('bertweet'
#                         , 'Model_berttweet/'
#                         , num_labels=2
#                         , args={'eval_batch_size':512})

result, model_outputs, wrong_predictions = model.eval_model(test)

y_t = list(test.labels)
y_hat = [np.argmax(a) for a in model_outputs]
print(sklearn.metrics.classification_report(y_true=y_t, y_pred=y_hat))
sklearn.metrics.confusion_matrix(y_true=y_t, y_pred=y_hat)

# put out the results 
test.loc[:, 'predicted'] = y_hat
