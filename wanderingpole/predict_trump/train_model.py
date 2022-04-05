import pandas as pd
import numpy as np
import json
import sklearn
import torch
import re
import os
import json
import emoji
from simpletransformers.classification import ClassificationModel

np.random.seed(1845)


### Setup

# Import the data (split into two parts)
tweets1 = pd.read_json('E:/Dropbox/Projects/Twitter/trump_model_tweets_1.json')
tweets2 = pd.read_json('E:/Dropbox/Projects/Twitter/trump_model_tweets_2.json')

# Join the tweets, clean up
tweets = pd.concat([tweets1, tweets2])
del (tweets1, tweets2)

# Restrict to tweets with label codes
tweets = tweets[tweets['trump'].isin([0, 1])]

# Subset to just text and labels, fix columns names
tweets = tweets.loc[:, ['text', 'trump']]
tweets.columns = ['text', 'labels']

# Split train/test
tweets.loc[:, 'split'] = np.random.choice([0, 1], len(tweets), p=[.8, .2])
train = tweets.loc[tweets.split == 0, ['text', 'labels']]
test = tweets.loc[tweets.split == 1, ['text', 'labels']]


### Train model

# Weights
counts = train['labels'].value_counts().sort_index()
weights = [1-(ii/len(train)) for ii in counts]

#Initialize model
model = ClassificationModel(
    'bertweet'
    , 'vinai/bertweet-base'
    , num_labels=len(tweets['labels'].unique())
    , weight=weights
    , use_cuda=True
    , args={'reprocess_input_data': True
        , 'overwrite_output_dir': True
        , 'training_batch_size': 2048
        , 'eval_batch_size': 2048
        , 'num_train_epochs': 10
        , 'output_dir': r'TrumpModel/'
        , 'cache_dir': r'TrumpModel/'}
)

# Train
model.train_model(train)

# Results
result, model_outputs, wrong_predictions = model.eval_model(test)
y_t = list(test.labels)
y_hat = [np.argmax(a) for a in model_outputs]
print(sklearn.metrics.classification_report(y_true=y_t, y_pred=y_hat))
test.loc[:, 'predicted'] = y_hat
