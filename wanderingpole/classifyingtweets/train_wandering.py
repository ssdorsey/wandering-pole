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

np.random.seed(619)

# import the data
tweets = pd.read_csv('../data/postIR_final.csv')
# restrict to tweets with coding
tweets = tweets[tweets['divisive'].isin([0,1])]

# fix columns names
tweets = tweets.loc[:, ['text', 'divisive']]
tweets.columns = ['text', 'labels']
# split train/test
tweets.loc[: , 'split'] = np.random.choice([0,1], len(tweets), p=[.8, .2])
train = tweets.loc[tweets.split==0 ,['text', 'labels']]
test = tweets.loc[tweets.split==1 ,['text', 'labels']]

# build / train

# weights 
# counts = train['labels'].value_counts().sort_index()
# weights = [1-(ii/len(train)) for ii in counts] 

model = ClassificationModel(
            'roberta'
            , 'roberta-base'
            , num_labels=len(tweets['labels'].unique())
            # , weight=weights # Don't help...
            , use_cuda=True 
            , args={'reprocess_input_data': True
                  , 'overwrite_output_dir': True
                  , 'training_batch_size': 256
                  , 'eval_batch_size': 1024
                  , 'num_train_epochs': 10
                  , 'output_dir': r'ModelOutput/'
                  , 'cache_dir': r'ModelOutput/'}) 

model.train_model(train)

# Evaluate the model
# model = ClassificationModel('roberta'
#                         , 'ModelOutput/'
#                         , num_labels=2
#                         , args={'eval_batch_size':512})

result, model_outputs, wrong_predictions = model.eval_model(test)

y_t = list(test.labels)
y_hat = [np.argmax(a) for a in model_outputs]
print(sklearn.metrics.classification_report(y_true=y_t, y_pred=y_hat))

# put out the results 
test.loc[:, 'predicted'] = y_hat