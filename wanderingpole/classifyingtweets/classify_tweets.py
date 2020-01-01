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

def process_json(filename):
    """reads and classifies tweets from a json"""
    # load the tweets
    with open(f'data/'+filename) as f:
        prev_json = json.load(f)
    # fix the weird thing with AGBecerra
    handle = filename.split('_tweets.json')[0]
    filtered = []
    for ii in prev_json:
        if ii['screen_name'] == handle:
            filtered.append(ii)
        elif ii['retweeted']:
            filtered.append(ii)
    prev_json = filtered
    # pull the text
    text = [dd['text'] for dd in prev_json]
    if len(text) > 0:
        # classify them
        results, model_outputs = model.predict(text)
        # write back out
        for ii in range(len(prev_json)):
            prev_json[ii]['polarizing'] = int(results[ii])
    with open(f'data/'+filename, 'w') as f:
        json.dump(prev_json, f)


if __name__ == "__main__":
    # load the model
    model = ClassificationModel('roberta'
                            , 'ModelOutput/'
                            , num_labels=2
                            , args={'eval_batch_size':512})

    # load the tweet filenames
    jsons = [ii for ii in os.listdir('data/') if ii.endswith('.json')]

    # classify the tweets
    for ff in tqdm(jsons):
        print('STARTING ' + ff)
        process_json(ff)

