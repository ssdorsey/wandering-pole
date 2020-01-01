# WanderingPole

<img src="https://upload.wikimedia.org/wikipedia/commons/7/74/Magnetic_North_Pole_Positions_2015.svg" width="300"> 

## Setup
1. install pytorch (CUDA recommended if GPU available)
2. clone this git and install requirements

Note: WanderingPole currently saves/loads all tweets as flat .json files but could be easily converted to using SQL/MongoDB for increased efficiency as the collection grows

## Collecting Tweets

Collection should be run at least once a week to avoid gaps in the data. 

Relies on a .csv file with a column labelled "twitter" that contains the twitter handles to collect.

The Twitter API is limited to 3,200 most recent tweets per user. You must insert your own API credentials in collect_api.py.

## Training Models
Relies on the terrific HuggingFace pytorch-port of RoBERTa. Run with train_classifier.py.

The latest (pretrained) model we use for classifying polarization is available HERE

## Classifying Tweets
After training a model (or downloading the pre-trained model), classification on a collection of .json files can be run with 

## Analysis
