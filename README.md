# WanderingPole

<img src="https://upload.wikimedia.org/wikipedia/commons/7/74/Magnetic_North_Pole_Positions_2015.svg" width="200"> 

This is the base code for *Dynamics of Polarizing Rhetoric in Congressional Tweets* 

## Setup
1. install pytorch (CUDA recommended if GPU available)
2. clone this git and install requirements
3. install mongoDB, set up a database named `wanderingpole` and a collection named `tweets`

## Collecting Tweets

Using the new academic license for Twitter, it is possible to collect a full collection of tweets for each user. This is done using collect.py in the collectingtweets directory.

## Training Models
Relies on the terrific HuggingFace versions of RoBERTa/BERTweet. Run with train_classifier.py.

The latest (pretrained) model we use for classifying polarization is available on huggingface

## Classifying Tweets
After training a model (or downloading the pre-trained model), classification can be run on the tweets in the mongoDB collection using the files in the classify_tweets directory

## Analysis
Various scripts available in the analyses directory.