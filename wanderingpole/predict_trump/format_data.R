####################################################################################################################
### Setup

### Packages, working directory, scientific notation option

rm(list=ls());gc()
library(data.table)
library(tidyverse)
library(magrittr)
library(abmisc)
library(lubridate)
library(rjson)
library(sjPlot)
library(speedglm)
library(prediction)
library(coefplot)
setwd('~/Dropbox/Projects/Twitter')
options(scipen=99999)



#####################################################################################################################
### Load, clean tweets for merge

# Load Trump tweets
trump <- jsonlite::fromJSON(txt=paste0(getwd(), '/Twitter/Data/trump_tweets.json')) %>% 
  as.data.frame()

# Load MC tweets
mcs <- fread(paste0(getwd(), '/tweets_newapi.csv'), stringsAsFactors = FALSE, data.table = FALSE)

# Match column names (id, screen_name, date, text, favorites, retweets)
trump %<>%
  mutate(screen_name='realDonaldTrump') %>%
  dplyr::select(id, screen_name, date, text, favorites, retweets)
mcs %<>%
  dplyr::rename(screen_name=author_username,
                date=created_at,
                retweets=public_metrics.retweet_count,
                favorites=public_metrics.like_count
                ) %>%
  dplyr::select(id, screen_name, date, text, favorites, retweets)

# Format dates
trump %<>% mutate(date=as.Date(date))
mcs %<>% mutate(date=as.Date(date))

# Indicator for whether MC tweets are in the sample (for the balanced model)
trump %<>% mutate(sample=1)
twsamp <- sample(1:nrow(mcs), nrow(trump))
mcs$sample <- 0
mcs$sample[twsamp] <- 1

# Join
tweets <- rbind(trump, mcs)

# Indicator for whether it's a Trump tweet (DV for transformers)
tweets %<>% mutate(trump=ifelse(screen_name=='realDonaldTrump', 1, 0))

# Restrict to dates between Trump's announcement and when he left office
tweets %<>% filter(date %within% interval(start='2015-06-15', end='2021-01-08'))

# Make ID a character string
tweets %<>% mutate(id=char(id))

# Save
jsonlite::write_json(tweets[1:2000000,], paste0(getwd(), '/trump_model_tweets_1.json')) # It takes like 10x longer to not split this in two, no idea why
jsonlite::write_json(tweets[2000001:nrow(tweets),], paste0(getwd(), '/trump_model_tweets_2.json'))
