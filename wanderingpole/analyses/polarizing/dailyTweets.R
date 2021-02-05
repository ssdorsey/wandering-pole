##############################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls())
library(tidyverse)
library(magrittr)
library(abmisc)

# Load data with covariates merged
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData.rds')



##############################################################################################
### Compute tweets per day

# Compute
dailyTweets <- tw %>%
  group_by(date, icpsr) %>%
  dplyr::summarise(
    tweets=n(),
    uncivil=table(uncivil)['1']
  )
dailyTweets$uncivil[is.na(dailyTweets$uncivil)] <- 0
dailyTweets %<>% mutate(pct.uncivil=uncivil/tweets)

# Save
write.csv(dailyTweets, '~/Dropbox/Projects/Twitter/Twitter/dailyTweets.csv', row.names=FALSE)
