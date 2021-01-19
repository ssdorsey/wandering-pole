#######################################################################################################################################################################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls())
library(tidyverse)
library(magrittr)
library(abmisc)
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load data with covariates merged
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData.rds')



#######################################################################################################################################################################################################################################
### Compute tweets per day

# Compute
dailyTweets <- tw %>%
  group_by(date, icpsr) %>%
  dplyr::summarise(
    tweets=n(),
    uncivil=table(uncivil)['1']
  )
dailyTweets$uncivil[is.na(dailyTweets$uncivil)] <- 0

# Save
write.csv(dailyTweets, 'Twitter/dailyTweets.csv', row.names=FALSE)
