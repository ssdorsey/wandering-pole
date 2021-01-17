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

dailyTweets <- tw %>%
  group_by(date, icpsr) %>%
  dplyr::summarise(
    tweets=n(),
    uncivil=n()[uncivil==1]
  )
dailyTweets$uncivil[is.na(dailyTweets$uncivil)] <- 0

write.csv(dailyTweets, 'dailyTweets.csv', row.names=FALSE)
