#####################################################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls())
library(tidyverse)
library(magrittr)
library(abmisc)

# Load data with covariates merged
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData_polarizing.rds')



#####################################################################################################################
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


#####################################################################################################################
### Compute tweets per month

# Pull month
tw$month <- format(tw$date, '%m') %>% num()

# Make YearMonth
tw$YearMonth <- paste0(tw$year, '/', tw$month)

# Get number per month (by party)
monthlyTweets <- tw %>%
  group_by(year, month, party) %>%
  dplyr::summarise(
    YearMonth=unique(YearMonth),
    Tweets=n(),
    Polarizing=table(polarizing)['1'],
    `Percent Polarizing`=Polarizing/Tweets
  )

# Overall per month
monthlyOverall <- tw %>%
  group_by(year, month) %>%
  dplyr::summarise(
    party='All',
    YearMonth=unique(YearMonth),
    Tweets=n(),
    Polarizing=table(polarizing)['1'],
    `Percent Polarizing`=Polarizing/Tweets
  )

# Join
monthlyTweets <- rbind(monthlyTweets, monthlyOverall)

# Save
write.csv(monthlyTweets, '~/Dropbox/Projects/Twitter/Twitter/monthlyPolarizingTweets.csv', row.names=FALSE)
