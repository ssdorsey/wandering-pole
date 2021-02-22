library(ggplot2)
library(dplyr)
library(lubridate)

library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(Hmisc)

#import follower data

followers <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Followers Remerge Files/congressional_twitter_follower_changes.csv")

followers$year <- format(as.Date(followers$date, format="%m/%d/%Y"),"%Y")

followers$week <- lubridate::week(mdy(followers$date))

#import twitter handle data

Twitter <- read.csv("C:/Users/User/Dropbox/Twitter/master_handles w FEC and Official Tag.csv")
twittercodes <- dplyr::select(Twitter, twitter, icpsr)

#make handles lower to ensure match

twittercodes$handle_lower <- tolower(twittercodes$twitter)

#merge datasets

followersmerge <- merge(followers, twittercodes, by = "handle_lower")

#group by week and take the weekly max in twitter followers

followersgroup <- followersmerge %>%
  group_by(icpsr, year, week) %>% 
  dplyr::summarize(
    followers = max(twitter_followers)
  ) %>% 
  ungroup()

#create lagged variable for twitter followers

followersgroup$lagged_followers <- Lag(followersgroup$followers, +1)

#remove first row for each group

followersgroup <- followersgroup %>%
  group_by(icpsr) %>%
  slice(2:n())   %>% 
  ungroup()

#create follower difference variables

followersgroup$followerchange <- (followersgroup$followers - followersgroup$lagged_followers)

followersgroup$followerchange_pct <- ((followersgroup$followerchange / followersgroup$lagged_followers) * 100)

#create icpsr-week identifier

followersgroup$icpsryearweek <- paste0(as.character(followersgroup$icpsr),"M", as.character(followersgroup$year), "Y", as.character(followersgroup$week))

#import tweets (weekly grouped tweets created from donor analysis for quicker loading (can also load all tweets and repeat weekly grouping proceedure))

tweets <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/tweetsweek.csv")

#merge tweets and followers

followerstweets <- merge(followersgroup, tweets, by = "icpsryearweek")

#write final file

write.csv(followerstweets, file = "followerstweets.csv")



