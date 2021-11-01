library(ggplot2)
library(dplyr)
library(lubridate)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)

# The donor analsysis requires the merging of three sources - tweets, Member information, and donor information.

# First, import FEC individual donor information. Extra donation variables have been trimmed to reduce file size.

donors10 <- read.csv("C:/Users/User/Dropbox/Twitter/data/contributions/contributions_2009_2010.csv")
donors12 <- read.csv("C:/Users/User/Dropbox/Twitter/data/contributions/contributions_2011_2012.csv")
donors14 <- read.csv("C:/Users/User/Dropbox/Twitter/data/contributions/contributions_2013_2014.csv")
donors16 <- read.csv("C:/Users/User/Dropbox/Twitter/data/contributions/contributions_2015_2016.csv")
donors18 <- read.csv("C:/Users/User/Dropbox/Twitter/data/contributions/contributions_2017_2018.csv")
donors20 <- read.csv("C:/Users/User/Dropbox/Twitter/data/contributions/contributions_2019_2020.csv")

donors10 <- rename(donors10, FECcommittee = X)
donors12 <- rename(donors12, FECcommittee = X)
donors14 <- rename(donors14, FECcommittee = X)
donors16 <- rename(donors16, FECcommittee = X)
donors18 <- rename(donors18, FECcommittee = X)
donors20 <- rename(donors20, FECcommittee = X)

# import Congress specific member files (so only incumbent members are included)

c111 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 111th.csv")
c112 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 112th.csv")
c113 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 113th.csv")
c114 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 114th.csv")
c115 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 115th.csv")
c116 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 116th.csv")

# merge with donations to remove non-incumbent donations

donors10 <- merge(donors10, c111, by = "FECcommittee")
donors12 <- merge(donors12, c112, by = "FECcommittee")
donors14 <- merge(donors14, c113, by = "FECcommittee")
donors16 <- merge(donors16, c114, by = "FECcommittee")
donors18 <- merge(donors18, c115, by = "FECcommittee")
donors20 <- merge(donors20, c116, by = "FECcommittee")

# combine to single file

donorscomplete <- rbind(donors10, donors12, donors14, donors16, donors18, donors20)

# Proper date format

donorscomplete$date <- ymd(donorscomplete$X.1)

# add column for year

donorscomplete$year <- year(donorscomplete$date)

# add column for week

donorscomplete$week <- lubridate::week(donorscomplete$date)

# add column for month

donorscomplete$month <- lubridate::month(donorscomplete$date)

#creating date identifier variable
donorscomplete$yearmonth <- paste0(as.character(donorscomplete$year),"Y", as.character(donorscomplete$month))
donorscomplete$yearweek <- paste0(as.character(donorscomplete$year),"Y", as.character(donorscomplete$week))

#grouping donations by week and month

donationsmonth <- donorscomplete %>%
  group_by(icpsr, yearmonth) %>% 
  dplyr::summarize(
    monthamount = sum(amount),
    monthdonors = sum(donors)
  ) %>% 
  ungroup()

donationsweek <- donorscomplete %>%
  group_by(icpsr, yearweek) %>% 
  dplyr::summarize(
    weekamount = sum(amount),
    weekdonors = sum(donors)
  ) %>% 
  ungroup()

#writing csv for easy re-loading

write.csv(donationsmonth, file = "donationsmonth.csv")
write.csv(donationsweek, file = "donationsweek.csv")



######
#########tweet counts merge

tweets <- fread("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

handles <- read.csv("C:/Users/User/Dropbox/Twitter/US Congress Handles Master List.csv")

handles <- select(handles, icpsr, twitter_lower)

congresstweets <- right_join(tweets, handles, by = "twitter_lower")

# add column for year

congresstweets$year <- year(congresstweets$created_at)

# add column for week

congresstweets$week <- lubridate::week(congresstweets$created_at)

# add column for month

congresstweets$month <- lubridate::month(congresstweets$created_at)

#creating date identifier variable
congresstweets$yearmonth <- paste0(as.character(congresstweets$year),"Y", as.character(congresstweets$month))
congresstweets$yearweek <- paste0(as.character(congresstweets$year),"Y", as.character(congresstweets$week))

#group tweets by date and icpsr
tweetsmonth <- congresstweets %>%
  group_by(icpsr, yearmonth) %>% 
  dplyr::summarize(
    tweets = n(),
    polarizing = sum(polarizing)
  ) %>% 
  ungroup()

tweetsweek <- congresstweets %>%
  group_by(icpsr, yearweek) %>% 
  dplyr::summarize(
    tweets = n(),
    polarizing = sum(polarizing)
  ) %>% 
  ungroup()

#make percentages

tweetsmonth$pct.polarizing <- ((tweetsmonth$polarizing/tweetsmonth$tweets) *100)
tweetsweek$pct.polarizing <- ((tweetsweek$polarizing/tweetsweek$tweets) *100)

#create unique member-date identifiers

tweetsmonth$icpsryearmonth <- paste0(as.character(tweetsmonth$icpsr),"M", as.character(tweetsmonth$yearmonth))
tweetsweek$icpsryearweek <- paste0(as.character(tweetsweek$icpsr),"M", as.character(tweetsweek$yearweek))

donationsmonth$icpsryearmonth <- paste0(as.character(donationsmonth$icpsr),"M", as.character(donationsmonth$yearmonth))
donationsweek$icpsryearweek <- paste0(as.character(donationsweek$icpsr),"M", as.character(donationsweek$yearweek))

#merge tweets and donors

dandtmonth <- left_join(donationsmonth, tweetsmonth, by = "icpsryearmonth")
dandtweek <- left_join(donationsweek, tweetsweek, by = "icpsryearweek")

dandtmonth$tweets[is.na(dandtmonth$tweets)] <- 0
dandtmonth$polarizing[is.na(dandtmonth$polarizing)] <- 0

dandtweek$tweets[is.na(dandtweek$tweets)] <- 0
dandtweek$polarizing[is.na(dandtweek$polarizing)] <- 0

######
######
######
######
######
##############analysis

dandtmonth$polarizing_sq <- ((dandtmonth$polarizing * dandtmonth$polarizing) / dandtmonth$tweets)
dandtweek$polarizing_sq <- ((dandtweek$polarizing * dandtweek$polarizing) / dandtweek$tweets)

######month
summary(monthamountpp <- felm(monthamount ~ pct.polarizing | icpsr.x + yearmonth.x, data=dandtmonth))
summary(monthamountcount <- felm(monthamount ~ polarizing  | icpsr.x + yearmonth.x, data=dandtmonth))
summary(monthamounttweets <- felm(monthamount ~ tweets | icpsr.x + yearmonth.x, data=dandtmonth))
summary(monthamountsq <- felm(monthamount ~ polarizing_sq | icpsr.x + yearmonth.x, data=dandtmonth))

summary(monthdonorspp <- felm(monthdonors ~ pct.polarizing | icpsr.x + yearmonth.x, data=dandtmonth))
summary(monthdonorscount <- felm(monthdonors ~ polarizing  | icpsr.x + yearmonth.x, data=dandtmonth))
summary(monthdonorstweets <- felm(monthdonors ~ tweets | icpsr.x + yearmonth.x, data=dandtmonth))
summary(monthdonorssq <- felm(monthdonors ~ polarizing_sq | icpsr.x + yearmonth.x, data=dandtmonth))

stargazer(monthamountpp, monthamountcount, monthamounttweets, monthamountsq, monthdonorspp, monthdonorscount, monthdonorstweets, monthdonorssq,
          Title = "Monthly Member Twitter Activity and Fundraising Trends", type="html",  out="Monthly Twitter Donations.htm")

######week
summary(weekamountpp <- felm(weekamount ~ pct.polarizing | icpsr.x + yearweek.x, data=dandtweek))
summary(weekamountcount <- felm(weekamount ~ polarizing  | icpsr.x + yearweek.x, data=dandtweek))
summary(weekamounttweets <- felm(weekamount ~ tweets | icpsr.x + yearweek.x, data=dandtweek))
summary(weekamountsq <- felm(weekamount ~ polarizing_sq | icpsr.x + yearweek.x, data=dandtweek))

summary(weekdonorspp <- felm(weekdonors ~ pct.polarizing | icpsr.x + yearweek.x, data=dandtweek))
summary(weekdonorscount <- felm(weekdonors ~ polarizing  | icpsr.x + yearweek.x, data=dandtweek))
summary(weekdonorstweets <- felm(weekdonors ~ tweets | icpsr.x + yearweek.x, data=dandtweek))
summary(weekdonorssq <- felm(weekdonors ~ polarizing_sq | icpsr.x + yearweek.x, data=dandtweek))

stargazer(weekamountpp, weekamountcount, weekamounttweets, weekamountsq, weekdonorspp, weekdonorscount, weekdonorstweets, weekdonorssq,
          Title = "Weekly Member Twitter Activity and Fundraising Trends", type="html",  out="Weekly Twitter Donations.htm")


######
######
##############extract coefficients and plot

