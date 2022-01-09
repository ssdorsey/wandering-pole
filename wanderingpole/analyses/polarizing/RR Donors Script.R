rm(list=ls())
library(ggplot2)
library(dplyr)
library(lubridate)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)

# The donor analysis requires the merging of three sources - tweets, Member information, and donor information.

# First, import FEC individual donor information. Extra donation variables have been trimmed to reduce file size.

donors10 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/data/contributions/contributions_2009_2010.csv")
donors12 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/data/contributions/contributions_2011_2012.csv")
donors14 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/data/contributions/contributions_2013_2014.csv")
donors16 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/data/contributions/contributions_2015_2016.csv")
donors18 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/data/contributions/contributions_2017_2018.csv")
donors20 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/data/contributions/contributions_2019_2020.csv")

donors10 <- rename(donors10, FECcommittee = X)
donors12 <- rename(donors12, FECcommittee = X)
donors14 <- rename(donors14, FECcommittee = X)
donors16 <- rename(donors16, FECcommittee = X)
donors18 <- rename(donors18, FECcommittee = X)
donors20 <- rename(donors20, FECcommittee = X)

# import Congress specific member files (so only incumbent members are included)

c111 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 111th.csv")
c112 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 112th.csv")
c113 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 113th.csv")
c114 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 114th.csv")
c115 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 115th.csv")
c116 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 116th.csv")

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

tweets <- fread("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

handles <- read.csv("~/Dropbox/Projects/Twitter/Twitter/US Congress Handles Master List.csv")

handles <- dplyr::select(handles, icpsr, twitter_lower)

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

# Remove NAs
congresstweets <- congresstweets %>%
  filter(!is.na(week))

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

# Push NAs to 0
tweetsmonth$polarizing[is.na(tweetsmonth$polarizing)] <- 0
tweetsweek$polarizing[is.na(tweetsweek$polarizing)] <- 0

#make percentages

tweetsmonth$pct.polarizing <- (tweetsmonth$polarizing/tweetsmonth$tweets) * 100
tweetsweek$pct.polarizing <- (tweetsweek$polarizing/tweetsweek$tweets) * 100

#create unique member-date identifiers

tweetsmonth$icpsryearmonth <- paste0(as.character(tweetsmonth$icpsr),"M", as.character(tweetsmonth$yearmonth))
tweetsweek$icpsryearweek <- paste0(as.character(tweetsweek$icpsr),"M", as.character(tweetsweek$yearweek))

donationsmonth$icpsryearmonth <- paste0(as.character(donationsmonth$icpsr),"M", as.character(donationsmonth$yearmonth))
donationsweek$icpsryearweek <- paste0(as.character(donationsweek$icpsr),"M", as.character(donationsweek$yearweek))

# Filter down tweetsmonth and tweetsweek
tweetsmonth <- tweetsmonth %>%
  dplyr::select(-c(icpsr, yearmonth))
tweetsweek <- tweetsweek %>%
  dplyr::select(-c(icpsr, yearweek))

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
summary(monthamountpp <- felm(monthamount ~ pct.polarizing | icpsr + yearmonth, data=dandtmonth))
summary(monthamountcount <- felm(monthamount ~ polarizing  | icpsr + yearmonth, data=dandtmonth))
summary(monthamounttweets <- felm(monthamount ~ tweets | icpsr + yearmonth, data=dandtmonth))
summary(monthamountsq <- felm(monthamount ~ polarizing_sq | icpsr + yearmonth, data=dandtmonth))

summary(monthdonorspp <- felm(monthdonors ~ pct.polarizing | icpsr + yearmonth, data=dandtmonth))
summary(monthdonorscount <- felm(monthdonors ~ polarizing  | icpsr + yearmonth, data=dandtmonth))
summary(monthdonorstweets <- felm(monthdonors ~ tweets | icpsr + yearmonth, data=dandtmonth))
summary(monthdonorssq <- felm(monthdonors ~ polarizing_sq | icpsr + yearmonth, data=dandtmonth))

stargazer(monthamountpp, monthamountcount, monthamounttweets, monthamountsq, monthdonorspp, monthdonorscount, monthdonorstweets, monthdonorssq,
          Title = "Monthly Member Twitter Activity and Fundraising Trends", type="html",  out="Monthly Twitter Donations.htm")

######week
summary(weekamountpp <- felm(weekamount ~ pct.polarizing | icpsr + yearweek, data=dandtweek))
summary(weekamountcount <- felm(weekamount ~ polarizing  | icpsr + yearweek, data=dandtweek))
summary(weekamounttweets <- felm(weekamount ~ tweets | icpsr + yearweek, data=dandtweek))
summary(weekamountsq <- felm(weekamount ~ polarizing_sq | icpsr + yearweek, data=dandtweek))

summary(weekdonorspp <- felm(weekdonors ~ pct.polarizing | icpsr + yearweek, data=dandtweek))
summary(weekdonorscount <- felm(weekdonors ~ polarizing  | icpsr + yearweek, data=dandtweek))
summary(weekdonorstweets <- felm(weekdonors ~ tweets | icpsr + yearweek, data=dandtweek))
summary(weekdonorssq <- felm(weekdonors ~ polarizing_sq | icpsr + yearweek, data=dandtweek))

stargazer(weekamountpp, weekamountcount, weekamounttweets, weekamountsq, weekdonorspp, weekdonorscount, weekdonorstweets, weekdonorssq,
          Title = "Weekly Member Twitter Activity and Fundraising Trends", type="html",  out="Weekly Twitter Donations.htm")


######
######
##############extract coefficients and plot

# Make into lists so we can get coefs and SEs
weekamount <- list(weekamountcount, weekamounttweets, weekamountpp, weekamountsq)
weekdonors <- list(weekdonorscount, weekdonorstweets, weekdonorspp, weekdonorssq)
monthamount <- list(monthamountcount, monthamounttweets, monthamountpp, monthamountsq)
monthdonors <- list(monthdonorscount, monthdonorstweets, monthdonorspp, monthdonorssq)


# Extract coefs and SEs
effPlotDat <- function(model){
  # Pull coefficient
  coefs <- summary(model)$coefficients
  coef <- coefs[,'Estimate']
  # Pull CI
  ci <- coefs[,'Std. Error']*1.96
  # Combine/return
  effs <- c(coef=coef, ci=ci)
  effs
}
effs_week <- lapply(list(weekamount, weekdonors), function(x){
  sapply(x, effPlotDat) %>% t()
})
effs_month <- lapply(list(monthamount, monthdonors), function(x){
  sapply(x, effPlotDat) %>% t()
})

# Plot weekly effects
barCenters_amt <- barplot(effs_week[[1]][,'coef'], ylim=c(0, 400))
barCenters_don <- barplot(effs_week[[2]][,'coef'], ylim=c(0, 3.5))
jpeg('~/Dropbox/Projects/Twitter/polarizing_weekly_effects.jpg', width=16, height=8, units='in', res=300)
par(mar=c(3.1, 4.1, 2.1, 1.1), mfrow=c(1,2))
barplot(effs_week[[1]][,'coef'], ylim=c(0, 400), ylab='Weekly Fundraising Amount Increase', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters_amt[ii], y0=effs_week[[1]][ii,'coef']-effs_week[[1]][ii,'ci'], y1=effs_week[[1]][ii,'coef']+effs_week[[1]][ii,'ci'], lwd=3)
  arrows(x0=barCenters_amt[ii], y0=effs_week[[1]][ii,'coef']-effs_week[[1]][ii,'ci'], y1=effs_week[[1]][ii,'coef']+effs_week[[1]][ii,'ci'], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters_amt[ii], y=effs_week[[1]][ii,'coef']/4, paste0(round(effs_week[[1]][ii,'coef'], 2), ' +/- ', round(effs_week[[1]][ii,'ci'], 2)))
}

barplot(effs_week[[2]][,'coef'], ylim=c(0, 3.5), ylab='Weekly Donors Increase', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters_don[ii], y0=effs_week[[2]][ii,'coef']-effs_week[[2]][ii,'ci'], y1=effs_week[[2]][ii,'coef']+effs_week[[2]][ii,'ci'], lwd=3)
  arrows(x0=barCenters_don[ii], y0=effs_week[[2]][ii,'coef']-effs_week[[2]][ii,'ci'], y1=effs_week[[2]][ii,'coef']+effs_week[[2]][ii,'ci'], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters_don[ii], y=effs_week[[2]][ii,'coef']/4, paste0(round(effs_week[[2]][ii,'coef'], 2), ' +/- ', round(effs_week[[2]][ii,'ci'], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))
dev.off()

# Plot monthly effects
barCenters_amt <- barplot(effs_month[[1]][,'coef'], ylim=c(0, 700))
barCenters_don <- barplot(effs_month[[2]][,'coef'], ylim=c(0, 5.5))
jpeg('~/Dropbox/Projects/Twitter/polarizing_monthly_effects.jpg', width=16, height=8, units='in', res=300)
par(mar=c(3.1, 4.1, 2.1, 1.1), mfrow=c(1,2))
barplot(effs_month[[1]][,'coef'], ylim=c(0, 700), ylab='Monthly Fundraising Amount Increase', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters_amt[ii], y0=effs_month[[1]][ii,'coef']-effs_month[[1]][ii,'ci'], y1=effs_month[[1]][ii,'coef']+effs_month[[1]][ii,'ci'], lwd=3)
  arrows(x0=barCenters_amt[ii], y0=effs_month[[1]][ii,'coef']-effs_month[[1]][ii,'ci'], y1=effs_month[[1]][ii,'coef']+effs_month[[1]][ii,'ci'], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters_amt[ii], y=effs_month[[1]][ii,'coef']/4, paste0(round(effs_month[[1]][ii,'coef'], 2), ' +/- ', round(effs_month[[1]][ii,'ci'], 2)))
}

barplot(effs_month[[2]][,'coef'], ylim=c(0, 5.5), ylab='Monthly Donors Increase', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters_don[ii], y0=effs_month[[2]][ii,'coef']-effs_month[[2]][ii,'ci'], y1=effs_month[[2]][ii,'coef']+effs_month[[2]][ii,'ci'], lwd=3)
  arrows(x0=barCenters_don[ii], y0=effs_month[[2]][ii,'coef']-effs_month[[2]][ii,'ci'], y1=effs_month[[2]][ii,'coef']+effs_month[[2]][ii,'ci'], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters_don[ii], y=effs_month[[2]][ii,'coef']/4, paste0(round(effs_month[[2]][ii,'coef'], 2), ' +/- ', round(effs_month[[2]][ii,'ci'], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1))
dev.off()