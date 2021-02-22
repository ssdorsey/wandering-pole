library(ggplot2)
library(dplyr)
library(lubridate)

library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)

setwd('~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Donors Remerge Files/')

# The donor analsysis requires the merging of three sources - tweets, Member information, and donor information.

# First, import FEC individual donor information (below files have Member ID (ICPSR) information included based on FEC Committee ID. Extra donation variables have been trimmed to reduce file size.

donors10 <- read.csv("donors10trim.csv")
donors12 <- read.csv("donors12trim.csv")
donors14 <- read.csv("donors14trim.csv")
donors16 <- read.csv("donors16trim.csv")
donors18 <- read.csv("donors18trim.csv")

# Make columns consistent across files

donors10trim <- select(donors10, twitter, FECID, icpsr, first, last, fullname, state, 
                      party, Name, City, State, Date, Amount, newdate)
donors12trim <- select(donors12, twitter, FECID, icpsr, first, last, fullname, state, 
                       party, Name, City, State, Date, Amount, newdate)
donors14trim <- select(donors14, twitter, FECID, icpsr, first, last, fullname, state, 
                      party, Name, City, State, Date, Amount, newdate)
donors16trim <- select(donors16, twitter, FECID, icpsr, first, last, fullname, state, 
                       party, Name, City, State, Date, Amount, newdate)
donors18trim <- select(donors18, twitter, FECID, icpsr, first, last, fullname, state, 
                       party, Name, City, State, Date, Amount, newdate)

# Remove negative donations (duplicates and refunds)

donors10trim <- filter(donors10trim, Amount > 0)
donors12trim <- filter(donors12trim, Amount > 0)
donors14trim <- filter(donors14trim, Amount > 0)
donors16trim <- filter(donors16trim, Amount > 0)
donors18trim <- filter(donors18trim, Amount > 0)

# import Congress specific member files (so only incumbent members are included)

members10 <- read.csv("111thTwitterInfo.csv")
members12 <- read.csv("112thTwitterInfo.csv")
members14 <- read.csv("113thTwitterInfo.csv")
members16 <- read.csv("114thTwitterInfo.csv")
members18 <- read.csv("115thTwitterInfo.csv") 

# keep just one copy of each member (remove duplicate twitter handle entries)

members10group <- members10 %>%
  group_by(FECcommittee) %>% 
  dplyr::summarize(
    icpsr = max(icpsr)
  ) %>% 
  ungroup()

members12group <- members12 %>%
  group_by(FECcommittee) %>% 
  dplyr::summarize(
    icpsr = max(icpsr)
  ) %>% 
  ungroup()

members14group <- members14 %>%
  group_by(FECcommittee) %>% 
  dplyr::summarize(
    icpsr = max(icpsr)
  ) %>% 
  ungroup()

members16group <- members16 %>%
  group_by(FECcommittee) %>% 
  dplyr::summarize(
    icpsr = max(icpsr)
  ) %>% 
  ungroup()

members18group <- members18 %>%
  group_by(FECcommittee) %>% 
  dplyr::summarize(
    icpsr = max(icpsr)
  ) %>% 
  ungroup()

# Create combined dataframes of members for looping through later
members10group %<>% mutate(congress=111) 
members12group %<>% mutate(congress=112) 
members14group %<>% mutate(congress=113) 
members16group %<>% mutate(congress=114) 
members18group %<>% mutate(congress=115) 
members <- rbind(members10group, members12group, members14group, members16group, members18group)

# merge with donations to remove non-incumbent donations
donors10 <- merge(donors10trim, members10group, by = "icpsr")
donors12 <- merge(donors12trim, members12group, by = "icpsr")
donors14 <- merge(donors14trim, members14group, by = "icpsr")
donors16 <- merge(donors16trim, members16group, by = "icpsr")
donors18 <- merge(donors18trim, members18group, by = "icpsr")

# combine to single file
donorscomplete <- rbind(donors10, donors12, donors14, donors16, donors18)

# add column for year

donorscomplete$year <- format(as.Date(donorscomplete$newdate, format="%m/%d/%Y"),"%Y")

# add column for week

donorscomplete$week <- lubridate::week(mdy(donorscomplete$newdate))

# add column for month

donorscomplete$month <- lubridate::month(mdy(donorscomplete$newdate))

# remove incorrect dates (some donations have no date or incorrect year)

donorscomplete <- filter(donorscomplete, year > 2008)
donorscomplete <- filter(donorscomplete, year < 2020)

#writing csv for easy re-loading

saveRDS(donorscomplete, file = "donorscomplete08-20.rds")

#creating date identifier variable

donorscomplete$yearmonth <- paste0(as.character(donorscomplete$year),"-", as.character(donorscomplete$month))
donorscomplete$yearweek <- paste0(as.character(donorscomplete$year),"-", as.character(donorscomplete$week))

#grouping donations by week and month
donationsmonth <- donorscomplete %>%
  group_by(icpsr, yearmonth) %>% 
  dplyr::summarize(
    monthamount = sum(Amount),
    monthdonors = n()
  ) %>% 
  ungroup()

donationsweek <- donorscomplete %>%
  group_by(icpsr, yearweek) %>% 
  dplyr::summarize(
    yearmonth = unique(yearmonth),
    weekamount = sum(Amount),
    weekdonors = n()
  ) %>% 
  ungroup()

#writing csv for easy re-loading
write.csv(donationsmonth, file = "donationsmonth_newAPI.csv")
write.csv(donationsweek, file = "donationsweek_newAPI.csv")


######
#########tweet counts merge

#First import master list of all handles and icpsr numbers
Twitter <- read.csv("~/Dropbox/Projects/Twitter/Twitter/master_handles w FEC and Official Tag.csv")
twittercodes <- dplyr::select(Twitter, twitter, icpsr)

#import tweets
tweets <- read.csv("tweets_newapi.csv")

#create timing variables
tweets$date <- as.Date(tweets$created_at)

tweets$year <- format(as.Date(tweets$date, format="%m/%d/%Y"),"%Y")

tweets$week <- lubridate::week(ymd(tweets$date))

tweets$month <- lubridate::month(ymd(tweets$date))

tweets$yearmonth <- paste0(as.character(tweets$year),"-", as.character(tweets$month))
tweets$yearweek <- paste0(as.character(tweets$year),"-", as.character(tweets$week))

#merge in icpsr
twittercodes$author_username <- tolower(twittercodes$twitter)
tweets$author_username <- tolower(tweets$author_username)
tweetsicpsr <- left_join(tweets, twittercodes, by='author_username')
tweetsicpsr %<>% filter(!is.na(icpsr))

#group tweets by date and icpsr
tweetsmonth <- tweetsicpsr %>%
  group_by(icpsr, yearmonth) %>% 
  dplyr::summarize(
    tweets = n(),
    polarizing = sum(polarizing),
    uncivil = sum(uncivil)
  ) %>% 
  ungroup()

tweetsweek <- tweetsicpsr %>%
  group_by(icpsr, yearweek) %>% 
  dplyr::summarize(
    yearmonth = unique(yearmonth),
    tweets = n(),
    polarizing = sum(polarizing),
    uncivil = sum(uncivil)
  ) %>% 
  ungroup()


### If members are in a Congress and had twitter, make sure there is a row for them in tweetsmonth and tweetsweek

# Join icpsr to tweets data

# Pull date of first tweet of each member
firstTweet <- tweetsicpsr %>%
  group_by(icpsr) %>%
  dplyr::summarise(date=min(date))

# Pull unique yearmonths and yearweeks
yearmonths <- tweetsicpsr %>%
  group_by(yearmonth) %>%
  dplyr::summarise(
    year=unique(year)
  ) %>%
  filter(year %in% 2009:2018)
yearweeks <- tweetsicpsr %>%
  group_by(yearweek) %>%
  dplyr::summarise(
    year=unique(year)
  ) %>%
  filter(year %in% 2009:2018)

# Add congress to yearmonths and yearweeks
congDF <- data.frame(year=unique(yearmonths$year),
                     congress=c(111, 111, 112, 112, 113, 113, 114, 114, 115, 115))
yearmonths <- left_join(yearmonths, congDF, by='year')
yearweeks <- left_join(yearweeks, congDF, by='year')

# Make dataframes of unique icpsr-yearmonth and icpsr-yearweek pairs
monthIDs <- left_join(members, yearmonths, by='congress')
weekIDs <- left_join(members, yearweeks, by='congress')

# Loop through yearmonths and then yearweeks, check for relevant members, add rows if necessary
for(ii in 1:nrow(yearmonths)){
  # Designate month
  month <- yearmonths$yearmonth[ii]
  # Pull members from that month
  mems <- filter(monthIDs, yearmonth==month)
  # Pull tweets for that month
  twts <- filter(tweetsmonth, yearmonth==month)
  # See if any members who didn't tweet this month
  missing <- mems$icpsr[!mems$icpsr %in% intersect(mems$icpsr, twts$icpsr)]
  # Create dataframe of values for those members, if any
  if(length(missing) > 0){
    to_add <- data.frame(
      icpsr=missing,
      yearmonth=month,
      tweets=0,
      polarizing=0,
      uncivil=0
    ) %>%
      left_join(firstTweet, by='icpsr')
    to_add %<>% mutate(month=str_extract(yearmonth, '-.+$') %>% str_extract(., '[0-9]+') %>% char(),
                       year=str_extract(yearmonth, '^[0-9]{4}'))
    to_add$month[nchar(to_add$month)==1] <- paste0('0', to_add$month[nchar(to_add$month)==1])
    to_add %<>% mutate(tmpDate = as.Date(paste0(year, '-', month, '-01')))
    to_add %<>% filter(!is.na(date) & date < tmpDate) %>%
      dplyr::select(-c(date, month, year, tmpDate))
    tweetsmonth <- rbind(tweetsmonth, to_add)
    
  }
}
for(ii in 1:nrow(yearweeks)){
  # Designate week
  week <- yearweeks$yearweek[ii]
  # Pull members from that week
  mems <- filter(weekIDs, yearweek==week)
  # Pull tweets for that month
  twts <- filter(tweetsweek, yearweek==week)
  # See if any members who didn't tweet this month
  missing <- mems$icpsr[!mems$icpsr %in% intersect(mems$icpsr, twts$icpsr)]
  # Create dataframe of values for those members, if any
  if(length(missing) > 0){
    to_add <- data.frame(
      icpsr=missing,
      yearweek=week,
      yearmonth=unique(twts$yearmonth)[1],
      tweets=0,
      polarizing=0,
      uncivil=0
    ) %>%
      left_join(firstTweet, by='icpsr')
    to_add %<>% mutate(month=str_extract(yearmonth, '-.+$') %>% str_extract(., '[0-9]+') %>% char(),
                       year=str_extract(yearmonth, '^[0-9]{4}'))
    to_add$month[nchar(to_add$month)==1] <- paste0('0', to_add$month[nchar(to_add$month)==1])
    to_add %<>% mutate(tmpDate = as.Date(paste0(year, '-', month, '-01')))
    to_add %<>% filter(!is.na(date) & date < tmpDate) %>%
      dplyr::select(-c(date, month, year, tmpDate))
    tweetsweek <- rbind(tweetsweek, to_add)
  }
}


#make percentages

tweetsmonth$pct.polarizing <- ((tweetsmonth$polarizing/tweetsmonth$tweets) *100)
tweetsmonth$pct.uncivil <- ((tweetsmonth$uncivil/tweetsmonth$tweets) *100)

tweetsweek$pct.polarizing <- ((tweetsweek$polarizing/tweetsweek$tweets) *100)
tweetsweek$pct.uncivil <- ((tweetsweek$uncivil/tweetsweek$tweets) *100)

#create unique member-date identifiers

tweetsmonth$icpsryearmonth <- paste0(as.character(tweetsmonth$icpsr),"M", as.character(tweetsmonth$yearmonth))
tweetsweek$icpsryearweek <- paste0(as.character(tweetsweek$icpsr),"M", as.character(tweetsweek$yearweek))

donationsmonth$icpsryearmonth <- paste0(as.character(donationsmonth$icpsr),"M", as.character(donationsmonth$yearmonth))
donationsweek$icpsryearweek <- paste0(as.character(donationsweek$icpsr),"M", as.character(donationsweek$yearweek))

# Save tweets by week/month for use with other scripts
write.csv(tweetsmonth, 'tweetsmonth.csv', row.names=FALSE)
write.csv(tweetsweek, 'tweetsweek.csv', row.names=FALSE)

#merge tweets and donors

dandtmonth <- left_join(donationsmonth, tweetsmonth, by = "icpsryearmonth")
dandtweek <- left_join(donationsweek, tweetsweek, by = "icpsryearweek")

dandtmonth <- merge(tweetsmonth, donationsmonth, by = "icpsryearmonth")
dandtweek <- merge(tweetsweek, donationsweek, by = "icpsryearweek")

######
######
######
######
######
##############analysis

dandtmonth$polarizing_sq <- ((dandtmonth$polarizing * dandtmonth$polarizing) / dandtmonth$tweets)
dandtweek$polarizing_sq <- ((dandtweek$polarizing * dandtweek$polarizing) / dandtweek$tweets)


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

