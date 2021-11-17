library(ggplot2)
library(dplyr)
library(lubridate)


library(dotwhisker)
library(broom)
library(jtools)
library(broom.mixed)

library(sjPlot)
library(MatchIt)
library(Matching)
library(sandwich)
library(AER)
library(estimatr)
library(stargazer)
library(jtools)
library(rgenoud)
library(plm)
library(gridExtra)
library(ggpubr)

TwitterTrim <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/TrimmedHandles.csv")


donors10 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Merged10Donors.csv")
donors12 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Merged12Donors.csv")
donors14 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Merged14Donors.csv")
donors16 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Merged16Donors.csv")
donors18 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Merged18Donors.csv")

donors10trim <- merge(TwitterTrim, donors10, by = "twitter")
donors12trim <- merge(TwitterTrim, donors12, by = "twitter")
donors14trim <- merge(TwitterTrim, donors14, by = "twitter")
donors18trim <- merge(TwitterTrim, donors18, by = "twitter")

write.csv(donors10trim, file = "donors10trim.csv")
write.csv(donors12trim, file = "donors12trim.csv")
write.csv(donors14trim, file = "donors14trim.csv")
write.csv(donors16trim, file = "donors16trim.csv")
write.csv(donors18trim, file = "donors18trim.csv")

donors10trim <- select(donors10trim, twitter, FECID,
                       icpsr.x, first.x, last.x, fullname.x, state.x, chamber.x, district.x, party.x, Name, City, State, ZIP, Date, Amount)
donors12trim <- select(donors12trim, twitter, FECID,
                       icpsr.x, first.x, last.x, fullname.x, state.x, chamber.x, district.x, party.x, Name, City, State, ZIP, Date, Amount)
donors14trim <- select(donors14trim, twitter, FECID,
                       icpsr.x, first.x, last.x, fullname.x, state.x, chamber.x, district.x, party.x, Name, City, State, ZIP, Date, Amount)
donors18trim <- select(donors18trim, twitter, FECID,
                       icpsr.x, first.x, last.x, fullname.x, state.x, chamber.x, district.x, party.x, Name, City, State, ZIP, Date, Amount)


donors10 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors10trim.csv")
donors12 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors12trim.csv")
donors14 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors14trim.csv")
donors16 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors16trim.csv")
donors18 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors18trim.csv")

donors10$newdate <- mdy(donors10$Date)
donors12$newdate <- mdy(donors12$Date)
donors14$newdate <- mdy(donors14$Date)
donors16$newdate <- mdy(donors16$Date)
donors18$newdate <- mdy(donors18$Date)

donors10$OutOfState <- ifelse(donors10$state.x == donors10$State, "0", "1")

donors10group <- donors10 %>%
  group_by(FECID, newdate) %>% 
  dplyr::summarize(
    donations = n(),
    total = sum(Amount),
    OutofStateCount = sum(OutOfState)
  ) %>% 
  ungroup()

donors12group <- donors12 %>%
  group_by(FECID, newdate) %>% 
  dplyr::summarize(
    donations = n(),
    total = sum(Amount),
    OutofStateCount = sum(OutOfState)
  ) %>% 
  ungroup()

donors14group <- donors14 %>%
  group_by(FECID, newdate) %>% 
  dplyr::summarize(
    donations = n(),
    total = sum(Amount),
    OutofStateCount = sum(OutOfState)
  ) %>% 
  ungroup()

donors16group <- donors16 %>%
  group_by(FECID, newdate) %>% 
  dplyr::summarize(
    donations = n(),
    total = sum(Amount)
  ) %>% 
  ungroup()

donors18group <- donors18 %>%
  group_by(FECID, newdate) %>% 
  dplyr::summarize(
    donations = n(),
    total = sum(Amount)
  ) %>% 
  ungroup()



write.csv(donors10, file = "donors10trim.csv")
write.csv(donors12, file = "donors12trim.csv")
write.csv(donors14, file = "donors14trim.csv")
write.csv(donors16, file = "donors16trim.csv")
write.csv(donors18, file = "donors18trim.csv")

write.csv(donors10group, file = "donors10group.csv")
write.csv(donors12group, file = "donors12group.csv")
write.csv(donors14group, file = "donors14group.csv")
write.csv(donors16group, file = "donors16group.csv")
write.csv(donors18group, file = "donors18group.csv")

donor10names <- select(donors10, FECID, icpsr.x, fullname.x)

donors10final <- merge(donors10group, TwitterTrim, by = "FECID")
donors12final <- merge(donors12group, TwitterTrim, by = "FECID")
donors14final <- merge(donors14group, TwitterTrim, by = "FECID")
donors16final <- merge(donors16group, TwitterTrim, by = "FECID")
donors18final <- merge(donors18group, TwitterTrim, by = "FECID")

write.csv(donors10final, file = "donors10final.csv")
write.csv(donors12final, file = "donors12final.csv")
write.csv(donors14final, file = "donors14final.csv")
write.csv(donors16final, file = "donors16final.csv")
write.csv(donors18final, file = "donors18final.csv")


donors10 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donors10final.csv")
donors12 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donors12final.csv")
donors14 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donors14final.csv")
donors16 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donors16final.csv")
donors18 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donors18final.csv")

donorscomplete <- rbind(donors10, donors12, donors14, donors16, donors18)
write.csv(donorscomplete, file = "donorscombined.csv")


# wiggling
donors18 <- merge(donors18, twitterparty, by = "icpsr")


donors181 <- donors18[1:999999, ]
donors182 <- donors18[1000000:1999998, ]
donors183 <- donors18[1999999:2923954, ]

write.csv(donors181, file = "donors181.csv")
write.csv(donors182, file = "donors182.csv")
write.csv(donors183, file = "donors183.csv")

donors181 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors182.csv")
donors182 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors182.csv")
donors183 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/donors183.csv")

donors18complete <- rbind(donors181, donors182)
donors18complete <- rbind(donors18complete, donors183)

write.csv(donors18complete, file = "donors18combined.csv")



# tweet counts merge

TwitterTrim <- read.csv("C:/Users/User/Dropbox/Twitter/master_handles w FEC and Official Tag.csv")
twittercodes <- dplyr::select(TwitterTrim, twitter, icpsr)

handlemonth <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/handle-month.csv")
handleweek <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/handle-week.csv")
handleday <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/handle-day.csv")

handleday$tweet_count <- (handleday$count_incivil + handleday$count_civil)

handlemonthicpsr <- merge(handlemonth, twittercodes, by = "twitter")
handleweekicpsr <- merge(handleweek, twittercodes, by = "twitter")
handledayicpsr <- merge(handleday, twittercodes, by.x = "screen_name", by.y = "twitter")

handledayicpsr$year <- year(handledayicpsr$last)
handledayicpsr$month <- month(handledayicpsr$last)
handledayicpsr$day <- day(handledayicpsr$last)

handledayicpsr$dayID <- paste(handledayicpsr$year, handledayicpsr$month, handledayicpsr$day, sep = ".")

daytrim <- filter(handledayicpsr, month > 9)



write.csv(handlemonthicpsr, file = "twittermonthmerge.csv")
write.csv(handleweekicpsr, file = "twitterweekmerge.csv")


handlemonthgroup <- handlemonthicpsr %>%
  group_by(icpsr, yearmonth) %>% 
  dplyr::summarize(
    tweets = sum(tweet_count),
    civil = sum(count_civil),
    incivil = sum(count_incivil)
  ) %>% 
  ungroup()

write.csv(handlemonthgroup, file = "handlemonthgroup.csv")

handleweekgroup <- handleweekicpsr %>%
  group_by(icpsr, yearweek) %>% 
  dplyr::summarize(
    tweets = sum(tweet_count),
    civil = sum(count_civil),
    incivil = sum(count_incivil)
  ) %>% 
  ungroup()

write.csv(handleweekgroup, file = "handleweekgroup.csv")

handledaygroup <- handledayicpsr %>%
  group_by(icpsr, dayID) %>% 
  dplyr::summarize(
    tweets = sum(tweet_count),
    civil = sum(count_civil),
    incivil = sum(count_incivil)
  ) %>% 
  ungroup()

write.csv(handledaygroup, file = "handledaygroup.csv")

daytrim <- filter(handledayicpsr, month > 9)
daytrim <- filter(daytrim, month < 12)
daytrim <- filter(daytrim, year == 2008 | year == 2010 | year == 2012 | year == 2014 | year == 2016 | year == 2018)


handledaygroup <- daytrim %>%
  group_by(icpsr, dayID) %>% 
  dplyr::summarize(
    tweets = sum(tweet_count),
    civil = sum(count_civil),
    incivil = sum(count_incivil)
  ) %>% 
  ungroup()

handledaygroup$date <- ymd(handledaygroup$dayID)

write.csv(handledaygroup, file = "handledaygrouptrim.csv")

handledaytrim <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/handledaygrouptrim.csv")

handledaytrim <- filter(handledaytrim, inrange == 1)

handledaygrouptrim <- handledaytrim %>%
  group_by(yearicpsr) %>% 
  dplyr::summarize(
    tweets = sum(tweets),
    civil = sum(civil),
    incivil = sum(incivil)
  ) %>% 
  ungroup()


handledaygrouptrim$incivilpp <- handledaygrouptrim$incivil / handledaygrouptrim$tweets

write.csv(handledaygrouptrim, file = "preelectiontweetsgrouped.csv")





donordata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donorscombined.csv")

donordatagroup <- donordata %>%
  group_by(icpsryearmonth) %>% 
  dplyr::summarize(
    monthamount = sum(total),
    monthdonors = sum(donations),
    monthOOS = sum(OutofStateCount)
  ) %>% 
  ungroup()

handlemonthgroupicpsr <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/handlemonthgroup.csv")

donordatagroup <- donordata %>%
  group_by(icpsryearweek) %>% 
  dplyr::summarize(
    monthamount = sum(total),
    monthdonors = sum(donations),
    monthOOS = sum(OutofStateCount)
  ) %>% 
  ungroup()

donorstweets <- merge(donordatagroup, handlemonthgroupicpsr, by = "icpsryearmonth")
donorstweets <- merge(donordatagroup, handleweekgroup, by = "icpsryearweek")

write.csv(donorstweets, file = "donorstweetsmonth.csv")
write.csv(donorstweets, file = "donorstweetsweek.csv")


# month analysis

donordata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donorsmonthtweets.csv")

donordata <- donordata %>% 
  rename(
    Proportion.Incivil = incivilpercent,
    Incivil.Tweets = incivil,
    Total.Tweets = tweets
  )

donordata$incivillog <- log1p(donordata$Incivil.Tweets)

donordataactive <- filter(donordata, Total.Tweets > 0)


summary(plm.monthamountpp <- plm(monthamount ~ Proportion.Incivil, data=donordataactive, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthamountcount <- plm(monthamount ~ Incivil.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthamountcountlog <- plm(monthamount ~ incivillog, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))

summary(plm.monthdonorspp <- plm(monthdonors ~ Proportion.Incivil, data=donordataactive, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthdonorscount <- plm(monthdonors ~ Incivil.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthdonorscountlog <- plm(monthdonors ~ incivillog, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))

summary(plm.monthOOSpp <- plm(monthOOS ~ Proportion.Incivil, data=donordataactive, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthOOScount <- plm(monthOOS ~ Incivil.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthOOScountlog <- plm(monthOOS ~ incivillog, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))

summary(plm.monthOOSPPpp <- plm(monthOOSPP ~ Proportion.Incivil, data=donordataactive, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthOOSPPcount <- plm(monthOOSPP ~ Incivil.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthOOSPPcountlog <- plm(monthOOSPP ~ incivillog, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))

# tab_model(plm.monthamountpp, plm.monthamountcount, plm.monthamountcountlog, plm.monthdonorspp, plm.monthdonorscount, plm.monthdonorscountlog, plm.monthOOSpp, plm.monthOOScount, plm.monthOOScountlog, plm.monthOOSPPpp, plm.monthOOSPPcount, plm.monthOOSPPcountlog)

summary(plm.monthamounttweets <- plm(monthamount ~ Total.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthdonorstweets <- plm(monthdonors ~ Total.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthOOSttweets <- plm(monthOOS ~ Total.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))
summary(plm.monthOOSPPtweets <- plm(monthOOSPP ~ Total.Tweets, data=donordata, index = c("icpsr", "yearmonth"), model = "within", effect = "twoways"))

# tab_model(plm.monthamounttweets, plm.monthdonorstweets, plm.monthOOSttweets, plm.monthOOSPPtweets)

stargazer(plm.monthamountpp, plm.monthamountcount, plm.monthamountcountlog, plm.monthamounttweets, plm.monthdonorspp, plm.monthdonorscount, plm.monthdonorscountlog, plm.monthdonorstweets, plm.monthaOOSpp, plm.monthOOScount, plm.monthOOScountlog, plm.monthOOSttweets, plm.monthaOOSPPpp, plm.monthOOSPPcount, plm.monthOOSPPcountlog, plm.monthOOSPPtweets, Title = "Monthly Candidate Donation Trends, Predicted by Twitter Activity", type="html",  out="Table 1 Monthly Donations.htm")

month1 <- plot_coefs(plm.monthamountpp,
                    coefs=c(`Percent Uncivil`='Proportion.Incivil'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
                    xlab('') +
                    xlim(-250, 750) +
                    theme(plot.title = element_text(hjust = 0.5))

month2 <- plot_coefs(plm.monthamountcount,
                    coefs=c(`Uncivil Tweets `='Incivil.Tweets'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('') +
  xlim(-250, 750) +
  theme(plot.title = element_text(hjust = 0.5))

month3 <- plot_coefs(plm.monthamounttweets,
                    coefs=c(`Total Tweets  `='Total.Tweets'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('Monthly Additional Dollars Received') +
  xlim(-250, 750) +
  theme(plot.title = element_text(hjust = 0.5))

month4 <- plot_coefs(plm.monthdonorspp,
                    coefs=c(`Percent Uncivil`='Proportion.Incivil'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('') +
  xlim(0, 5) +
  theme(plot.title = element_text(hjust = 0.5))

month5 <- plot_coefs(plm.monthdonorscount,
                    coefs=c(`Uncivil Tweets `='Incivil.Tweets'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('') +
  xlim(0, 5) +
  theme(plot.title = element_text(hjust = 0.5))

month6 <- plot_coefs(plm.monthdonorstweets,
                    coefs=c(`Total Tweets  `='Total.Tweets'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('Monthly Additional Donors') +
  xlim(0, 5) +
  theme(plot.title = element_text(hjust = 0.5))

monthdonationscombined <- ggarrange(month1, month4, month2, month5, month3, month6,
                                   labels = c("", ""),
                                   ncol = 2, nrow = 3, legend = NULL)
monthdonationscombined

# week analysis

donorweekdata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donorsweektweets.csv")

donorweekdata <- donorweekdata %>% 
  rename(
    Proportion.Incivil = incivilpercent,
    Incivil.Tweets = incivil,
    Total.Tweets = tweets
  )

donorweekdata$incivillog <- log1p(donorweekdata$Incivil.Tweets)

donorweekdataactive <- filter(donorweekdata, Total.Tweets > 0)


summary(plm.weekamountpp <- plm(weekamount ~ Proportion.Incivil, data=donorweekdataactive, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekamountcount <- plm(weekamount ~ Incivil.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekamountcountlog <- plm(weekamount ~ incivillog, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))

summary(plm.weekdonorspp <- plm(weekdonors ~ Proportion.Incivil, data=donorweekdataactive, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekdonorscount <- plm(weekdonors ~ Incivil.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekdonorscountlog <- plm(weekdonors ~ incivillog, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))

summary(plm.weekOOSpp <- plm(weekOOS ~ Proportion.Incivil, data=donorweekdataactive, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekOOScount <- plm(weekOOS ~ Incivil.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekOOScountlog <- plm(weekOOS ~ incivillog, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))

summary(plm.weekOOSPPpp <- plm(weekOOSPP ~ Proportion.Incivil, data=donorweekdataactive, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekOOSPPcount <- plm(weekOOSPP ~ Incivil.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))
summary(plm.weekOOSPPcountlog <- plm(weekOOSPP ~ incivillog, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within", effect = "twoways"))

#  tab_model(plm.weekamountpp, plm.weekamountcount, plm.weekamountcountlog, plm.weekdonorspp, plm.weekdonorscount, plm.weekdonorscountlog, plm.weekOOSpp, plm.weekOOScount, plm.weekOOScountlog, plm.weekOOSPPpp, plm.weekOOSPPcount, plm.weekOOSPPcountlog)

summary(plm.weekamounttweets <- plm(weekamount ~ Total.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within"))
summary(plm.weekdonorstweets <- plm(weekdonors ~ Total.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within"))
summary(plm.weekOOSttweets <- plm(weekOOS ~ Total.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within"))
summary(plm.weekOOSPPtweets <- plm(weekOOSPP ~ Total.Tweets, data=donorweekdata, index = c("icpsr", "yearweek"), model = "within"))

stargazer(plm.weekamountpp, plm.weekamountcount, plm.weekamountcountlog, plm.weekamounttweets, plm.weekdonorspp, plm.weekdonorscount, plm.weekdonorscountlog, plm.weekdonorstweets, plm.weekOOSpp, plm.weekOOScount, plm.weekOOScountlog, plm.weekOOSttweets, plm.weekOOSPPpp, plm.weekOOSPPcount, plm.weekOOSPPcountlog, plm.weekOOSPPtweets, Title = "Weekly Candidate Donation Trends, Predicted by Twitter Activity", type="html",  out="Table 2 Weekly Donations.htm")

week1 <- plot_coefs(plm.weekamountpp,
                      coefs=c(`Percent Uncivil`='Proportion.Incivil'),
                      scale=TRUE,
                      inner_ci_level=0.9,
                      colors=c('black')) +
  xlab('') +
  xlim(-100, 500) +
  theme(plot.title = element_text(hjust = 0.5))

week2 <- plot_coefs(plm.weekamountcount,
           coefs=c(`Uncivil Tweets `='Incivil.Tweets'),
           scale=TRUE,
           inner_ci_level=0.9,
           colors=c('black')) +
  xlab('') +
  xlim(-100, 500) +
  theme(plot.title = element_text(hjust = 0.5))

week3 <- plot_coefs(plm.weekamounttweets,
           coefs=c(`Total Tweets  `='Total.Tweets'),
           scale=TRUE,
           inner_ci_level=0.9,
           colors=c('black')) +
  xlab('Weekly Additional Dollars Received') +
  xlim(-100, 500) +
  theme(plot.title = element_text(hjust = 0.5))

week4 <- plot_coefs(plm.weekdonorspp,
           coefs=c(`Percent Uncivil`='Proportion.Incivil'),
           scale=TRUE,
           inner_ci_level=0.9,
           colors=c('black')) +
  xlab('') +
  xlim(-1, 5) +
  theme(plot.title = element_text(hjust = 0.5))

week5 <- plot_coefs(plm.weekdonorscount,
           coefs=c(`Uncivil Tweets `='Incivil.Tweets'),
           scale=TRUE,
           inner_ci_level=0.9,
           colors=c('black')) +
  xlab('') +
  xlim(-1, 5) +
  theme(plot.title = element_text(hjust = 0.5))

week6 <- plot_coefs(plm.weekdonorstweets,
           coefs=c(`Total Tweets  `='Total.Tweets'),
           scale=TRUE,
           inner_ci_level=0.9,
           colors=c('black')) +
          xlab('Weekly Additional Donors') +
          xlim(-1, 5) +
          theme(plot.title = element_text(hjust = 0.5))

weekdonationscombined <- ggarrange(week1, week4, week2, week5, week3, week6,
                                   labels = c("", ""),
                                   ncol = 2, nrow = 3, legend = NULL)
weekdonationscombined

# Weekamount <- plot_summs(plm.weekamountpp, plm.weekamountcount, plm.weekamounttweets, scale = TRUE, inner_ci_level = .9, colors=c('black')) + ggtitle("Weekly Donation Amounts ($)") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
# weekdonors <- plot_summs(plm.weekdonorspp, plm.weekdonorscount, plm.weekdonorstweets, scale = TRUE, inner_ci_level = .9, colors=c('black')) + ggtitle("Weekly Number of Donors") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
# weekOOS <- plot_summs(plm.weekOOSpp, plm.weekOOScount, plm.weekOOSttweets, scale = TRUE, inner_ci_level = .9) + ggtitle("Weekly Number Out-of-State Donors") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")
# weekOOSPP <- plot_summs(plm.weekOOSPPpp, plm.weekOOSPPcount, plm.weekOOSPPtweets, scale = TRUE, inner_ci_level = .9) + ggtitle("Weekly Proportion Out-of-State Donors") + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position = "none")

