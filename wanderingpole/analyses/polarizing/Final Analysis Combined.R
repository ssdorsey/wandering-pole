library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)


#### Turnout Analysis
results <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/turnoutfirstyear.csv")

results <- filter(results, firstyearmatch == 0)
results <- results %>% filter(!is.na(Gturnout_pct_difference))
results$Gturnout_pct_difference <- as.numeric(results$Gturnout_pct_difference)


turnout1 <- felm(Gturnout_pct_difference ~ tweets_diff | yearstate, data = results)
turnout2 <- felm(Gturnout_pct_difference ~ incivil_diff | yearstate, data = results)
turnout3 <- felm(Gturnout_pct_difference ~ incivilpp_diff | yearstate, data = results)
turnout4 <- felm(Gturnout_pct_difference ~ divisive_sq_diff | yearstate, data = results)

tab_model(lmturnout1, lmturnout2, lmturnout3, lmturnout4)

stargazer(turnout1, turnout2, turnout3, turnout4, 
          Title = "Cycle-on-Cycle Election Turnout Percentage Change (VAP) Predicted by Twitter Activity")


#### Donor Month Analysis

donordata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donorsmonthtweets.csv")

donordata <- donordata %>% 
  rename(
    Proportion.Incivil = incivilpercent,
    Incivil.Tweets = incivil,
    Total.Tweets = tweets
  )

donordataactive <- filter(donordata, Total.Tweets > 0)

summary(monthamountpp <- felm(monthamount ~ Proportion.Incivil | icpsr + yearmonth, data=donordataactive))
summary(monthamountcount <- felm(monthamount ~ Incivil.Tweets  | icpsr + yearmonth, data=donordata))
summary(monthamounttweets <- felm(monthamount ~ Total.Tweets | icpsr + yearmonth, data=donordata))
summary(monthamountsq <- felm(monthamount ~ divisive_sq | icpsr + yearmonth, data=donordata))

summary(monthdonorspp <- felm(monthdonors ~ Proportion.Incivil | icpsr + yearmonth, data=donordataactive))
summary(monthdonorscount <- felm(monthdonors ~ Incivil.Tweets  | icpsr + yearmonth, data=donordata))
summary(monthdonorstweets <- felm(monthdonors ~ Total.Tweets | icpsr + yearmonth, data=donordata))
summary(monthdonorssq <- felm(monthdonors ~ divisive_sq | icpsr + yearmonth, data=donordata))

stargazer(monthamountpp, monthamountcount, monthamounttweets, monthamountsq, monthdonorspp, monthdonorscount, monthdonorstweets, monthdonorssq,
          Title = "Monthly Member Twitter Activity and Fundraising Trends", type="html",  out="Monthly Twitter Donations.htm")


#### Donor Week Analysis

donorweekdata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Donor Files/donorsweektweets.csv")

donorweekdata <- donorweekdata %>% 
  rename(
    Proportion.Incivil = incivilpercent,
    Incivil.Tweets = incivil,
    Total.Tweets = tweets
  )

donorweekdataactive <- filter(donorweekdata, Total.Tweets > 0)

summary(weekamountpp <- felm(weekamount ~ Proportion.Incivil | icpsr + yearweek, data=donorweekdataactive))
summary(weekamountcount <- felm(weekamount ~ Incivil.Tweets  | icpsr + yearweek, data=donorweekdata))
summary(weekamounttweets <- felm(weekamount ~ Total.Tweets | icpsr + yearweek, data=donorweekdata))
summary(weekamountsq <- felm(weekamount ~ divisive_sq | icpsr + yearweek, data=donorweekdata))

summary(weekdonorspp <- felm(weekdonors ~ Proportion.Incivil | icpsr + yearweek, data=donorweekdataactive))
summary(weekdonorscount <- felm(weekdonors ~ Incivil.Tweets  | icpsr + yearweek, data=donorweekdata))
summary(weekdonorstweets <- felm(weekdonors ~ Total.Tweets | icpsr + yearweek, data=donorweekdata))
summary(weekdonorssq <- felm(weekdonors ~ divisive_sq | icpsr + yearweek, data=donorweekdata))

stargazer(weekamountpp, weekamountcount, weekamounttweets, weekamountsq, weekdonorspp, weekdonorscount, weekdonorstweets, weekdonorssq,
          Title = "Weekly Member Twitter Activity and Fundraising Trends", type="html",  out="Weekly Twitter Donations.htm")


#### Follower Analysis

tweets <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Followers analysis/divisivefollowers.csv")

tweets <- filter(tweets, follower_change > -10000)
tweets <- filter(tweets, icpsr != "NA")
tweets$follower_change_pct <- (tweets$follower_change/tweets$followers)*100
tweets <- filter(tweets, follower_change_pct > -20)
tweets <- filter(tweets, follower_change_pct < 50)

summary(weeklyfollowersuncivil <- felm(follower_change ~ divisive | icpsr + weekyear, data=tweets) )
summary(weeklyfollowerstweets <- felm(follower_change ~ tweets.y | icpsr + weekyear, data=tweets) )
summary(weeklyfollowerspct <- felm(follower_change ~ divisivepp | icpsr + weekyear, data=tweets) )
summary(weeklyfollowerssq <- felm(follower_change ~ divisive_sq | icpsr + weekyear, data=tweets) )

tab_model(weeklyfollowersuncivil, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowerssq)

summary(weeklyfollowersuncivil2 <- felm(follower_change_pct ~ divisive | icpsr + weekyear, data=tweets) )
summary(weeklyfollowerstweets2 <- felm(follower_change_pct ~ tweets.y | icpsr + weekyear, data=tweets) )
summary(weeklyfollowerspct2 <- felm(follower_change_pct ~ divisivepp | icpsr + weekyear, data=tweets) )
summary(weeklyfollowerssq2 <- felm(follower_change_pct ~ divisive_sq | icpsr + weekyear, data=tweets) )

tab_model(weeklyfollowersuncivil2, weeklyfollowerstweets2, weeklyfollowerspct2, weeklyfollowerssq2)

stargazer(weeklyfollowersuncivil, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowerssq, weeklyfollowersuncivil2, weeklyfollowerstweets2, weeklyfollowerspct2, weeklyfollowerssq2,
          Title = "Weekly Percentage Change in Twitter Followers Predicted by Twitter Activity", type="html",  out="Weekly Twitter Followers Change.htm")

