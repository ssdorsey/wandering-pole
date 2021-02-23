library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(ggpubr)


#### Turnout Analysis
results <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/turnoutfirstyear.csv")

results <- filter(results, firstyearmatch == 0)
results <- results %>% filter(!is.na(Gturnout_pct_difference))
results$Gturnout_pct_difference <- as.numeric(results$Gturnout_pct_difference)


summary(turnout1 <- felm(Gturnout_pct_difference ~ tweets_diff | yearstate, data = results))
summary(turnout2 <- felm(Gturnout_pct_difference ~ incivil_diff | yearstate, data = results))
summary(turnout3 <- felm(Gturnout_pct_difference ~ incivilpp_diff | yearstate, data = results))
summary(turnout4 <- felm(Gturnout_pct_difference ~ divisive_sq_diff | yearstate, data = results))

tab_model(turnout1, turnout2, turnout3, turnout4)

stargazer(turnout1, turnout2, turnout3, turnout4, 
          Title = "Cycle-on-Cycle Election Turnout Percentage Change (VAP) Predicted by Twitter Activity")


#### Donor Month Analysis

donordata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/donorstweetsmonthly.csv")

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

monthamountcoefs <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/monthlyamount coefs.csv")
monthdonorcoefs <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/monthlydonor coefs.csv")

monthamountcoefs$coef <- monthamountcoefs$ï..coef
monthdonorcoefs$coef <- monthdonorcoefs$ï..coef

monthamountcoefs$coef <- factor(monthamountcoefs$coef, levels =monthamountcoefs$coef)
monthdonorcoefs$coef <- factor(monthdonorcoefs$coef, levels = monthdonorcoefs$coef)

ggplot(monthamountcoefs) +
  geom_pointrange( aes(x=coef, y=amountincrease, ymin=amountincrease_low, ymax=amountincrease_up), colour="black", alpha=0.9, size=1.3)+
  geom_bar(aes(x=coef, y=amountincrease, ymin=amountincrease_low, ymax=amountincrease_up), position = 'dodge', stat='identity') +
  geom_text(aes(x=coef, y=amountincrease, label=amountincrease), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(y = "Monthly Donation Amount Increase")+
  theme(axis.title.x = element_blank(), axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(labels = c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * (% Polarizing)'))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

ggplot(monthdonorcoefs) +
  geom_pointrange( aes(x=coef, y=donorincrease, ymin=donorincrease_low, ymax=donorincrease_up), colour="black", alpha=0.9, size=1.3)+
  labs(y = "Monthly Donors Increase")+
  theme(axis.title.x = element_blank(), axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(labels = c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * (% Polarizing)'))+
  ylim(0,4)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

barCenters <- barplot(monthamountcoefs$amountincrease, ylim=c(-200, 800))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/DivisiveMonthAmount.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(monthamountcoefs$amountincrease, ylim=c(-200, 800), ylab='Monthly Fundraising Amount Increase', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=monthamountcoefs$amountincrease_up[ii], y1=monthamountcoefs$amountincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=monthamountcoefs$amountincrease_up[ii], y1=monthamountcoefs$amountincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=monthamountcoefs$amountincrease[ii]/4, paste0(round(monthamountcoefs$amountincrease[ii], 2), ' +/- ', round(monthamountcoefs$amountincrease_ci[ii], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

barCenters <- barplot(monthdonorcoefs$donorincrease, ylim=c(0, 4))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/DivisiveMonthDonors.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(monthdonorcoefs$donorincrease, ylim=c(0, 4), ylab='Monthly Donors Increase', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=monthdonorcoefs$donorincrease_up[ii], y1=monthdonorcoefs$donorincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=monthdonorcoefs$donorincrease_up[ii], y1=monthdonorcoefs$donorincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=monthdonorcoefs$donorincrease[ii]/4, paste0(round(monthdonorcoefs$donorincrease[ii], 2), ' +/- ', round(monthdonorcoefs$donorincrease_ci[ii], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()


#### Donor Week Analysis

donorweekdata <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/donorstweetsweekly.csv")

donorweekdata <- donorweekdata %>% 
  rename(
    Proportion.Incivil = incivilpercent,
    Incivil.Tweets = incivil,
    Total.Tweets = tweets
  )

donorweekdataactive <- filter(donorweekdata, Total.Tweets > 0)

summary(weekamountpp <- felm(weekamount ~ Proportion.Incivil | icpsr + yearweekpartychamber, data=donorweekdataactive))
summary(weekamountcount <- felm(weekamount ~ Incivil.Tweets  | icpsr + yearweekpartychamber, data=donorweekdata))
summary(weekamounttweets <- felm(weekamount ~ Total.Tweets | icpsr + yearweekpartychamber, data=donorweekdata))
summary(weekamountsq <- felm(weekamount ~ divisive_sq | icpsr + yearweekpartychamber, data=donorweekdata))

summary(weekdonorspp <- felm(weekdonors ~ Proportion.Incivil | icpsr + yearweekpartychamber, data=donorweekdataactive))
summary(weekdonorscount <- felm(weekdonors ~ Incivil.Tweets  | icpsr + yearweekpartychamber, data=donorweekdata))
summary(weekdonorstweets <- felm(weekdonors ~ Total.Tweets | icpsr + yearweekpartychamber, data=donorweekdata))
summary(weekdonorssq <- felm(weekdonors ~ divisive_sq | icpsr + yearweekpartychamber, data=donorweekdata))

stargazer(weekamountpp, weekamountcount, weekamounttweets, weekamountsq, weekdonorspp, weekdonorscount, weekdonorstweets, weekdonorssq,
          Title = "Weekly Member Twitter Activity and Fundraising Trends", type="html",  out="Weekly Twitter Donations.htm")


weekamountcoefs <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/weeklyamount coefs.csv")
weekdonorcoefs <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/weeklydonor coefs.csv")

weekamountcoefs$coef <- weekamountcoefs$ï..coef
weekdonorcoefs$coef <- weekdonorcoefs$ï..coef

weekamountcoefs$coef <- factor(weekamountcoefs$coef, levels = weekamountcoefs$coef)
weekdonorcoefs$coef <- factor(weekdonorcoefs$coef, levels = weekdonorcoefs$coef)

week1 <- ggplot(weekamountcoefs) +
  geom_pointrange( aes(x=coef, y=amountincrease, ymin=amountincrease_low, ymax=amountincrease_up), colour="black", alpha=0.9, size=1.3)+
  labs(x = "Weekly Donation Amount Increase")+
  theme(axis.title.x = element_blank(), axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(labels = c('# Tweets', '# Polarizing', '% Polarizing', 'Polarizing^2'))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  coord_flip()

week2 <- ggplot(weekdonorcoefs) +
  geom_pointrange( aes(x=coef, y=donorincrease, ymin=donorincrease_low, ymax=donorincrease_up), colour="black", alpha=0.9, size=1.3)+
  labs(x = "Weekly Donors Increase")+
  theme(axis.title.x = element_blank(), axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_x_discrete(labels = c('', '', '', ''))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  coord_flip()

ggarrange(week1, week2, ncol = 2, nrow = 1, common.legend = TRUE)

barCenters <- barplot(weekamountcoefs$amountincrease, ylim=c(-100, 500))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/DivisiveWeekAmount.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(weekamountcoefs$amountincrease, ylim=c(-100, 500), ylab='Weekly Fundraising Amount Increase', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=weekamountcoefs$amountincrease_up[ii], y1=weekamountcoefs$amountincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=weekamountcoefs$amountincrease_up[ii], y1=weekamountcoefs$amountincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=weekamountcoefs$amountincrease[ii]/4, paste0(round(weekamountcoefs$amountincrease[ii], 2), ' +/- ', round(weekamountcoefs$amountincrease_ci[ii], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

barCenters <- barplot(weekdonorcoefs$donorincrease, ylim=c(-1, 3))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/DivisiveWeekDonors.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(weekdonorcoefs$donorincrease, ylim=c(-1, 3), ylab='Weekly Donors Increase', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=weekdonorcoefs$donorincrease_up[ii], y1=weekdonorcoefs$donorincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=weekdonorcoefs$donorincrease_up[ii], y1=weekdonorcoefs$donorincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=weekdonorcoefs$donorincrease[ii]/4, paste0(round(weekdonorcoefs$donorincrease[ii], 2), ' +/- ', round(weekdonorcoefs$donorincrease_ci[ii], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()



#### Follower Analysis

tweets <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Donors Remerge Files/followerstweetsjoin.csv")

tweets <- filter(tweets, followerchange > -10000) %>%
  filter(followerchange_pct != Inf)
tweets <- filter(tweets, followerchange_pct > -20)
tweets <- filter(tweets, followerchange_pct < 50)
tweets %<>% mutate(polarizing_sq = polarizing * pct.polarizing)

summary(weeklyfollowersuncivil <- felm(followerchange ~ polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerstweets <- felm(followerchange ~ tweets | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerspct <- felm(followerchange ~ pct.polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerssq <- felm(followerchange ~ polarizing_sq | icpsr + yearweek, data=tweets) )

tab_model(weeklyfollowersuncivil, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowerssq)

summary(weeklyfollowersuncivil2 <- felm(followerchange_pct ~ polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerstweets2 <- felm(followerchange_pct ~ tweets | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerspct2 <- felm(followerchange_pct ~ pct.polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerssq2 <- felm(followerchange_pct ~ polarizing_sq | icpsr + yearweek, data=tweets) )

tab_model(weeklyfollowersuncivil2, weeklyfollowerstweets2, weeklyfollowerspct2, weeklyfollowerssq2)

stargazer(weeklyfollowersuncivil, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowerssq, weeklyfollowersuncivil2, weeklyfollowerstweets2, weeklyfollowerspct2, weeklyfollowerssq2,
          Title = "Weekly Percentage Change in Twitter Followers Predicted by Twitter Activity", type="html",  out="Weekly Twitter Followers Change.htm")


followercoefs <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/follower coefs.csv")
followercoefs$coef <- followercoefs$ï..coef

followercoefs$coef <- factor(followercoefs$coef, levels = followercoefs$coef)

ggplot(followercoefs) +
  geom_pointrange( aes(x=coef, y=Followerincrease, ymin=Followerincrease_low, ymax=Followerincrease_up), colour="black", alpha=0.9, size=1.3)+
  labs(y = "Follower Percent Increase")+
  theme(axis.title.x = element_blank(), axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())+
  scale_x_discrete(labels = c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * (% Polarizing)'))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

barCenters <- barplot(followercoefs$Followerincrease, ylim=c(0, 0.02))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisivefollowerchange2.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(followercoefs$Followerincrease, ylim=c(0, 0.02), ylab='Weekly Percent Change in Twitter Followers', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=followercoefs$Followerincrease_up[ii], y1=followercoefs$Followerincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=followercoefs$Followerincrease_up[ii], y1=followercoefs$Followerincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=followercoefs$Followerincrease[ii]/4, paste0(round(followercoefs$Followerincrease[ii], 3), ' +/- ', round(followercoefs$Followerincrease_ci[ii], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

