library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)

#import data

tweetsdonorsmonth <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/tweets_donations_month.csv")
tweetsdonorsweek <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/tweets_donations_week.csv")

#convert weeks with no donations to zeros

tweetsdonorsmonth$polarizing[is.na(tweetsdonorsmonth$polarizing)] <- 0
tweetsdonorsmonth$tweets[is.na(tweetsdonorsmonth$tweets)] <- 0

tweetsdonorsmonth$polarizing_sq <- ((tweetsdonorsmonth$polarizing * tweetsdonorsmonth$polarizing) / tweetsdonorsmonth$tweets)

tweetsdonorsweek$polarizing[is.na(tweetsdonorsweek$polarizing)] <- 0
tweetsdonorsweek$tweets[is.na(tweetsdonorsweek$tweets)] <- 0

tweetsdonorsweek$polarizing_sq <- ((tweetsdonorsweek$polarizing * tweetsdonorsweek$polarizing) / tweetsdonorsweek$tweets)


#monthly fixed effects models for amount donated and number of donors

summary(monthamountpp <- felm(monthamount ~ pct.polarizing | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))
summary(monthamountcount <- felm(monthamount ~ polarizing  | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))
summary(monthamounttweets <- felm(monthamount ~ tweets | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))
summary(monthamountsq <- felm(monthamount ~ polarizing_sq | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))

summary(monthdonorspp <- felm(monthdonors ~ pct.polarizing | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))
summary(monthdonorscount <- felm(monthdonors ~ polarizing  | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))
summary(monthdonorstweets <- felm(monthdonors ~ tweets | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))
summary(monthdonorssq <- felm(monthdonors ~ polarizing_sq | icpsr.x + yearmonth.x, data=tweetsdonorsmonth))

stargazer(monthamountpp, monthamountcount, monthamounttweets, monthamountsq, monthdonorspp, 
          monthdonorscount, monthdonorstweets, monthdonorssq)

#weekly fixed effects models for amount donated and number of donors

summary(weekamountpp <- felm(weekamount ~ pct.polarizing | icpsr.x + yearweek.x, data=tweetsdonorsweek))
summary(weekamountcount <- felm(weekamount ~ polarizing  | icpsr.x + yearweek.x, data=tweetsdonorsweek))
summary(weekamounttweets <- felm(weekamount ~ tweets | icpsr.x + yearweek.x, data=tweetsdonorsweek))
summary(weekamountsq <- felm(weekamount ~ polarizing_sq | icpsr.x + yearweek.x, data=tweetsdonorsweek))

summary(weekdonorspp <- felm(weekdonors ~ pct.polarizing | icpsr.x + yearweek.x, data=tweetsdonorsweek))
summary(weekdonorscount <- felm(weekdonors ~ polarizing  | icpsr.x + yearweek.x, data=tweetsdonorsweek))
summary(weekdonorstweets <- felm(weekdonors ~ tweets | icpsr.x + yearweek.x, data=tweetsdonorsweek))
summary(weekdonorssq <- felm(weekdonors ~ polarizing_sq | icpsr.x + yearweek.x, data=tweetsdonorsweek))

stargazer(weekamountpp, weekamountcount, weekamounttweets, weekamountsq, weekdonorspp, 
          weekdonorscount, weekdonorstweets, weekdonorssq)

#extract month coefficients and standard errors

mamountpct <- coef(monthamountpp)
mamountpol <- coef(monthamountcount)
mamounttw <- coef(monthamounttweets)
mamountsq <- coef(monthamountsq)

mamountpctse <- coef(summary(monthamountpp))[, 2]
mamountpolse <- coef(summary(monthamountcount))[, 2]
mamounttwse <- coef(summary(monthamounttweets))[, 2]
mamountsqse <- coef(summary(monthamountsq))[, 2]

mdonorspct <- coef(monthdonorspp)
mdonorspol <- coef(monthdonorscount)
mdonorstw <- coef(monthdonorstweets)
mdonorssq <- coef(monthdonorssq)

mdonorspctse <- coef(summary(monthdonorspp))[, 2]
mdonorspolse <- coef(summary(monthdonorscount))[, 2]
mdonorstwse <- coef(summary(monthdonorstweets))[, 2]
mdonorssqse <- coef(summary(monthdonorssq))[, 2]

#extract week coefficients and standard errors

wamountpct <- coef(weekamountpp)
wamountpol <- coef(weekamountcount)
wamounttw <- coef(weekamounttweets)
wamountsq <- coef(weekamountsq)

wamountpctse <- coef(summary(weekamountpp))[, 2]
wamountpolse <- coef(summary(weekamountcount))[, 2]
wamounttwse <- coef(summary(weekamounttweets))[, 2]
wamountsqse <- coef(summary(weekamountsq))[, 2]

wdonorspct <- coef(weekdonorspp)
wdonorspol <- coef(weekdonorscount)
wdonorstw <- coef(weekdonorstweets)
wdonorssq <- coef(weekdonorssq)

wdonorspctse <- coef(summary(weekdonorspp))[, 2]
wdonorspolse <- coef(summary(weekdonorscount))[, 2]
wdonorstwse <- coef(summary(weekdonorstweets))[, 2]
wdonorssqse <- coef(summary(weekdonorssq))[, 2]

#create coefficients and confidence intervals dataframe

amountincrease <- c(mamountpol, mamounttw, mamountpct, mamountsq)
ses <- c(mamountpolse, mamounttwse, mamountpctse, mamountsqse)

monthamountcoefs <- cbind(data.frame(amountincrease), data.frame(ses))
monthamountcoefs$amountincrease <- as.numeric(monthamountcoefs$amountincrease)

monthamountcoefs$amountincrease_ci <- as.numeric(monthamountcoefs$ses * 1.96)
monthamountcoefs$amountincrease_low <- as.numeric((monthamountcoefs$amountincrease - (monthamountcoefs$ses * 1.96)))
monthamountcoefs$amountincrease_up <- as.numeric((monthamountcoefs$amountincrease + (monthamountcoefs$ses * 1.96)))

donorincrease <- c(mdonorspol, mdonorstw, mdonorspct, mdonorssq)
ses <- c(mdonorspolse, mdonorstwse, mdonorspctse, mdonorssqse)

monthdonorcoefs <- cbind(data.frame(donorincrease), data.frame(ses))
monthdonorcoefs$donorincrease <- as.numeric(monthdonorcoefs$donorincrease)

monthdonorcoefs$donorincrease_ci <- as.numeric(monthdonorcoefs$ses * 1.96)
monthdonorcoefs$donorincrease_low <- as.numeric((monthdonorcoefs$donorincrease - (monthdonorcoefs$ses * 1.96)))
monthdonorcoefs$donorincrease_up <- as.numeric((monthdonorcoefs$donorincrease + (monthdonorcoefs$ses * 1.96)))


amountincrease <- c(wamountpol, wamounttw, wamountpct, wamountsq)
ses <- c(wamountpolse, wamounttwse, wamountpctse, wamountsqse)

weekamountcoefs <- cbind(data.frame(amountincrease), data.frame(ses))
weekamountcoefs$amountincrease <- as.numeric(weekamountcoefs$amountincrease)

weekamountcoefs$amountincrease_ci <- as.numeric(weekamountcoefs$ses * 1.96)
weekamountcoefs$amountincrease_low <- as.numeric((weekamountcoefs$amountincrease - (weekamountcoefs$ses * 1.96)))
weekamountcoefs$amountincrease_up <- as.numeric((weekamountcoefs$amountincrease + (weekamountcoefs$ses * 1.96)))

donorincrease <- c(wdonorspol, wdonorstw, wdonorspct, wdonorssq)
ses <- c(wdonorspolse, wdonorstwse, wdonorspctse, wdonorssqse)

weekdonorcoefs <- cbind(data.frame(donorincrease), data.frame(ses))
weekdonorcoefs$donorincrease <- as.numeric(weekdonorcoefs$donorincrease)

weekdonorcoefs$donorincrease_ci <- as.numeric(weekdonorcoefs$ses * 1.96)
weekdonorcoefs$donorincrease_low <- as.numeric((weekdonorcoefs$donorincrease - (weekdonorcoefs$ses * 1.96)))
weekdonorcoefs$donorincrease_up <- as.numeric((weekdonorcoefs$donorincrease + (weekdonorcoefs$ses * 1.96)))


#create plots

barCenters <- barplot(monthamountcoefs$amountincrease, ylim=c(-100, 500))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/PolarizingMonthAmount.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(monthamountcoefs$amountincrease, ylim=c(-100, 500), ylab='Monthly Fundraising Amount Increase', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=monthamountcoefs$amountincrease_up[ii], y1=monthamountcoefs$amountincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=monthamountcoefs$amountincrease_up[ii], y1=monthamountcoefs$amountincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=monthamountcoefs$amountincrease[ii]/4, paste0(round(monthamountcoefs$amountincrease[ii], 2), ' +/- ', round(monthamountcoefs$amountincrease_ci[ii], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

barCenters <- barplot(monthdonorcoefs$donorincrease, ylim=c(-0.5, 2.5))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/PolarizingMonthDonors.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(monthdonorcoefs$donorincrease, ylim=c(-0.5, 3), ylab='Monthly Donors Increase', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=monthdonorcoefs$donorincrease_up[ii], y1=monthdonorcoefs$donorincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=monthdonorcoefs$donorincrease_up[ii], y1=monthdonorcoefs$donorincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=monthdonorcoefs$donorincrease[ii]/4, paste0(round(monthdonorcoefs$donorincrease[ii], 2), ' +/- ', round(monthdonorcoefs$donorincrease_ci[ii], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()



barCenters <- barplot(weekamountcoefs$amountincrease, ylim=c(-50, 300))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/PolarizingWeekAmount.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(weekamountcoefs$amountincrease, ylim=c(-50, 300), ylab='Weekly Fundraising Amount Increase', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=weekamountcoefs$amountincrease_up[ii], y1=weekamountcoefs$amountincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=weekamountcoefs$amountincrease_up[ii], y1=weekamountcoefs$amountincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=weekamountcoefs$amountincrease[ii]/4, paste0(round(weekamountcoefs$amountincrease[ii], 2), ' +/- ', round(weekamountcoefs$amountincrease_ci[ii], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

barCenters <- barplot(weekdonorcoefs$donorincrease, ylim=c(-0.5, 2))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/PolarizingWeekDonors.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(weekdonorcoefs$donorincrease, ylim=c(-0.5, 2), ylab='Weekly Donors Increase', names.arg=c('# Tweets', '# Polarizing', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=weekdonorcoefs$donorincrease_up[ii], y1=weekdonorcoefs$donorincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=weekdonorcoefs$donorincrease_up[ii], y1=weekdonorcoefs$donorincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=weekdonorcoefs$donorincrease[ii]/4, paste0(round(weekdonorcoefs$donorincrease[ii], 2), ' +/- ', round(weekdonorcoefs$donorincrease_ci[ii], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()
