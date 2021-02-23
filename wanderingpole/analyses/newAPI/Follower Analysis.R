library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(ggpubr)

#import data

tweets <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Donors Remerge Files/followerstweetsjoin.csv")

#remove invalid fields and anamolous drops and increases, create new variable

tweets <- filter(tweets, followerchange > -10000) %>%
  filter(followerchange_pct != Inf)
tweets <- filter(tweets, followerchange_pct > -20)
tweets <- filter(tweets, followerchange_pct < 50)
tweets %<>% mutate(polarizing_sq = polarizing * pct.polarizing)

#fixed-effects models numerical change in followers

summary(weeklyfollowerspol <- felm(followerchange ~ polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerstweets <- felm(followerchange ~ tweets | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerspct <- felm(followerchange ~ pct.polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerssq <- felm(followerchange ~ polarizing_sq | icpsr + yearweek, data=tweets) )

tab_model(weeklyfollowerspol, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowerssq)


#fixed-effects models percent change in followers

summary(weeklyfollowerspol2 <- felm(followerchange_pct ~ polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerstweets2 <- felm(followerchange_pct ~ tweets | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerspct2 <- felm(followerchange_pct ~ pct.polarizing | icpsr + yearweek, data=tweets) )
summary(weeklyfollowerssq2 <- felm(followerchange_pct ~ polarizing_sq | icpsr + yearweek, data=tweets) )

tab_model(weeklyfollowerspol2, weeklyfollowerstweets2, weeklyfollowerspct2, weeklyfollowerssq2)

stargazer(weeklyfollowerspol, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowerssq, 
          weeklyfollowerspol2, weeklyfollowerstweets2, weeklyfollowerspct2, weeklyfollowerssq2)

#extract coefficients and standard errors

pol <- coef(weeklyfollowerspol)
tw <- coef(weeklyfollowerstweets)
pct <- coef(weeklyfollowerspct)
sq <- coef(weeklyfollowerssq)

polse <- coef(summary(weeklyfollowerspol))[, 2]
twse <- coef(summary(weeklyfollowerstweets))[, 2]
pctse <- coef(summary(weeklyfollowerspct))[, 2]
sqse <- coef(summary(weeklyfollowerssq))[, 2]

Followerincrease <- c(pol, tw, pct, sq)
ses <- c(polse, twse, pctse, sqse)

followercoefs <- cbind(data.frame(Followerincrease), data.frame(ses))
followercoefs$Followerincrease <- as.numeric(followercoefs$Followerincrease)

followercoefs$Followerincrease_low <- as.numeric((followercoefs$Followerincrease - (followercoefs$ses * 1.96)))
followercoefs$Followerincrease_up <- as.numeric((followercoefss$Followerincrease + (followercoefs$ses * 1.96)))

pol2 <- coef(weeklyfollowerspol2)
tw2 <- coef(weeklyfollowerstweets2)
pct2 <- coef(weeklyfollowerspct2)
sq2 <- coef(weeklyfollowerssq2)

polse2 <- coef(summary(weeklyfollowerspol2))[, 2]
twse2 <- coef(summary(weeklyfollowerstweets2))[, 2]
pctse2 <- coef(summary(weeklyfollowerspct2))[, 2]
sqse2 <- coef(summary(weeklyfollowerssq2))[, 2]

Followerincrease2 <- c(pol2, tw2, pct2, sq2)
ses2 <- c(polse2, twse2, pctse2, sqse2)

followercoefs2 <- cbind(data.frame(Followerincrease2), data.frame(ses2))
followercoefs2$Followerincrease <- as.numeric(followercoefs2$Followerincrease)

followercoefs2$Followerincrease_low <- as.numeric((followercoefs2$Followerincrease - (followercoefs2$ses2 * 1.96)))
followercoefs2$Followerincrease_up <- as.numeric((followercoefs2$Followerincrease + (followercoefs2$ses2 * 1.96)))


#create plots

barCenters <- barplot(followercoefs$Followerincrease, ylim=c(0, 50))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Followers Remerge Files/Divisivefollowerchange.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(followercoefs$Followerincrease, ylim=c(0, 50), ylab='Weekly Numerical Change in Twitter Followers', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=followercoefs$Followerincrease_up[ii], y1=followercoefs$Followerincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=followercoefs$Followerincrease_up[ii], y1=followercoefs$Followerincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=followercoefs$Followerincrease[ii]/2, paste0(round(followercoefs$Followerincrease[ii], 2), ' +/- ', round(followercoefs$ses[ii], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()


barCenters <- barplot(followercoefs2$Followerincrease, ylim=c(0, 0.025))
pdf('C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Followers Remerge Files/Divisivefollowerchangepct.pdf', width=8, height=8)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(followercoefs2$Followerincrease, ylim=c(0, 0.025), ylab='Weekly Percent Change in Twitter Followers', names.arg=c('# Polarizing', '# Tweets', '% Polarizing', '(# Polarizing) * \n(% Polarizing)'))
for(ii in 1:4){
  segments(x0=barCenters[ii], y0=followercoefs2$Followerincrease_up[ii], y1=followercoefs2$Followerincrease_low[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=followercoefs2$Followerincrease_up[ii], y1=followercoefs2$Followerincrease_low[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=followercoefs2$Followerincrease[ii]/2, paste0(round(followercoefs2$Followerincrease[ii], 3), ' +/- ', round(followercoefs2$ses[ii], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

