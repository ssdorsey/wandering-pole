library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)
library(lubridate)
library(Hmisc)


#### Follower Analysis (figure 4)

tweetsfollow <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/followerstweetsjoin.csv")

tweetsfollow  <- filter(tweetsfollow, followerchange > -10000)
tweetsfollow  <- filter(tweetsfollow, icpsr.x != "NA")
tweetsfollow$divisivepp <- (tweetsfollow$divisive/tweetsfollow$tweets)*100
tweetsfollow <- filter(tweetsfollow, followerchange_pct > -20)
tweetsfollow <- filter(tweetsfollow, followerchange_pct < 50)

summary(weeklyfollowersdivisive <- felm(followerchange ~ divisive | icpsr.x + yearweek, data=tweetsfollow) )
summary(weeklyfollowerstweets <- felm(followerchange ~ tweets | icpsr.x + yearweek, data=tweetsfollow) )
summary(weeklyfollowerspct <- felm(followerchange ~ divisivepp | icpsr.x + yearweek, data=tweetsfollow) )

tab_model(weeklyfollowersdivisive, weeklyfollowerstweets, weeklyfollowerspct)

summary(weeklyfollowersdivisive2 <- felm(followerchange_pct ~ divisive | icpsr.x + yearweek, data=tweetsfollow) )
summary(weeklyfollowerstweets2 <- felm(followerchange_pct ~ tweets | icpsr.x + yearweek, data=tweetsfollow) )
summary(weeklyfollowerspct2 <- felm(followerchange_pct ~ divisivepp | icpsr.x + yearweek, data=tweetsfollow) )

tab_model(weeklyfollowersdivisive2, weeklyfollowerstweets2, weeklyfollowerspct2)

stargazer(weeklyfollowersdivisive, weeklyfollowerstweets, weeklyfollowerspct, weeklyfollowersdivisive2, weeklyfollowerstweets2, weeklyfollowerspct2,
          Title = "Weekly Percentage Change in Twitter Followers Predicted by Twitter Activity", type="html",  out="Weekly Twitter Followers Change.htm")

#### extract coefficients and errors

#### plot coefficients

########################

#### Enagement Analysis (figure 3)

tweets <- fread("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

#add year variable

tweets$year <- format(as.Date(tweets$created_at, format="%m/%d/%Y"),"%Y")

#create year-handle identifier

tweets$yearhandle <- paste0(as.character(tweets$twitter_lower), "-", as.character(tweets$year))

#remove API errors for likes on retweets

tweets <- filter(tweets, public_metrics.like_count > 0)

#create year-name like and retweet averages

avgs <- tweets %>%
  group_by(yearhandle) %>%
  dplyr::summarize(
    likeavg = mean(public_metrics.like_count),
    retweetavg = mean(public_metrics.retweet_count)
  ) %>%
  ungroup()

#merge averages

tweetsavgs <- merge(tweets, avgs, by = "yearhandle")

#create variable for differences

tweetsavgs$likediff <- (tweetsavgs$public_metrics.like_count - tweetsavgs$likeavg)
tweetsavgs$retweetdiff <- (tweetsavgs$public_metrics.retweet_count - tweetsavgs$retweetavg)

#create variable for percent difference

tweetsavgs$likepct <- ((tweetsavgs$public_metrics.like_count - tweetsavgs$likeavg) / tweetsavgs$likeavg) * 100
tweetsavgs$retweetpct <- ((tweetsavgs$public_metrics.retweet_count - tweetsavgs$retweetavg) / tweetsavgs$retweetavg) * 100

#run regressions

likes <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweetsavgs)

rts <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweetsavgs)

stargazer(likes, rts,
          type="html",  out="Engagement Tweet Models.htm")

#### extract coefficients and errors

effPlotDat <- function(model){
  # Pull coefficient
  coefs <- summary(model)$coefficients
  coef <- coefs['polarizing','Estimate']
  # Pull CI
  ci <- coefs['polarizing', 'Std. Error']*1.96
  # Combine/return
  effs <- c(coef=coef, ci=ci)
  effs
}
effs <- sapply(list(mFave, mRT), function(x){
  effPlotDat(x)
}) %>% 
  t()

#### plot coefficients

barCenters <- barplot(effs[,1], ylim=c(0, 250))
pdf('~/Dropbox/Projects/Twitter/engageMods_effects_polarizing.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs[,1], ylim=c(0, 250), ylab='Effect of Polarizing Rhetoric', names.arg=c('Likes above\nAverage', 'Retweets above\nAverage'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs[ii,1]/2, paste0(round(effs[ii,1], 2), ' +/- ', round(effs[ii,2], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

########################


#yearly follower breakouts (new figure)

tweets10 <- filter(tweetsavgs, year == "2010")
tweets11 <- filter(tweetsavgs, year == "2011")
tweets12 <- filter(tweetsavgs, year == "2012")
tweets13 <- filter(tweetsavgs, year == "2013")
tweets14 <- filter(tweetsavgs, year == "2014")
tweets15 <- filter(tweetsavgs, year == "2015")
tweets16 <- filter(tweetsavgs, year == "2016")
tweets17 <- filter(tweetsavgs, year == "2017")
tweets18 <- filter(tweetsavgs, year == "2018")
tweets19 <- filter(tweetsavgs, year == "2019")
tweets20 <- filter(tweetsavgs, year == "2020")

likes10 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets10)
likes11 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets11)
likes12 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets12)
likes13 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets13)
likes14 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets14)
likes15 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets15)
likes16 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets16)
likes17 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets17)
likes18 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets18)
likes19 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets19)
likes20 <- felm(likepct ~ polarizing | yearhandle + created_at, data = tweets20)

rts10 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets10)
rts11 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets11)
rts12 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets12)
rts13 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets13)
rts14 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets14)
rts15 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets15)
rts16 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets16)
rts17 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets17)
rts18 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets18)
rts19 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets19)
rts20 <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = tweets20)

stargazer(likes10, likes11, likes12, likes13, likes14, likes15, likes16, likes17, 
          likes18, likes19, likes20, rts10, rts11, rts12, rts13, rts14, rts15, rts16, rts17, rts18, rts19, rts20,
          type="html",  out="Engagement Tweet Models.htm")

#### extract coefficients and errors

#### plot coefficients (perhaps just a bar chart, or maybe a line graph with two lines for likes and retweets, with year as running variable)
