library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)
library(lubridate)
library(Hmisc)
library(cowplot)
# library(abmisc)


#### Follower Analysis (figure 4)

tweetsfollow <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/followerstweetsjoin.csv")

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

tweets <- fread("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

#add year variable

tweets$year <- format(as.Date(tweets$created_at, format="%m/%d/%Y"),"%Y")

#create year-handle identifier

tweets$yearhandle <- paste0(as.character(tweets$twitter_lower), "-", as.character(tweets$year))

#remove API errors for likes on retweets

tweets <- tweets %>% 
  filter(substr(text, 1, 3) != "RT ")

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
effs <- sapply(list(likes, rts), function(x){
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
  text(x=barCenters[ii], y=effs[ii,1]/2, paste0(round(effs[ii,1], 1), ' +/- ', round(effs[ii,2], 1)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

########################


#yearly follower breakouts (new figure)

years <- 2010:2020
likes_mods <- vector(mode='list', length=d(years))
rts_mods <- vector(mode='list', length=d(years))
tweetsavgs %<>% mutate(years_num=num(year))

for(ii in seq_along(years)){
  mod_df <- filter(tweetsavgs, years_num==years[ii])
  likes_mod <- felm(likepct ~ polarizing | yearhandle + created_at, data = mod_df)
  rts_mod <- felm(retweetpct ~ polarizing | yearhandle + created_at, data = mod_df)
  likes_mods[[ii]] <- likes_mod
  rts_mods[[ii]] <- rts_mod
}

stargazer(likes10, likes11, likes12, likes13, likes14, likes15, likes16, likes17, 
          likes18, likes19, likes20, rts10, rts11, rts12, rts13, rts14, rts15, rts16, rts17, rts18, rts19, rts20,
          type="html",  out="Engagement Tweet Models Yearly.htm")

#### extract coefficients and errors

effs_likes <- sapply(likes_mods, function(x){
  effPlotDat(x)
}) %>% 
  t()
effs_rts <- sapply(rts_mods, function(x){
  effPlotDat(x)
}) %>% 
  t()

# Add year and CI range values to effects data
effs_likes <- as.data.frame(effs_likes) %>%
  mutate(year=years,
         ci_lo=coef-ci,
         ci_hi=coef+ci) 
effs_rts <- as.data.frame(effs_rts) %>%
  mutate(year=years,
         ci_lo=coef-ci,
         ci_hi=coef+ci)


#### plot coefficients and standard errors

# Likes
likes_plot <- ggplot(data=effs_likes, mapping=aes(year, coef)) + 
  geom_point(size=3) +
  geom_smooth(method='lm', se=FALSE, color='black', linetype='dashed') +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), width=0.3, size=1) +
  ylab('Effect of Polarizing Rhetoric') +
  xlab('') +
  ggtitle('Likes above Average') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(breaks=seq(0, 140, 20)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x=element_blank())

# Retweets
rts_plot <- ggplot(data=effs_rts, mapping=aes(year, coef)) + 
  geom_point(size=3) +
  geom_smooth(method='lm', se=FALSE, color='black', linetype='dashed') +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), width=0.3, size=1) +
  ylab('Effect of Polarizing Rhetoric') +
  xlab('') +
  ggtitle('Retweets above Average') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(breaks=seq(0, 140, 20)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x=element_blank())

# Plot together
