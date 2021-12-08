###########################################################################################################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls());gc()
library(data.table)
library(tidyverse)
library(magrittr)
library(abmisc)
library(lubridate)
library(kableExtra)
library(tidyr)

### Load and format tweets data

# Load
tweets <- fread("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv", data.table=FALSE)

# Format dates
tweets$created_at <- as.Date(tweets$created_at)
tweets$year <- format(as.Date(tweets$created_at, format="%y/%m/%d"),"%Y")
tweets$month <- lubridate::month(ymd(tweets$created_at))
tweets$yearmonth <- paste0(as.character(tweets$year), "/", as.character(tweets$month))

# Add party data
handles <- read.csv("~/Dropbox/Projects/Twitter/Twitter/US Congress Handles Master List.csv")
info <- dplyr::select(handles, twitter_lower, party)
tweets <- left_join(tweets, info, by = "twitter_lower") 
##issue here with eric cantor and kevin mccarthy using gopleader account, possible to remove one by hand as they are same party? Make eric cantor gopleader twitter variable blank?

# Filter out missing observations
tweets %<>% filter(!is.na(uncivil2))

### Group behavior by month

# By party
monthly <- tweets %>%
  group_by(party, yearmonth) %>%
  dplyr::summarize(
    tweets = n(),
    uncivil = sum(uncivil2)
  ) %>%
  ungroup()

# All members
monthlyall <- tweets %>%
  group_by(yearmonth) %>%
  dplyr::summarize(
    tweets = n(),
    uncivil = sum(uncivil2)
  ) %>%
  ungroup()
monthlyall$party <- "All"

# Join
monthly <- rbind(monthly, monthlyall)

# Format dates
monthly$year <- substr(monthly$yearmonth, 1, 4)
monthly$month <- substring(monthly$yearmonth, 6)

monthly$pct.uncivil <- ((monthly$uncivil/monthly$tweets) *100)
monthly <- monthly %>% 
  mutate(yearmonth=paste0(year, '-', month)) %>%
  mutate(yearmonthdate=parse_date_time(yearmonth, orders=c('Y-m'))) %>%
  mutate(yearmonthaxis=format(yearmonthdate, '%Y-%b')) %>%
  dplyr::rename(Party=party)


#Plot goes here....... (figure 2)
plotdf <- filter(monthly, Party!='I')
pdf(file='~/Dropbox/Projects/Twitter/monthlyUncivilTweets.pdf', width=10, height=7)
par(mar=c(5,4,1,1)+0.1)
months <- sort(unique(plotdf$yearmonthdate))
monthsindex <- months[str_detect(months, '-01-01|-07-01')]
monthsaxis <- unique(plotdf$yearmonthaxis)[which(months %in% monthsindex)]
plot(months, plotdf$pct.uncivil[plotdf$Party=='D'], ylim=c(0,15), ylab='Percent Uncivil', xlab='', xaxt='n', 
     type='n', las=2)
lapply(seq(0,50,by=10), function(horiz){abline(h=horiz, lwd=0.5, lty=3, col='grey')})
lapply(monthsindex, function(vert){abline(v=vert, lwd=0.5, lty=3, col='grey')})
lines(months, plotdf$pct.uncivil[plotdf$Party=='D'], lwd=4, col='black', lty=1)
lines(months, plotdf$pct.uncivil[plotdf$Party=='R'], lwd=4, col='grey33', lty=2)
lines(months, plotdf$pct.uncivil[plotdf$Party=='All'], lwd=4, col='grey66', lty=3)
points(months, plotdf$pct.uncivil[plotdf$Party=='D'], pch=16, col='black', cex=0.8)
points(months, plotdf$pct.uncivil[plotdf$Party=='R'], pch=16, col='grey33', cex=0.8)
points(months, plotdf$pct.uncivil[plotdf$Party=='All'], pch=16, col='grey66', cex=0.8)
abline(v=as.POSIXct('2016-11-01'), col='black', lty=4, lwd=2)
axis(side=1, at=monthsindex, labels=monthsaxis, las=2)
legend(x='topleft', bty='n', legend=c('Democrats', 'Republicans', 'All Members'), col=c('black', 'grey33', 'grey66'), lty=c(1,2,3), lwd=c(3,3,3))
par(mar=c(5,4,4,2)+0.1)
dev.off()

##Table of Tweets by Year (table 3)

twy <- monthly %>%
  group_by(year, Party) %>%
  dplyr::summarise(
    tweets=sum(tweets),
    uncivil=sum(uncivil), 
    pct.uncivil=round(uncivil/tweets, 3)
  ) %>% 
  pivot_wider(names_from=Party, values_from=c(tweets, uncivil, pct.uncivil)) %>%
  dplyr::select(c(year, tweets_D, uncivil_D, pct.uncivil_D, tweets_R, uncivil_R, pct.uncivil_R, tweets_All, uncivil_All, pct.uncivil_All)) %>%
  mutate(year=as.character(year)) %>%
  filter(year != '2021')
kbl(twy, format='latex', align='c',
    col.names=c('Year', '# Tweets', '# Uncivil', 'Pct. Uncivil', '# Tweets', '# Uncivil', 'Pct. Uncivil', 
                '# Tweets', '# Uncivil', 'Pct. Uncivil'),
    format.args=list(big.mark=','), vline='', booktabs=TRUE, linesep='', toprule='\\hline', midrule='\\hline', bottomrule='\\hline'
)
