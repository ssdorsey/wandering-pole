rm(list=ls())
library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)
library(lubridate)
library(Hmisc)


###Over time plot

tweets <- fread("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

tweets$created_at <- as.Date(tweets$created_at)

tweets$year <- format(as.Date(tweets$created_at, format="%y/%m/%d"),"%Y")

tweets$month <- lubridate::month(ymd(tweets$created_at))

tweets$yearmonth <- paste0(as.character(tweets$year), "/", as.character(tweets$month))


handles <- read.csv("~/Dropbox/Projects/Twitter/Twitter/US Congress Handles Master List.csv")

info <- select(handles, twitter_lower, party)

tweets <- left_join(tweets, info, by = "twitter_lower") 
##issue here with eric cantor and kevin mccarthy using gopleader account, possible to remove one by hand as they are same party? Make eric cantor gopleader twitter variable blank?

monthly <- tweets %>%
  group_by(party, yearmonth) %>%
  dplyr::summarize(
    tweets = n(),
    polarizing = sum(polarizing)
  ) %>%
  ungroup()

monthlyall <- tweets %>%
  group_by(yearmonth) %>%
  dplyr::summarize(
    tweets = n(),
    polarizing = sum(polarizing)
  ) %>%
  ungroup()

monthlyall$party <- "All"

monthly <- rbind(monthly, monthlyall)

monthly$year <- substr(monthly$yearmonth, 1, 4)

monthly$month <- substring(monthly$yearmonth, 6)

monthly$pct.polarizing <- ((monthly$polarizing/monthly$tweets) *100)
monthly <- monthly %>% 
  mutate(yearmonth=paste0(year, '-', month)) %>%
  mutate(yearmonthdate=parse_date_time(yearmonth, orders=c('Y-m'))) %>%
  mutate(yearmonthaxis=format(yearmonthdate, '%Y-%b')) %>%
  dplyr::rename(Party=party)


#Plot goes here....... (figure 2)
plotdf <- filter(monthly, Party!='I')
pdf(file='~/Dropbox/Projects/Twitter/monthlyPolarizingTweets.pdf', width=10, height=7)
par(mar=c(5,4,1,1)+0.1)
months <- sort(unique(plotdf$yearmonthdate))
monthsindex <- months[str_detect(months, '-01-01|-07-01')]
monthsaxis <- unique(plotdf$yearmonthaxis)[which(months %in% monthsindex)]
plot(months, plotdf$pct.polarizing[plotdf$Party=='D'], ylim=c(0,50), ylab='Percent Polarizing', xlab='', xaxt='n', 
     type='n', las=2)
lapply(seq(0,50,by=10), function(horiz){abline(h=horiz, lwd=0.5, lty=3, col='grey')})
lapply(monthsindex, function(vert){abline(v=vert, lwd=0.5, lty=3, col='grey')})
lines(months, plotdf$pct.polarizing[plotdf$Party=='D'], lwd=4, col='black', lty=1)
lines(months, plotdf$pct.polarizing[plotdf$Party=='R'], lwd=4, col='grey33', lty=2)
lines(months, plotdf$pct.polarizing[plotdf$Party=='All'], lwd=4, col='grey66', lty=3)
points(months, plotdf$pct.polarizing[plotdf$Party=='D'], pch=16, col='black', cex=0.8)
points(months, plotdf$pct.polarizing[plotdf$Party=='R'], pch=16, col='grey33', cex=0.8)
points(months, plotdf$pct.polarizing[plotdf$Party=='All'], pch=16, col='grey66', cex=0.8)
abline(v=as.POSIXct('2016-11-01'), col='black', lty=4, lwd=2)
axis(side=1, at=monthsindex, labels=monthsaxis, las=2)
legend(x='topleft', bty='n', legend=c('Democrats', 'Republicans', 'All Members'), col=c('black', 'grey33', 'grey66'), lty=c(1,2,3), lwd=c(3,3,3))
par(mar=c(5,4,4,2)+0.1)
dev.off()

##Table of Tweets by Year (table 3)

library(kableExtra)
twy <- monthly %>%
  group_by(year, Party) %>%
  dplyr::summarise(
    tweets=sum(tweets),
    polarizing=sum(polarizing), 
    pct.polarizing=round(polarizing/tweets, 3)
  ) %>% 
  pivot_wider(names_from=Party, values_from=c(tweets, polarizing, pct.polarizing)) %>%
  dplyr::select(c(year, tweets_D, polarizing_D, pct.polarizing_D, tweets_R, polarizing_R, pct.polarizing_R, tweets_All, polarizing_All, pct.polarizing_All)) %>%
  mutate(year=as.character(year))
kbl(twy, format='latex', align='c',
    col.names=c('Year', '# Tweets', '# Polarizing', 'Pct. Polarizing', '# Tweets', '# Polarizing', 'Pct. Polarizing', 
                '# Tweets', '# Polarizing', 'Pct. Polarizing'),
    format.args=list(big.mark=','), vline='', booktabs=TRUE, linesep='', toprule='\\hline', midrule='\\hline', bottomrule='\\hline'
)