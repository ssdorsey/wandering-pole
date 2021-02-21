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
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load monthly tweets data
mut <- read_csv('monthlyPolarizingTweets_newAPI.csv') %>%
  as.data.frame()

# Format names
names(mut) <- tolower(names(mut))
names(mut)[names(mut)=='party'] <- 'Party'

# Reformat date
mut %<>% mutate(month=as.character(month))
mut$month[nchar(mut$month)==1] <- paste0('0', mut$month[nchar(mut$month)==1])
mut %<>% mutate(yearmonth=paste0(year, '-', month)) %>%
  mutate(yearmonthdate=parse_date_time(yearmonth, orders=c('Y-m'))) %>%
  mutate(yearmonthaxis=format(yearmonthdate, '%Y-%b')) %>%
  dplyr::rename(pct.polarizing=`percent polarizing`) %>%
  mutate(pct.polarizing=pct.polarizing*100) 



#######################################################################################################################################################################
### Plot of incivility by party over time

pdf(file='~/Dropbox/Projects/Twitter/monthlyPolarizingTweets.pdf', width=10, height=7)
par(mar=c(5,4,1,1)+0.1)
months <- sort(unique(mut$yearmonthdate))
monthsindex <- months[str_detect(months, '-01-01|-07-01')]
monthsaxis <- unique(mut$yearmonthaxis)[which(months %in% monthsindex)]
plot(months, mut$pct.polarizing[mut$Party=='D'], ylim=c(0,50), ylab='Percent Polarizing', xlab='', xaxt='n', 
     type='n', las=2)
lapply(seq(0,50,by=10), function(horiz){abline(h=horiz, lwd=0.5, lty=3, col='grey')})
lapply(monthsindex, function(vert){abline(v=vert, lwd=0.5, lty=3, col='grey')})
lines(months, mut$pct.polarizing[mut$Party=='D'], lwd=4, col='black', lty=1)
lines(months, mut$pct.polarizing[mut$Party=='R'], lwd=4, col='grey33', lty=2)
lines(months, mut$pct.polarizing[mut$Party=='All'], lwd=4, col='grey66', lty=3)
points(months, mut$pct.polarizing[mut$Party=='D'], pch=16, col='black', cex=0.8)
points(months, mut$pct.polarizing[mut$Party=='R'], pch=16, col='grey33', cex=0.8)
points(months, mut$pct.polarizing[mut$Party=='All'], pch=16, col='grey66', cex=0.8)
abline(v=as.POSIXct('2016-11-01'), col='black', lty=4, lwd=2)
axis(side=1, at=monthsindex, labels=monthsaxis, las=2)
legend(x='topleft', bty='n', legend=c('Democrats', 'Republicans', 'All Members'), col=c('black', 'grey33', 'grey66'), lty=c(1,2,3), lwd=c(3,3,3))
par(mar=c(5,4,4,2)+0.1)
dev.off()



######################################################################################################################################################################
### Table of tweets and uncivil tweets by year


### Data for table
twy <- mut %>%
  group_by(year, Party) %>%
  dplyr::summarise(
    tweets=sum(tweets),
    polarizing=sum(polarizing), 
    pct.polarizing=round(polarizing/tweets, 3)
  ) %>% 
  pivot_wider(names_from=Party, values_from=c(tweets, polarizing, pct.polarizing)) %>%
  dplyr::select(c(year, tweets_D, polarizing_D, pct.polarizing_D, tweets_R, polarizing_R, pct.polarizing_R, tweets_All, polarizing_All, pct.polarizing_All)) %>%
  mutate(year=as.character(year)) %>%
  filter(!year %in% c('2009', '2019'))


### Print table
kbl(twy, format='latex', align='c',
    col.names=c('Year', '# Tweets', '# Polarizing', 'Pct. Polarizing', '# Tweets', '# Polarizing', 'Pct. Polarizing', 
                '# Tweets', '# Polarizing', 'Pct. Polarizing'),
    format.args=list(big.mark=','), vline='', booktabs=TRUE, linesep='', toprule='\\hline', midrule='\\hline', bottomrule='\\hline'
    )


### Summary data for main text

# Number of tweets
sum(twy$tweets_All)

# Proportion polarizing
sum(twy$polarizing_All)/sum(twy$tweets_All)

# Number polarizing
sum(twy$polarizing_All)

# Polarizing margin between parties 2010-16
twy %>%
  filter(!year %in% c('2017', '2018')) %>%
  mutate(diff = pct.polarizing_R - pct.polarizing_D) %>%
  pull(diff) %>%
  summary

# Dem % change 2010-16 to 2017-18
twy$tweets_D[twy$year=='2017']/twy$tweets_D[twy$year=='2016']
filter(twy, !year %in% c('2017', '2018')) %>% pull(pct.polarizing_D) %>% summary()

# Rep % change 2010-16 to 2017-18
twy$tweets_R[twy$year=='2017']/twy$tweets_R[twy$year=='2016']
twy$tweets_R[twy$year=='2018']/twy$tweets_R[twy$year=='2017']
filter(twy, !year %in% c('2017', '2018')) %>% pull(pct.polarizing_R) %>% summary()
filter(twy, year %in% c('2017', '2018')) %>% pull(pct.polarizing_R) %>% summary()
