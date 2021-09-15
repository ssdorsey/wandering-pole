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
setwd('~/Dropbox/Projects/Twitter')

# Load data with covariates merged
mut <- read.csv('Twitter/monthlyUncivilTweets.csv', stringsAsFactors = FALSE)

# Format names
names(mut) <- tolower(names(mut))
mut %<>% dplyr::rename(Party=party)

# Reformat date
mut %<>% mutate(month=as.character(month))
mut$month[nchar(mut$month)==1] <- paste0('0', mut$month[nchar(mut$month)==1])
mut %<>% mutate(yearmonth=paste0(year, '-', month)) %>%
  mutate(yearmonthdate=parse_date_time(yearmonth, orders=c('Y-m'))) %>%
  mutate(yearmonthaxis=format(yearmonthdate, '%Y-%b'))

# Fill in NAs
mut$uncivil[is.na(mut$uncivil)] <- 0
mut$percent.uncivil[is.na(mut$percent.uncivil)] <- 0

# Multiply percentages by 100 so they're all actually percentages
mut %<>% mutate(percent.uncivil=percent.uncivil*100)



#######################################################################################################################################################################
### Plot of incivility by party over time

# monthplot <- ggplot(data = plotdf, aes(x = yearmonth, y = pct.uncivil)) +
#   geom_point(color = "black", size = 1.5) +
#   geom_line(mapping = (aes(group = Party, color = Party)), lwd=1.5) +
#   labs(title = "") + ylab("Percent Uncivil") + xlab("Month") +
#   scale_color_manual(breaks = c("R", "D", "All"),
#                      values=c("grey33", "grey66", "black")) +
#   geom_vline(xintercept = "2016-11", linetype="twodash",
#              color = "black", size=1) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(axis.text.x = element_text(angle=90, vjust=.5)) +
#   ylim(0,20)
# 
# ggsave(filename='~/Dropbox/Projects/Twitter/monthlyUncivilTweets.pdf', plot=monthplot, width=15, height=10)

pdf(file='~/Dropbox/Projects/Twitter/monthlyUncivilTweets.pdf', width=10, height=7)
par(mar=c(5,4,1,1)+0.1)
months <- sort(unique(mut$yearmonthdate))
monthsindex <- months[str_detect(months, '-01-01|-07-01')]
monthsaxis <- unique(mut$yearmonthaxis)[which(months %in% monthsindex)]
plot(months, mut$percent.uncivil[mut$Party=='D'], ylim=c(0,16), ylab='Percent Uncivil', xlab='', xaxt='n', 
     type='n', las=2)
lapply(seq(0,20,by=5), function(horiz){abline(h=horiz, lwd=0.5, lty=3, col='grey')})
lapply(monthsindex, function(vert){abline(v=vert, lwd=0.5, lty=3, col='grey')})
lines(months, mut$percent.uncivil[mut$Party=='D'], lwd=4, col='grey33', lty=2)
lines(months, mut$percent.uncivil[mut$Party=='R'], lwd=4, col='grey66', lty=3)
lines(months, mut$percent.uncivil[mut$Party=='All'], lwd=4, col='black', lty=1)
points(months, mut$percent.uncivil[mut$Party=='D'], pch=16, col='grey33', cex=0.8)
points(months, mut$percent.uncivil[mut$Party=='R'], pch=16, col='grey66', cex=0.8)
points(months, mut$percent.uncivil[mut$Party=='All'], pch=16, col='black', cex=0.8)
abline(v=as.POSIXct('2016-11-01'), col='black', lty=4, lwd=2)
axis(side=1, at=monthsindex, labels=monthsaxis, las=2)
legend(x='topleft', bty='n', legend=c('Democrats', 'Republicans', 'All Members'), col=c('grey33', 'grey66', 'black'), lty=c(2,3,1), lwd=c(3,3,3))
par(mar=c(5,4,4,2)+0.1)
dev.off()



######################################################################################################################################################################
### Table of tweets and uncivil tweets by year

twy <- mut %>%
  group_by(year, Party) %>%
  dplyr::summarise(
    tweets=sum(tweets),
    uncivil=sum(uncivil), 
    pct.uncivil=round(uncivil/tweets, 3)
  ) %>% 
  pivot_wider(names_from=Party, values_from=c(tweets, uncivil, pct.uncivil)) %>%
  dplyr::select(c(year, tweets_D, uncivil_D, pct.uncivil_D, tweets_R, uncivil_R, pct.uncivil_R, tweets_All, uncivil_All, pct.uncivil_All)) %>%
  mutate(year=as.character(year))
kbl(twy, format='latex', align='c',
    col.names=c('Year', '# Tweets', '# Uncivil', 'Pct. Uncivil', '# Tweets', '# Uncivil', 'Pct. Uncivil', 
                '# Tweets', '# Uncivil', 'Pct. Uncivil'),
    format.args=list(big.mark=','), vline='', booktabs=TRUE, linesep='', toprule='\\hline', midrule='\\hline', bottomrule='\\hline'
)
