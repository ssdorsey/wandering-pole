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

tweets <- fread("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

alltweets$created_at <- as.Date(alltweets$created_at)

tweets$year <- format(as.Date(tweets$created_at, format="%y/%m/%d"),"%Y")

tweets$month <- lubridate::month(ymd(tweets$created_at))

tweets$yearmonth <- paste0(as.character(tweets$year), "/", as.character(tweets$month))


handles <- read.csv("C:/Users/User/Dropbox/Twitter/US Congress Handles Master List.csv")

info <- select(handles, twitter_lower, party)

tweets <- merge(tweets, info, by = "twitter_lower") 
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


#Plot goes here....... (figure 2)


##Table of Tweets by Year (table 3)

library(kableExtra)
twy <- monthlyall %>%
  group_by(year, party) %>%
  dplyr::summarise(
    tweets=sum(tweets),
    polarizing=sum(polarizing), 
    pct.polarizing=round(polarizing/tweets, 3)
  ) %>% 
  pivot_wider(names_from=party, values_from=c(tweets, polarizing, pct.polarizing)) %>%
  dplyr::select(c(year, tweets_D, polarizing_D, pct.polarizing_D, tweets_R, polarizing_R, pct.polarizing_R, tweets_All, polarizing_All, pct.polarizing_All)) %>%
  mutate(year=as.character(year))
kbl(twy, format='latex', align='c',
    col.names=c('Year', '# Tweets', '# Polarizing', 'Pct. Polarizing', '# Tweets', '# Polarizing', 'Pct. Polarizing', 
                '# Tweets', '# Polarizing', 'Pct. Polarizing'),
    format.args=list(big.mark=','), vline='', booktabs=TRUE, linesep='', toprule='\\hline', midrule='\\hline', bottomrule='\\hline'
)