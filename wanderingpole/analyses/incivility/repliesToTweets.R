###########################################################################################################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls());gc()
library(data.table)
library(tidyverse)
library(magrittr)
library(abmisc)
library(lubridate)
library(rjson)
library(sjPlot)
library(speedglm)
library(prediction)
library(coefplot)
library(jsonlite)
library(ndjson)
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load tweets data with covariates merged
#tw <- readRDS('~/Dropbox/Projects/Twitter/modelData.rds')

# Stream in the replies to tweets
# jfile <- fromJSON(file('~/Dropbox/Projects/Twitter/replies2.json'))
options(scipen=999) # Keep R from converting large numbers (like dates and ids in tweet JSON) to scientific notation
jfile <- ndjson::stream_in('~/Dropbox/Projects/Twitter/replies2.json') %>%
  as.data.frame()


### Need to collapse the columns that got separated

# Pull the column names in the JSON file
jnames <- names(jfile) %>% sort()

# Pull names of columns that need collapsing
multCol <- jnames[str_detect(jnames, '\\.[0-9]+.*') %>% unlist()] %>% 
  str_replace_all(., '\\.[0-9]+', '') %>% 
  unique()

# Collapse hashtags
hash <- dplyr::select(jfile, str_extract_all(jnames, 'hashtags\\.[0-9]+') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
hash %<>%
  str_replace_all(., ';*NA;*', '')
hash[hash==''] <- NA

# Collapse cashtags
cash <- dplyr::select(jfile, str_extract_all(jnames, 'cashtags\\.[0-9]+') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
cash %<>%
  str_replace_all(., 'NA;*', '') %>% 
  str_replace_all(., '^;|;$', '')
cash[cash==''] <- NA

# Collapse mentions
ment <- dplyr::select(jfile, str_extract_all(jnames, 'mentions\\.[0-9]+') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
ment %<>%
  str_replace_all(., 'NA;*', '') %>% 
  str_replace_all(., '^;|;$', '')
ment[ment==''] <- NA

# Collapse photos
phot <- dplyr::select(jfile, str_extract_all(jnames, 'photos\\.[0-9]+') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
phot %<>%
  str_replace_all(., 'NA;*', '') %>% 
  str_replace_all(., '^;|;$', '')
phot[phot==''] <- NA

# Collapse user ID of who they're replying to
replyID <- dplyr::select(jfile, str_extract_all(jnames, 'reply_to\\.[0-9]+\\.user_id') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
replyID %<>%
  str_replace_all(., 'NA;*', '') %>% 
  str_replace_all(., '^;|;$', '')
replyID[replyID==''] <- NA

# Collapse username of who they're replying to
replyUser <- dplyr::select(jfile, str_extract_all(jnames, 'reply_to\\.[0-9]+\\.username') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
replyUser %<>%
  str_replace_all(., 'NA;*', '') %>% 
  str_replace_all(., '^;|;$', '')
replyUser[replyUser==''] <- NA

# Collapse URLs
urls <- dplyr::select(jfile, str_extract_all(jnames, 'urls\\.[0-9]+') %>% unlist()) %>%
  apply(., 1, paste, collapse=';')
urls %<>%
  str_replace_all(., 'NA;*', '') %>% 
  str_replace_all(., '^;|;$', '')
urls[urls==''] <- NA

# Remove the columns in jfile with multiple columns that we've collapsed
test <- dplyr::select(jfile, -c(str_extract_all(jnames, 'urls\\.[0-9]+') %>% unlist())) %>%
  dplyr::select(., -c(str_extract_all(jnames, 'reply_to\\.[0-9]+\\.username') %>% unlist())) %>%
  dplyr::select(., -c(str_extract_all(jnames, 'reply_to\\.[0-9]+\\.user_id') %>% unlist())) %>%
  dplyr::select(., -c(str_extract_all(jnames, 'photos\\.[0-9]+') %>% unlist())) %>%
  dplyr::select(., -c(str_extract_all(jnames, 'mentions\\.[0-9]+') %>% unlist())) %>%
  dplyr::select(., -c(str_extract_all(jnames, 'cashtags\\.[0-9]+') %>% unlist())) %>%
  dplyr::select(., -c(str_extract_all(jnames, 'hashtags\\.[0-9]+') %>% unlist())) 

# Add in new columns for each of the collapsed columns
test$replyUsername <- tolower(replyUser)
test$hashtags <- hash
test$cashtags <- cash
test$mentions <- ment
test$urls <- urls
test$replyID <- tolower(replyID)

# Indicator for whether any of the replies were to a member of congress in the master file
handles <- read.csv('~/Dropbox/Projects/Twitter/master_handles.csv', stringsAsFactors=FALSE) %>%
  mutate(twitter=tolower(twitter))
replMC <- str_split(test$replyUsername, ';') %>% 
  lapply(., function(tweet){
    ifelse(any(tweet %in% handles$twitter), TRUE, FALSE)
  }) %>% 
  unlist() 
mentMC <- str_split(test$mentions, ';') %>% 
  lapply(., function(tweet){
    ifelse(any(tweet %in% handles$twitter), TRUE, FALSE)
  }) %>% 
  unlist() 
both <- data.frame(repl=replMC, ment=mentMC) %>%
  mutate(mc=ifelse(ment|repl, 1, 0))

# What's up with the missing ones?
missing <- test[which(both$mc==0),]

# Need to identify those that were actually sent by a member of Congress (I guess replying to another tweet?)
test %<>% mutate(selfRef=ifelse(username %in% handles$twitter & str_detect(replyUsername, username), 1, 0)) %>%
  mutate(mc=ifelse(tolower(username) %in% handles$twitter, 1, 0)) %>%
  mutate(root_author=tolower(root_author))

### Run a model!
# First: split by date (indicator for each week)
# This requires getting the 13 digit POSIX dates 
# Two periods: June 4-10 and October 1-7 2018
test$date <- as.POSIXct(test$date/1000, origin="1970-01-01") %>% as.Date()
test$period <- NA
test$period[test$date %within% interval('2018-06-03', '2018-06-11')] <- 0
test$period[test$date %within% interval('2018-09-30', '2018-10-08')] <- 1

# Second: cut out tweets by accounts that didn't have enough (20? 30?) tweets in each period
# What proportion is this?
p1 <- filter(test, period==0)
p2 <- filter(test, period==1)

numTweet <- function(df, thresh){
  df %>%
    group_by(root_author) %>%
    dplyr::summarise(n=n()) %>%
    ungroup() %>%
    filter(n > thresh)
}
num1 <- numTweet(p1, 10)
num2 <- numTweet(p2, 10)
num1_thresh5 <- numTweet(p1, 5)
num2_thresh5 <- numTweet(p1, 5)
numt <- numTweet(df=test, thresh=10)

# Are tweets in response to incivil tweets more likely to be incivil?
replMod1 <- glm(reply_incivil ~ root_incivil + factor(root_author), data=filter(p1, root_author %in% num1$root_author))
replMod2 <- glm(reply_incivil ~ root_incivil + factor(root_author), data=filter(p2, root_author %in% num2$root_author))
replMod1_t5 <- glm(reply_incivil ~ root_incivil + factor(root_author), data=filter(p1, root_author %in% num1_thresh5$root_author))
replMod2_t5 <- glm(reply_incivil ~ root_incivil + factor(root_author), data=filter(p2, root_author %in% num2_thresh5$root_author))
replMod <- glm(reply_incivil ~ root_incivil*period + factor(root_author), data=filter(test, root_author %in% numt$root_author))

coefs <- summary(replMod)$coefficients
coefs[,4] <- round(coefs[,4], 6)
coefs[!str_detect(rownames(coefs), 'factor'),]

plot_model(replMod, type='eff', terms=c('root_incivil', 'period'))

### Need to just do descriptives
# Proportion of incivil replies to civil and incivil tweets by period
resp <- roots %>%
  group_by(period, root_incivil) %>%
  dplyr::summarise(
    n=n(),
    incivil=table(reply_incivil)['1'],
    incivProp=incivil/n
  ) %>% 
  ungroup()
 

# Predicted probabilities by hand
roots <- filter(test, root_author %in% numt$root_author)
predvals <- expand.grid(incivility=c(0,1), period=c(0,1))
draw <- mvrnorm(1000, replMod$coefficients, vcov(replMod)) %>%
  .[,!str_detect(colnames(.), 'factor')]

predmat <- matrix(NA, nrow=nrow(draw), ncol=nrow(predvals)) #build matrix instead of vector, columns = length.out and rows = number of draws
for(jj in 1:nrow(draw)){
  predvec <- NULL
  for(ii in 1:nrow(predvals)){
      values <- c(1, predvals$incivility[ii], predvals$period[ii], predvals$incivility[ii]*predvals$period[ii])
      pred <- draw[jj,] %*% values
      predvec <- c(predvec, pred)
  }
  predmat[jj,] <- 1/(1+exp(-predvec)) #store predvecs in the null matrix you made
}

# 1. spagetti plot	
plot(lwgvec, predprob, type="n", ylim=c(0,1) ) #just the line, predprob has no uncertainty
for(i in 1:1000){ 
  points(lwgvec, predmat[i,], type="l", lwd=0.2, col="grey") #plots simulated uncertainty from predmat about the line for predprob
}
points(lwgvec, predprob, type="l")
rug(data$lwg)

# 2. confidence interval instead
lowervec <- apply(predmat,2,quantile, 0.025) 
uppervec <- apply(predmat,2,quantile, 0.975) 

plot(lwgvec, predprob, type="n", ylim=c(0,1) )
polygon(c(lwgvec, rev(lwgvec)), c(lowervec,rev(uppervec)), col="grey80", border=NA)
points(lwgvec, predprob, type="l")
rug(data$lwg)