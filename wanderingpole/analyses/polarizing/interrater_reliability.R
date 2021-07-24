#########################################################################################################################################################
### Setup

# Clear workspace
rm(list=ls())

# Load libraries
library(stringr)
library(jsonlite)
library(dplyr)
library(magrittr)
library(data.table)
library(tidyverse)
library(irr)
library(psych)


### Load data

# List RA names
names <- c('leela', 'london', 'zoe', 'jonathan')

# Load interrater reliability DFs, concatenate
irt <- c()
for(name in names){
  df <- fread(file=paste0('~/Dropbox/Projects/Twitter/Twitter RAs/', name, 'IR.csv'), stringsAsFactors = FALSE, data.table=FALSE)
  df <- mutate(df, id=paste0(screen_name, tweet_id, date)) %>%
    mutate(irName=name)
  irt <- rbind(irt, df)
}

# Load originally coded DFs, concatenate
ot <- c()
for(name in names){
  df <- fread(file=paste0('~/Dropbox/Projects/Twitter/Twitter RAs/Initial Tweets/', name, 'Tweets.csv'), stringsAsFactors = FALSE, data.table=FALSE)
  df <- mutate(df, id=paste0(screen_name, tweet_id, date)) %>%
    mutate(name=name)
  ot <- rbind(ot, df)
}


### Format data

# Set up interrater tweets to merge with original tweet codings
irt <- dplyr::select(irt, c(id,irName, HTC, Unifying, Divisive, Congratulatory)) %>%
  dplyr::rename(htcIR=HTC) %>%
  dplyr::rename(unifIR = Unifying) %>%
  dplyr::rename(divIR = Divisive) %>%
  dplyr::rename(congratIR = Congratulatory)

# Merge initial tweets coding to interrater relability coding
mt <- left_join(ot, irt, by='id') %>%
  filter(!is.na(irName) & !duplicated(id))

# Dataframe for two-rater agreement measures
mt2 <- mt %>%
  dplyr::select(Divisive, divIR) %>% 
  filter(complete.cases(.)) %>%
  dplyr::rename(
    rater1=Divisive,
    rater2=divIR
  ) 



#########################################################################################################################################################
### Interrater reliability


### Overall reliability
# Compute agreement
agree(mt2)
# Compute kappa
kappa2(mt2)
# Compute kripp's alpha
kripp.alpha(t(mt2))
# Compute cronbach's alpha
ltm::cronbach.alpha(t(mt2))


### Pairwise IRR (% agree, kappa, kripps/cronbach's alpha)

# Set up dataframe
mtmult <- mt %>%
  dplyr::select(name, Divisive, irName, divIR) %>% 
  filter(complete.cases(.)) %>%
  dplyr::rename(
    rater1=name,
    rating1=Divisive,
    rater2=irName,
    rating2=divIR
  ) 

# Empty list for results 
pairIRR <- list()

# Loop through, compute pairwise IRR
for(ii in 1:d(names)){
  # Set up names and dataframes
  res <- c()
  name <- names[ii]
  comps <- names[names!=name]
  df <- filter(mtmult, rater1==name | rater2==name)
  
  # Compute overall IRR
  pct_agree <- agree(dplyr::select(df, c(rating1, rating2)))$value/100
  kappa <- kappa2(dplyr::select(df, c(rating1, rating2)))$value
  kripp <- kripp.alpha(t(dplyr::select(df, c(rating1, rating2))))$value
  cron <- ltm::cronbach.alpha(t(dplyr::select(df, c(rating1, rating2))))$alpha
  overall <- c(agree=pct_agree, kappa=kappa, kripp=kripp, cron=cron)
  res <- rbind(res, overall)
  
  # Go through comps and compute pairwise IRR
  for(jj in 1:d(comps)){
    comp <- comps[jj]
    compdf <- filter(df, rater1==comp | rater2==comp)
    pct_agree_comp <- agree(dplyr::select(compdf, c(rating1, rating2)))$value/100
    kappa_comp <- kappa2(dplyr::select(compdf, c(rating1, rating2)))$value
    kripp_comp <- kripp.alpha(t(dplyr::select(compdf, c(rating1, rating2))))$value
    cron_comp <- ltm::cronbach.alpha(t(dplyr::select(compdf, c(rating1, rating2))))$alpha
    comp_res <- c(agree=pct_agree_comp, kappa=kappa_comp, kripp=kripp_comp, cron=cron_comp)
    res <- rbind(res, comp_res)
  }
  rownames(res) <- c('overall', comps)
  
  pairIRR[[ii]] <- res
}
names(pairIRR) <- names



######################################################################################################################################################
# Dataframes to sort out disagreements

for(nm in names){
  dfU <- filter(mt, unifAny1==1 & accU==0 & (name==nm | irName==nm)) %>%
    mutate('You Said'=ifelse(name==nm, Unifying, unifIR) %>% car::recode(., "1='Unifying'; 0='Not Unifying'")) %>%
    mutate('They Said'=ifelse(name==nm, unifIR, Unifying) %>% car::recode(., "1='Unifying'; 0='Not Unifying'")) %>%
    mutate('You Now Say'='') %>%
    mutate(Explanation='') %>%
    dplyr::select(., c('screen_name', 'tweet_id', 'date', 'date_collected', 'text', 'Notes',
                       'You Said', 'They Said', 'You Now Say', 'Explanation' ) )
  dfD <- filter(mt, divAny1==1 & accD==0 & (name==nm | irName==nm)) %>%
    mutate('You Said'=ifelse(name==nm, Divisive, divIR) %>% car::recode(., "1='Divisive'; 0='Not Divisive'")) %>%
    mutate('They Said'=ifelse(name==nm, divIR, Divisive) %>% car::recode(., "1='Divisive'; 0='Not Divisive'")) %>%
    mutate('You Now Say'='') %>%
    mutate(Explanation='') %>%
    dplyr::select(., c('screen_name', 'tweet_id', 'date', 'date_collected', 'text', 'Notes',
                       'You Said', 'They Said', 'You Now Say', 'Explanation' ) )
  df <- rbind(dfD, dfU)
  write.csv(df, file=paste0('C:/Dropbox/Projects/Twitter/Twitter RAs/Post-IR/', nm, '_post_IR_explanations.csv'), row.names = FALSE)
}
