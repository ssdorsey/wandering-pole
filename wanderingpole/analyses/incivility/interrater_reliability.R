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
options(scipen=999)

# Load finished training data
sp <- read.csv('~/Dropbox/Projects/Twitter/Twitter/training_data/spencer_training.csv', stringsAsFactors = FALSE)
mi <- read.csv('~/Dropbox/Projects/Twitter/Twitter/training_data/michael_training.csv', stringsAsFactors = FALSE)
mi2 <- read.csv('~/Dropbox/Projects/Twitter/Twitter/training_data/michael_training_round2.csv', stringsAsFactors = FALSE)
sp2 <- read.csv('~/Dropbox/Projects/Twitter/Twitter/training_data/spencer_training_round2.csv', stringsAsFactors = FALSE)


### Round one data

# Pull text of the overlap set
matches <- intersect(sp$text, mi$text)

# Filter to overlap set
overlap_sp <- filter(sp, text %in% matches) %>% filter(!duplicated(text)) %>% arrange(text)
overlap_mi <- filter(mi, text %in% matches) %>% arrange(text)

# Merge, add uncivil
overlap <- cbind(overlap_sp, uncivil_mi=overlap_mi$uncivil)

# Dataframe for two-rater agreement measures
irdf <- overlap %>%
  dplyr::select(uncivil, uncivil_mi) %>% 
  filter(complete.cases(.)) %>%
  dplyr::rename(
    rater1=uncivil,
    rater2=uncivil_mi
  ) 


### Round 2 data

# Pull text of the overlap set
matches2 <- c(intersect(sp$text, mi2$text), intersect(mi$text, sp2$text))

# Match second round Michael to first round Spencer
overlap_s1 <- filter(sp, text %in% matches2) %>% filter(!duplicated(text)) %>% arrange(text)
overlap_m2 <- filter(mi2, text %in% matches2) %>% arrange(text)
overlap_s1m2 <- cbind(overlap_s1, uncivil_2=overlap_m2$uncivil) %>%
  mutate(original='spencer') 

# Match second round Spencer to first round Michael
overlap_m1 <- filter(mi, text %in% matches2) %>% filter(!duplicated(text)) %>% arrange(text)
overlap_s2 <- filter(sp2, text %in% matches2) %>% arrange(text)
overlap_m1s2 <- cbind(overlap_m1, uncivil_2=overlap_s2$uncivil) %>%
  mutate(original='michael') %>%
  dplyr::select(-flagged)

# Join overlap DFs
overlap2 <- rbind(overlap_m1s2, overlap_s1m2)

# Dataframe for two-rater agreement measures
irdf2 <- overlap2 %>%
  dplyr::select(uncivil, uncivil_2) %>% 
  filter(complete.cases(.)) %>%
  dplyr::rename(
    rater1=uncivil,
    rater2=uncivil_2
  ) 

# Create final coding category
overlap2 %<>% mutate(uncivil_final=ifelse(uncivil==uncivil_2, uncivil, NA)) %>%
  dplyr::select(date, date_collected, divisive, screen_name, tweet_id, retweeted, type, original, uncivil, uncivil_2, uncivil_final, text)

# Save to resolve discrepancies by hand and then use as the final training data to train the model
# write.csv(overlap2, '~/Dropbox/Projects/Twitter/Twitter/training_data/training_final.csv', row.names=FALSE)

#########################################################################################################################################################
### Interrater reliability

### Round 1

# Compute agreement
agree(irdf)

# Compute kappa
kappa2(irdf)

# Compute kripp's alpha
kripp.alpha(t(irdf))

# Compute cronbach's alpha
alpha(irdf)$total$std.alpha


### Round 2

# Compute agreement
agree(irdf2)

# Compute kappa
kappa2(irdf2)

# Compute kripp's alpha
kripp.alpha(t(irdf2))

# Compute cronbach's alpha
alpha(irdf2)$total$std.alpha

