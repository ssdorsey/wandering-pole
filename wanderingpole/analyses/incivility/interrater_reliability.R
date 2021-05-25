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
  select(-flagged)

# Join overlap DFs
overlap2 <- overlap_m1s2

# Dataframe for two-rater agreement measures
irdf2 <- overlap2 %>%
  dplyr::select(uncivil, uncivil_2) %>% 
  filter(complete.cases(.)) %>%
  dplyr::rename(
    rater1=uncivil,
    rater2=uncivil_2
  ) 


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
