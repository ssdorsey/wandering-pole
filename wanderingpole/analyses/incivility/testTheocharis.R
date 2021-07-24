#########################################################################################################################################################
### Setup

# Clear workspace
rm(list=ls())

# Load libraries
library(abmisc)
library(magrittr)
library(data.table)
library(tidyverse)
library(quanteda)
library(glmnet)
library(caret)

# Source classifier from Theocharis et al.
source("C:/GitHub/incivility-sage-open/functions.R")
load("C:/GitHub/incivility-sage-open/data/lasso-classifier.rdata")
load("C:/GitHub/incivility-sage-open/data/dfm-file.rdata")

# Load our test data
test <- read.csv("~/Dropbox/Projects/Twitter/wandering-pole/wanderingpole/data/test_july2021.csv")



#########################################################################################################################################################
### Accuracy metrics


#########################################################################################################################################################
### Evaluate classifier on our test data

### Use their classifier

# Pull vector of test data
text <- test$text

# Get predictions from classifier
preds <- predict_incivility(text, 
                   old_dfm = dfm,
                   classifier = lasso)
preds <- round(preds)

# Add to test data
test$preds <- preds


### Performance metrics

# Setup
test %<>% mutate(acc = ifelse(preds==labels, 1, 0))
zeros <- filter(test, labels==0)
ones <- filter(test, labels==1)
y <- factor(test$labels)
y_zeros <- factor(zeros$labels)
y_ones <- factor(ones$labels)
predictions <- factor(test$preds)
predictions_zeros <- factor(zeros$preds)
predictions_ones <- factor(ones$preds)


# Accuracy
table(test$acc) %>% prop.table()
filter(test, labels==1) %>% pull(acc) %>% table() %>% prop.table()
filter(test, labels==0) %>% pull(acc) %>% table() %>% prop.table()


# Precision
posPredValue(predictions, y, positive='1')





# Compute precision


# Compute recall


# Compute F1



