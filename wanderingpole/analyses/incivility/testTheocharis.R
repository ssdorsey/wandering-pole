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

### Use Theocharis et al's classifier

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
y <- factor(test$labels)
predictions <- factor(test$preds)

# Accuracy
table(test$acc) %>% prop.table()
filter(test, labels==1) %>% pull(acc) %>% table() %>% prop.table()
filter(test, labels==0) %>% pull(acc) %>% table() %>% prop.table()

# Precision
precision <- posPredValue(predictions, y, positive='1')

# Recall
recall <- sensitivity(predictions, y, positive="1")

# Compute F1
F1 <- (2 * precision * recall) / (precision + recall)



#########################################################################################################################################################
### VADER Analysis

### Get VADER predictions

# Run VADER
vader <- vader_df(test$text)

# Generate predictions (in the standard way, compound < -0.05)
vader %<>% mutate(vader_pred = ifelse(compound <= -0.05, 1, 0))

# Add predictions to test
test$vader <- vader$vader_pred


### Performance metrics

# Setup
test %<>% mutate(acc_vader = ifelse(vader==labels, 1, 0))
y <- factor(test$labels)
predictions <- factor(test$vader)

# Accuracy
table(test$acc_vader) %>% prop.table()
filter(test, labels==1) %>% pull(acc_vader) %>% table() %>% prop.table()
filter(test, labels==0) %>% pull(acc_vader) %>% table() %>% prop.table()

# Precision
precision <- posPredValue(predictions, y, positive='1')

# Recall
recall <- sensitivity(predictions, y, positive="1")

# Compute F1
F1 <- (2 * precision * recall) / (precision + recall)
