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
cm <- as.matrix(table(Actual=test$labels, Predicted=test$preds))
n <- sum(cm) # number of instances
diag <- diag(cm) # number of correctly classified instances per class 
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class

# Accuracy
table(test$acc) %>% prop.table()
filter(test, labels==1) %>% pull(acc) %>% table() %>% prop.table()
filter(test, labels==0) %>% pull(acc) %>% table() %>% prop.table()

# Precision
precision <- diag / colsums
mean(precision) # Macro average
sum(precision*rowsums/n) # weighted average

# Recall
recall <- diag / rowsums
mean(recall) # Macro average
sum(recall*rowsums/n) # weighted average

# F1-score
f1 <- (2 * precision * recall) / (precision + recall)
mean(f1) # Macro average
sum(f1*rowsums/n) # weighted average



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
cm <- as.matrix(table(Actual=test$labels, Predicted=test$vader))
n <- sum(cm) # number of instances
diag <- diag(cm) # number of correctly classified instances per class 
rowsums <- apply(cm, 1, sum) # number of instances per class
colsums <- apply(cm, 2, sum) # number of predictions per class

# Accuracy
table(test$acc_vader) %>% prop.table()
filter(test, labels==1) %>% pull(acc_vader) %>% table() %>% prop.table()
filter(test, labels==0) %>% pull(acc_vader) %>% table() %>% prop.table()

# Precision
precision <- diag / colsums
mean(precision) # Macro average
sum(precision*rowsums/n) # weighted average

# Recall
recall <- diag / rowsums
mean(recall) # Macro average
sum(recall*rowsums/n) # weighted average

# F1-score
f1 <- (2 * precision * recall) / (precision + recall)
mean(f1) # Macro average
sum(f1*rowsums/n) # weighted average
