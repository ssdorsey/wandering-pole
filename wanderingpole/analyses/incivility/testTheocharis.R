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

# Source classifier from Theocharis et al.
source("C:/GitHub/incivility-sage-open/functions.R")
load("C:/GitHub/incivility-sage-open/data/lasso-classifier.rdata")
load("C:/GitHub/incivility-sage-open/data/dfm-file.rdata")

# Load our test data
test <- read.csv()



#########################################################################################################################################################
### Accuracy metrics


#########################################################################################################################################################
### Evaluate classifier on our test data

# Pull vector of test data
text <- test$text

# Get predictions from classifier
preds <- predict_incivility(text, 
                   old_dfm = dfm,
                   classifier = lasso)
preds <- round(preds)

# Add to test data
test$preds <- preds

# Compute accuracy


# Compute precision


# Compute recall


# Compute F1



