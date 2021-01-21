###########################################################################################################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls());gc()
library(data.table)
library(tidyverse)
library(magrittr)
library(abmisc)
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load data with covariates merged
mut <- read.csv('MonthlyUncivilTweets.csv', stringsAsFactors = FALSE)

# Reformat date
mut %<>% mutate(month=as.character(month))
mut$month[nchar(mut$month)==1] <- paste0('0', mut$month[nchar(mut$month)==1])
mut %<>% mutate(yearmonth=paste0(year, '-', month))

# Format names
names(mut) <- tolower(names(mut))
names(mut)[1] <- 'Party'

# Remove independents
plotdf <- filter(mut, Party!='I')



#######################################################################################################################################################################
### Plot of incivility by party over time

monthplot <- ggplot(data = plotdf, aes(x = yearmonth, y = pct.uncivil)) +
  geom_point(color = "black", size = 1.5) +
  geom_line(mapping = (aes(group = Party, color = Party)), lwd=1.5) +
  labs(title = "") + ylab("Percent Uncivil") + xlab("Month") +
  scale_color_manual(breaks = c("R", "D", "All"),
                     values=c("grey33", "grey66", "black")) +
  geom_vline(xintercept = "2016-11", linetype="twodash",
             color = "black", size=1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=90, vjust=.5)) +
  ylim(0,20)

ggsave(filename='~/Dropbox/Projects/Twitter/monthlyUncivilTweets.pdf', plot=monthplot, width=15, height=10)
