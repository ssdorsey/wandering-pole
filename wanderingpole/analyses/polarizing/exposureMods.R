#######################################################################################################################################################################################################################################
### Setup

# Clear workspace, load packages, set WD
rm(list=ls())
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
library(ggeffects)
library(jtools)
library(broom)
library(broom.mixed)
library(gridExtra)
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load data with covariates merged
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData_polarizing.rds')



#########################################################################################################################################################################################################################################
### Models predicting exposure

# Run models, save (takes about 40min per model)
mRT <- lm(rtDiff ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
saveRDS(mRT, file='~/Dropbox/Projects/Twitter/rtMod_polarizing.RData'); rm(mRT); gc()
mFave <- lm(faveDiff ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
saveRDS(mFave, file='~/Dropbox/Projects/Twitter/faveMod_polarizing.RData'); rm(mFave); gc()
#mRTCiv <- speedlm(rtDiffCiv ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
#saveRDS(mRTCiv, file='~/Dropbox/Projects/Twitter/rtCivMod.RData'); rm(mRTCiv); gc()
#mFaveCiv <- speedlm(faveDiffCiv ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
#saveRDS(mFaveCiv, file='~/Dropbox/Projects/Twitter/faveCivMod.RData'); rm(mFaveCiv); gc()
mRT <- readRDS('~/Dropbox/Projects/Twitter/rtMod_polarizing.RData')
mFave <- readRDS('~/Dropbox/Projects/Twitter/faveMod_polarizing.RData')

# Regression table
stargazer(mFave, mRT, font.size='small', label='engagementTab', colnames=FALSE,
          keep=c('polarizing', 'memberPresPty', 'absPVI', 'house', 'female', 'ideolDiffPos', 'years', 
                 'congress114', 'congress115'),
          covariate.labels=c('Polarizing', "President's Party", 'Ideological Extremity', '|PVI|', 'House', 'Female', 'Seniority',
                             '114th Congress', '115th Congress'),
          keep.stat=c('n', 'adj.rsq'), dep.var.labels = c('Difference from Average Likes', 'Difference from Average Retweets'),
          title="OLS models predicting the number of likes (column 1) and retweets (column 2) relative to the average tweet for each member.")



###################################################################################################################################################################################################################################
### Effects plots

# Pull effect size (coefficient in LMs) and CI (1.96*SE)
effPlotDat <- function(model){
  # Pull coefficient
  coefs <- summary(model)$coefficients
  coef <- coefs['polarizing','Estimate']
  # Pull CI
  ci <- coefs['polarizing', 'Std. Error']*1.96
  # Combine/return
  effs <- c(coef=coef, ci=ci)
  effs
}
effs <- sapply(list(mFave, mRT), function(x){
  effPlotDat(x)
}) %>% 
  t()

# Plot and save
barCenters <- barplot(effs[,1], ylim=c(0, 250))
pdf('~/Dropbox/Projects/Twitter/engageMods_effects_polarizing.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs[,1], ylim=c(0, 250), ylab='Effect of Polarizing Rhetoric', names.arg=c('Likes above\nAverage', 'Retweets above\nAverage'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs[ii,1]/2, paste0(round(effs[ii,1], 2), ' +/- ', round(effs[ii,2], 2)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()




