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
library(ggeffects)
library(jtools)
library(broom)
library(broom.mixed)
library(gridExtra)
#library(future)
#library(googleComputeEngineR)
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load data with covariates merged
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData_polarizing.rds') 

# Ideology measure from DW-NOMINATE
tw$ideolDiffPosDW <- tw$dwDist
tw$ideolDiffPosDW[tw$party=='D'] <- (-1)*tw$ideolDiffPosDW[tw$party=='D']



#######################################################################################################################################################################
### Models predicting incivility 

### 1) Model using all congresses (no ideology scores)
### 2) Add ideology scores (only 113th-115th congresses)
### 3) Each of 1 and 2 with and without seniority (lots of NAs for seniority)
### Others? Separately by chamber? More fine-grained time measure than a full Congress?

# Set up data (member-level models)
twm <- tw %>%
  group_by(icpsr, congress) %>%
  dplyr::summarise(
    n=n(),
    polarizing=table(polarizing)['1'],
    memberPresPty=unique(memberPresPty),
    absPVI=unique(absPVI),
    house=unique(house),
    female=unique(female),
    years=unique(years),
    ideolDiffPos=unique(ideolDiffPos),
    ideolDiffPosDW=unique(ideolDiffPosDW)
  )
twm$polarizing[is.na(twm$polarizing)] <- 0
twm %<>% mutate(pct_polarizing=polarizing/n)

# Run models
mFullMem <- lm(pct_polarizing ~ memberPresPty + absPVI + house + female + congress + icpsr, data=twm)
mIdMem <- lm(pct_polarizing ~ memberPresPty + ideolDiffPos + absPVI + house + female + congress + icpsr, data=twm)
mSenMem <- lm(pct_polarizing ~ memberPresPty + absPVI + house + female + years + congress + icpsr, data=twm)
mSenIdMem <- lm(pct_polarizing ~ memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=twm)
mSenMemNFE <- lm(pct_polarizing ~ memberPresPty + absPVI + house + female + years + congress, data=twm)
mSenIdMemNFE <- lm(pct_polarizing ~ memberPresPty + ideolDiffPos + absPVI + house + female + years + congress, data=twm)
mDW <- lm(pct_polarizing ~ memberPresPty + ideolDiffPosDW + absPVI + house + female + years + congress + icpsr, data=twm)
mDWNFE <- lm(pct_polarizing ~ memberPresPty + ideolDiffPosDW + absPVI + house + female + years + congress, data=twm) 
mNoTrump <- lm(pct_polarizing ~ memberPresPty + absPVI + house + female + years + congress, data=filter(twm, congress!=115))



### Regression tables
stargazer(mFullMem, mIdMem, mSenMem, mSenIdMem, mSenMemNFE, mSenIdMemNFE, mDWNFE, mNoTrump, font.size='tiny', label='polarTab', 
          keep=c('memberPresPty', 'absPVI', 'house', 'female', 'ideolDiffPos', 'ideolDiffPosDW', 'years', 
                 'congress112', 'congress113', 'congress114', 'congress115'),
          covariate.labels=c("President's Party", 'Ideological Extremity', 'Ideological Extremity (DW-NOMINATE)', 
                             '|PVI|', 'House', 'Female', 'Seniority',
                             '112th Congress', '113th Congress', '114th Congress', '115th Congress'),
          keep.stat=c('n', 'adj.rsq'), dep.var.labels = 'Dependent Variable: Proportion Polarizing', dep.var.caption='',
          title="OLS models predicting the proportion of members' tweets that were polarizing.",
          column.sep.width = '0pt')


#############################################################################################################################################################################################################
### Presentation (coefficient plots, marginal effects, regression table)


### Data for effect sizes
effPlotDat <- function(model){
  # Pull coefficient
  coefs <- summary(model)$coefficients
  if( 'ideolDiffPos' %in% names(coefficients(model)) ){
    coef <- coefs['ideolDiffPos','Estimate']
    # Pull CI
    ci <- coefs['ideolDiffPos', 'Std. Error']*1.96  
  } else {
    coef <- coefs['memberPresPty','Estimate']
    # Pull CI
    ci <- coefs['memberPresPty', 'Std. Error']*1.96 
  }
  
  # Combine/return
  effs <- c(coef=coef, ci=ci)
  effs
}
effs <- sapply(list(mSenMem, mSenIdMem), function(x){
  effPlotDat(x)
}) %>% 
  t() %>%
  abs()

# Plot and save
barCenters <- barplot(effs[,1], ylim=c(0, 0.4))
pdf('~/Dropbox/Projects/Twitter/incivilityMods_effects_polarizing.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs[,1], ylim=c(0, 0.4), ylab='Change in Proportion Polarizing', names.arg=c("Opposite President's\nParty", 'Ideological Extremity\n(2 SD Increase)'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs[ii,1]/4, paste0(round(effs[ii,1], 2), ' +/- ', round(effs[ii,2], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Effect sizes in terms of the SD of the distribution
effs[,'coef']/sd(twm$pct_polarizing)
