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
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData.rds')



#########################################################################################################################################################################################################################################
### Models predicting exposure

# Run models, save (takes about 40min per model)
mRT <- speedlm(rtDiff ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
saveRDS(mRT, file='~/Dropbox/Projects/Twitter/rtMod.RData'); rm(mRT); gc()
mFave <- speedlm(faveDiff ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
saveRDS(mFave, file='~/Dropbox/Projects/Twitter/faveMod.RData'); rm(mFave); gc()
#mRTCiv <- speedlm(rtDiffCiv ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
#saveRDS(mRTCiv, file='~/Dropbox/Projects/Twitter/rtCivMod.RData'); rm(mRTCiv); gc()
#mFaveCiv <- speedlm(faveDiffCiv ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
#saveRDS(mFaveCiv, file='~/Dropbox/Projects/Twitter/faveCivMod.RData'); rm(mFaveCiv); gc()
mRT <- readRDS('~/Dropbox/Projects/Twitter/rtMod.RData')
mFave <- readRDS('~/Dropbox/Projects/Twitter/faveMod.RData')



###################################################################################################################################################################################################################################
### Coefficient plots

# Named vector to loop through (and a list to store the plots in)
mods <- c(rtMod='Retweets above Average', rtCivMod='Retweets above Average (Civil)',
          faveMod='Likes above Average', faveCivMod='Likes above Average (Civil)')
coefPlots <- list()

for(ii in 1:length(mods)){
  # Load a model
  mod <- readRDS(file=paste0('~/Dropbox/Projects/Twitter/', names(mods)[ii], '.RData'))

  # Save coefficient plot as a ggplot object
  modPlot <- plot_coefs(mod,
                         coefs=c(`Incivility`='polarizing', `Ideological Extremity`='ideolDiffPos', `President's Party`='memberPresPty', `Electoral Safety`='absPVI', `House`='house', `Female`='female', `Seniority`='years'),
                         scale=TRUE,
                         inner_ci_level=0.9,
                         colors=c('black')) +
    xlab('Coefficient') +
    ggtitle(mods[ii]) +
    theme(plot.title = element_text(hjust = 0.5))

  # Store ggplot object in the plots list
  coefPlots[[ii]] <- modPlot

  # Remove the model object, garbage clean
  rm(mod); gc()
}

# Plot all together
pdf('~/Dropbox/Projects/Twitter/engageModsAll.pdf', width=12, height=12)
par(mfrow=c(2,2))
do.call(grid.arrange, c(coefPlots, list(layout_matrix=rbind( c(1, 2), c(3, 4) ) ) ) )
#grid.arrange(coefPlots[[1]], coefPlots[[2]], coefPlots[[3]], coefPlots[[4]],
             # layout_matrix=rbind(c(1, 2), c(3, 4)))
par(mfrow=c(1,1))
dev.off()

# Plot just the effects for the overall average changes
pdf('~/Dropbox/Projects/Twitter/engageMods.pdf', width=10, height=6)
par(mfrow=c(2,2))
grid.arrange(coefPlots[[1]], coefPlots[[3]], layout_matrix=rbind(c(1, 2)))
par(mfrow=c(1,1))
dev.off()

# Plot just the effects for the changes relative to civil tweets
pdf('~/Dropbox/Projects/Twitter/engageModsCivil.pdf', width=10, height=6)
par(mfrow=c(2,2))
grid.arrange(coefPlots[[2]], coefPlots[[4]], layout_matrix=rbind(c(1, 2)))
par(mfrow=c(1,1))
dev.off()



###################################################################################################################################################################################################################################
### Effects plots

# Pull effect size (coefficient in LMs) and CI (1.96*SE)
effPlotDat <- function(model){
  # Pull coefficient
  coefs <- summary(model)$coefficients
  coef <- coefs['polarizing','coef']
  # Pull CI
  ci <- coefs['polarizing', 'se']*1.96
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
pdf('~/Dropbox/Projects/Twitter/engageMods_effects.pdf', width=6, height=6)
barplot(effs[,1], ylim=c(0, 250), ylab='Effect Size', names.arg=c('Likes above\nAverage', 'Retweets above\nAverage'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs[ii,1]/2, paste0(round(effs[ii,1], 2), ' +/- ', round(effs[ii,2], 2)))
}
dev.off()

      