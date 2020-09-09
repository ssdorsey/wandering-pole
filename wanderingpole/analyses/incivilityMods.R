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
tw <- readRDS('~/Dropbox/Projects/Twitter/modelData.rds') 



#######################################################################################################################################################################
### Models predicting incivility 

### 1) Model using all congresses (no ideology scores)
### 2) Add ideology scores (only 113th-115th congresses)
### 3) Each of 1 and 2 with and without seniority (lots of NAs for seniority)
### Others? Separately by chamber? More fine-grained time measure than a full Congress?


### Run models, save

# Full
mFull <- speedglm(polarizing ~ memberPresPty + absPVI + house + female + congress + icpsr, data=tw, family=binomial(link='logit'), sparse=FALSE, model=TRUE)
# Add ideology (cuts 111th and 112th)
mIdeol <- speedglm(polarizing ~ memberPresPty + ideolDiffPos + absPVI + house + female + congress + icpsr, data=tw, family=binomial('logit'), model=TRUE, sparse=FALSE)
# Add seniority to full model
mSeniority <- speedglm(polarizing ~ memberPresPty + absPVI + house + female + years + congress + icpsr, data=tw, family=binomial('logit'), model=TRUE, sparse=FALSE)
# Add seniority to ideology model
mSeniorityIdeol <- speedglm(polarizing ~ memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw, family=binomial('logit'), model=TRUE, sparse=FALSE)

# Save 
save(mFull, mIdeol, mSeniority, mSeniorityIdeol, file='~/Dropbox/Projects/Twitter/invicilityMods.rds')
# load('~/Dropbox/Projects/Twitter/invicilityMods.rds')



#############################################################################################################################################################################################################
### Presentation (coefficient plots, marginal effects, regression table)

### Coefficient plots
# Full and full(seniority) together
coefBoth <- plot_coefs(mFull, mSeniority, 
                       coefs=c(`President's Party`='memberPresPty', `Electoral Safety`='absPVI', `House`='house', `Female`='female', `Seniority`='years'), 
                       scale=TRUE,
                       inner_ci_level=0.9,
                       colors=c('black','grey40'),
                       model.names=c('Regular', 'Seniority')) +
  xlab('Coefficient') +
  ggtitle('All Congresses Model') +
  theme(plot.title = element_text(hjust = 0.5), legend.position='none') +
  xlim(c(-4,2))

# Ideology and ideology/seniority together
coefBothIdeol <- plot_coefs(mIdeol, mSeniorityIdeol, 
                           coefs=c(`President's Party`='memberPresPty', `Electoral Safety`='absPVI', `House`='house', `Female`='female', `Ideological Extremity`='ideolDiffPos', `Seniority`='years'), 
                           scale=TRUE,
                           inner_ci_level=0.9,
                           colors=c('black','grey40'),
                           model.names=c('Ideology', 'Ideology and Seniority')) +
  xlab('Coefficient') +
  ggtitle('Ideology Model (113th-115th)') +
  theme(plot.title = element_text(hjust = 0.5), legend.position='none') +
  xlim(-4,2)

# Save
pdf('~/Dropbox/Projects/Twitter/incivilityModsCoefs.pdf', width=10, height=6)
grid.arrange(coefBoth, coefBothIdeol, nrow=1)
dev.off()


### Predicted probabilities to show effect of president's party

# Need data that has 
# 1) two points for the IV of interest (pres party/not and 2 SD change in ideology)
# 2) values for all covariates for each of the two points

# Pull names of coefficients
coefs <- names(mSeniority$coefficients) 

# Pull names of factor IVs
icpsr <- coefs[str_detect(coefs, 'icpsr')]
congs <- coefs[str_detect(coefs, 'congress')]

# Make identity matrices for factor IVs
icpsrDat <- diag(d(icpsr)) %>%
  as.data.frame()
congDat <- diag(d(congs)) %>%
  as.data.frame()

# Assign column names for factor DFs
names(congDat) <- congs
names(icpsrDat) <- icpsr

# Add congs to pDat
c112 <- do.call('rbind', replicate(1366, congDat[1,], simplify=FALSE))
c113 <- do.call('rbind', replicate(1366, congDat[2,], simplify=FALSE))
c114 <- do.call('rbind', replicate(1366, congDat[3,], simplify=FALSE))
c115 <- do.call('rbind', replicate(1366, congDat[4,], simplify=FALSE))
cDat <- rbind(c112, c113, c114, c115)

# Put together a DF of covariates
pDat <- do.call("rbind", replicate(8, icpsrDat, simplify = FALSE))
index <- nrow(pDat)/8
covs <- data.frame(`(Intercept)`=rep(1, index*2),
                   memberPresPty=c(rep(0, index), rep(1, index)),
                   absPVI=rep(median(tw$absPVI), index*2),
                   house=rep(median(tw$house), index*2),
                   female=rep(median(tw$female), index*2),
                   years=rep(median(tw$years, na.rm=TRUE), index*2)
                   )
covs <- do.call("rbind", replicate(4, covs, simplify = FALSE))

# Put it all together
pDat <- cbind(cDat, pDat)
pDat <- cbind(covs, pDat)
pDat <- as.matrix(pDat)
colnames(pDat)[1] <- '(Intercept)'
pDat <- pDat[,!is.na(mSeniority$coefficients)]

# Pull model coefficients (removing NAs)
modCoefs <- mSeniority$coefficients[!is.na(mSeniority$coefficients)]

# Do prediction
preds <- pDat %*% modCoefs

# Apply link function
pDat <- as.data.frame(pDat) %>%
  mutate(pred = 1/(1+exp(-preds)))

# Effect size for president's party
effSize <- pDat %>%
  group_by(memberPresPty) %>%
  dplyr::summarise(avg=mean(pred)) 
effSize
diff(effSize$avg)


### Predicted probabilities to show effect of ideological extremity

# Pull values to compare (1 SD below mean, 1 SD above mean)
ideolVals <- c(mean(tw$ideolDiffPos, na.rm=TRUE)-sd(tw$ideolDiffPos, na.rm=TRUE),
               mean(tw$ideolDiffPos, na.rm=TRUE)+sd(tw$ideolDiffPos, na.rm=TRUE))

# Pull names of coefficients
coefsId <- names(mSeniorityIdeol$coefficients) 

# Pull names of factor IVs
icpsrId <- coefsId[str_detect(coefsId, 'icpsr')]
congsId <- coefsId[str_detect(coefsId, 'congress')]

# Make identity matrices for factor IVs
icpsrDatId <- diag(d(icpsrId)) %>%
  as.data.frame()
congDatId <- diag(d(congsId)) %>%
  as.data.frame()

# Assign column names for factor DFs
names(congDatId) <- congsId
names(icpsrDatId) <- icpsrId

# Add congs to pDat
index <- nrow(icpsrDatId)
c114Id <- do.call('rbind', replicate((index*2), congDatId[1,], simplify=FALSE))
c115Id <- do.call('rbind', replicate((index*2), congDatId[2,], simplify=FALSE))
cDatId <- rbind(c114Id, c115Id)

# Put together a DF of covariates

covsId <- data.frame(`(Intercept)`=rep(1, index*2),
                   memberPresPty=rep(1, index*2),
                   ideolDiffPos=c(rep(ideolVals[1], index), rep(ideolVals[2], index)),
                   absPVI=rep(median(tw$absPVI), index*2),
                   house=rep(median(tw$house), index*2),
                   female=rep(median(tw$female), index*2),
                   years=rep(median(tw$years, na.rm=TRUE), index*2)
)
covsId <- do.call("rbind", replicate(4, covsId, simplify = FALSE))
names(covsId)[1] <- '(Intercept)'

# Put it all together
pDatId <- do.call("rbind", replicate(8, icpsrDatId, simplify = FALSE))
pDatId <- cbind(cDatId, pDatId)
pDatId <- cbind(covsId, pDatId)
pDatId <- as.matrix(pDatId)
pDatId <- pDatId[,!is.na(mSeniorityIdeol$coefficients)]

# Pull model coefficients (removing NAs)
modCoefsId <- mSeniorityIdeol$coefficients[!is.na(mSeniorityIdeol$coefficients)]
modCoefsId <- modCoefsId[names(modCoefsId) %in% intersect(colnames(pDatId), names(modCoefsId))]

# Do prediction
predsId <- pDatId %*% modCoefsId

# Apply link function
pDatId <- as.data.frame(pDatId) %>%
  mutate(pred = 1/(1+exp(-predsId)))

# Effect size for president's party
effSizeId <- pDatId %>%
  group_by(ideolDiffPos) %>%
  dplyr::summarise(avg=mean(pred)) 
effSizeId
diff(effSizeId$avg)
  

### Plot effect sizes

# Effect sizes
effPres <- abs(diff(effSize$avg))
effId <- abs(diff(effSizeId$avg))
effs <- c(effPres, effId)

# Uncertainty estimate
coefPres <- summary(mSeniority)$coefficients
uncertPres <- abs(coefPres['memberPresPty', 'Std. Error']/coefPres['memberPresPty','Estimate'])*effPres*1.96*2
coefId <- summary(mSeniorityIdeol)$coefficients
uncertId <- abs(coefId['ideolDiffPos', 'Std. Error']/coefId['ideolDiffPos','Estimate'])*effId*1.96*2
unc <- c(uncertPres, uncertId)


barCenters <- barplot(c(effPres, effId), ylim=c(0, 0.15))
pdf('~/Dropbox/Projects/Twitter/incivilityMods_effects.pdf', width=6, height=6)
barplot(effs, ylim=c(0, 0.15), ylab='Change in Predicted Probability of Political Incivility', names.arg=c("Opposite President's\nParty", 'Ideological\nExtremity'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs[ii]-unc[ii], y1=effs[ii]+unc[ii], lwd=3)
  arrows(x0=barCenters[ii], y0=effs[ii]-unc[ii], y1=effs[ii]+unc[ii], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs[ii]/2, paste0(round(effs[ii], 3), ' +/- ', round(unc[ii], 3)))
}
dev.off()
