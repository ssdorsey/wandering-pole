
library(MASS)
library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)
library(lubridate)
library(Hmisc)
# library(abmisc)

##Covariate Merging by Congress

nominate <- read.csv("C:/Users/User/Dropbox//Twitter/covariateData/RR/HSall_members2021.csv")

c111 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 111th.csv")
c112 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 112th.csv")
c113 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 113th.csv")
c114 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 114th.csv")
c115 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 115th.csv")
c116 <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/Members by Congress 116th.csv")

c111$GovTrack <- as.numeric(c111$GovTrack)
c112$GovTrack <- as.numeric(c112$GovTrack)
c113$GovTrack <- as.numeric(c113$GovTrack)
c114$GovTrack <- as.numeric(c114$GovTrack)
c115$GovTrack <- as.numeric(c115$GovTrack)
c116$GovTrack <- as.numeric(c116$GovTrack)

c111$govdist <- abs(c111$GovTrack - mean(c111$GovTrack,na.rm=TRUE))
c112$govdist <- abs(c112$GovTrack - mean(c112$GovTrack,na.rm=TRUE))
c113$govdist <- abs(c113$GovTrack - mean(c113$GovTrack,na.rm=TRUE))
c114$govdist <- abs(c114$GovTrack - mean(c114$GovTrack,na.rm=TRUE))
c115$govdist <- abs(c115$GovTrack - mean(c115$GovTrack,na.rm=TRUE))
c116$govdist <- abs(c116$GovTrack - mean(c116$GovTrack,na.rm=TRUE))

nominate111 <- dplyr::select(filter(nominate, congress == "111"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate112 <- dplyr::select(filter(nominate, congress == "112"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate113 <- dplyr::select(filter(nominate, congress == "113"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate114 <- dplyr::select(filter(nominate, congress == "114"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate115 <- dplyr::select(filter(nominate, congress == "115"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate116 <- dplyr::select(filter(nominate, congress == "116"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)

m111 <- merge(c111, nominate111, by = "icpsr")
m112 <- merge(c112, nominate112, by = "icpsr")
m113 <- merge(c113, nominate113, by = "icpsr")
m114 <- merge(c114, nominate114, by = "icpsr")
m115 <- merge(c115, nominate115, by = "icpsr")
m116 <- merge(c116, nominate116, by = "icpsr")

m111$DWdist <- abs(m111$nominate_dim1 - mean(m111$nominate_dim1))
m112$DWdist <- abs(m112$nominate_dim1 - mean(m112$nominate_dim1))
m113$DWdist <- abs(m113$nominate_dim1 - mean(m113$nominate_dim1))
m114$DWdist <- abs(m114$nominate_dim1 - mean(m114$nominate_dim1))
m115$DWdist <- abs(m115$nominate_dim1 - mean(m115$nominate_dim1))
m116$DWdist <- abs(m116$nominate_dim1 - mean(m116$nominate_dim1))

m111$NPdist <- abs(m111$nokken_poole_dim1 - mean(m111$nokken_poole_dim1))
m112$NPdist <- abs(m112$nokken_poole_dim1 - mean(m112$nokken_poole_dim1))
m113$NPdist <- abs(m113$nokken_poole_dim1 - mean(m113$nokken_poole_dim1))
m114$NPdist <- abs(m114$nokken_poole_dim1 - mean(m114$nokken_poole_dim1))
m115$NPdist <- abs(m115$nokken_poole_dim1 - mean(m115$nokken_poole_dim1))
m116$NPdist <- abs(m116$nokken_poole_dim1 - mean(m116$nokken_poole_dim1))

combined <- rbind(m111, m112, m113, m114, m115, m116)

combined$icpsrcongress <- paste0(as.character(combined$icpsr),"-", as.character(combined$congress))

##Tweets by Member

tweets <- fread("C:/Users/User/Dropbox/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv")

congresstweets <- tweets %>%
  group_by(twitter_lower, congress) %>%
  dplyr::summarize(
    tweets = n(),
    uncivil = sum(uncivil2)
  ) %>%
  ungroup()

handles <- read.csv("C:/Users/User/Dropbox/Twitter/US Congress Handles Master List.csv")

handles <- dplyr::select(handles, icpsr, twitter_lower, Official)

handletweets <- merge(congresstweets, handles, by = "twitter_lower")

##############skip for handle-level analysis
congresstweets <- handletweets %>%
  group_by(icpsr, congress) %>%
  dplyr::summarize(
    tweets = sum(tweets),
    uncivil = sum(uncivil)
  ) %>%
  ungroup()

congresstweets$pct.uncivil <- ((congresstweets$uncivil / congresstweets$tweets) *100)

congresstweets$icpsrcongress <-  paste0(as.character(congresstweets$icpsr),"-", as.character(congresstweets$congress))



###################################################################################################################################################################
### Percentage Polarizing Models


### Run models (base model)
congresstweets <- merge(congresstweets, combined, by = "icpsrcongress")

congresstweets$republican <- ifelse(congresstweets$party == "R",1,0)

congresstweets$govscaled <- scale(congresstweets$govdist)[, 1]
congresstweets$DWscaled <- scale(congresstweets$DWdist)[, 1]
congresstweets$NPscaled <- scale(congresstweets$NPdist)[, 1]
congresstweets$PVIscaled <- scale(congresstweets$PVIABS)[, 1]
congresstweets$Seniorityscaled <- scale(congresstweets$Seniority)[, 1]


govmodel <- felm(pct.uncivil ~ govscaled + Pres.Party + PVIscaled + chamber.y + female + republican + Chamber.Majority + Seniorityscaled + Freshman | congress.x, data = congresstweets)
DWmodel <- felm(pct.uncivil ~ DWscaled + Pres.Party + PVIscaled + chamber.y + female + republican + Chamber.Majority + Seniorityscaled + Freshman| congress.x, data = congresstweets)
NPmodel <- felm(pct.uncivil ~ NPscaled + Pres.Party + PVIscaled + chamber.y + female + republican + Chamber.Majority + Seniorityscaled + Freshman| congress.x, data = congresstweets)

### Nooken-Poole member fixed-effects model - DW is a 0-variance measure, so can't be used as reliably (the only variance is based on distance from median at each congres, rather than member movement)
femodel <- felm(pct.uncivil ~ NPscaled + Pres.Party + PVIscaled + Chamber.Majority + Seniorityscaled + Freshman| congress.x + icpsr.x, data = congresstweets)

stargazer(govmodel, DWmodel, NPmodel, femodel, type="html", out = "Incivility Mods.htm")

### Pre/post-trump split models
govmodelpre <- felm(pct.uncivil ~ govscaled + Pres.Party + PVIscaled + chamber.y + female + Chamber.Majority + Seniorityscaled + Freshman | congress.x, data = filter(congresstweets, congress.x < 115))
govmodelpost <- felm(pct.uncivil ~ govscaled + Pres.Party + PVIscaled + chamber.y + female + Chamber.Majority + Seniorityscaled + Freshman | congress.x, data = filter(congresstweets, congress.x > 114))

DWmodelpre <- felm(pct.uncivil ~ DWscaled + Pres.Party + PVIscaled + chamber.y + female + Chamber.Majority + Seniorityscaled + Freshman| congress.x, data = filter(congresstweets, congress.x < 115))
DWmodelpost <- felm(pct.uncivil ~ DWscaled + Pres.Party + PVIscaled + chamber.y + female + Chamber.Majority + Seniorityscaled + Freshman| congress.x, data = filter(congresstweets, congress.x > 114))

stargazer(govmodelpre, govmodelpost, DWmodelpre, DWmodelpost, type="html", out = "Incivility Mods Pre Post.htm")

### Plot seniority

ggplot(congresstweets, aes(x=Seniority, y=pct.uncivil, shape=as.factor(republican), color=as.factor(republican))) +
  geom_point() +
  stat_smooth(aes(fill=as.factor(republican)))+
  facet_wrap(~ congress.x)

### Plot results

## Data for effect sizes

# Flexible function for each plot
effPlotDat <- function(models, plot_vars){
  if(length(models)==1){
    mod <- models[[1]]
    # Pull coefficients
    coef_tab <- summary(mod)$coefficients
    coefs <- coef_tab[plot_vars,'Estimate']
    cis <- coef_tab[plot_vars,'Std. Error']*1.96
    
    # Combine/return
    effs <- data.frame(coef=coefs, ci=cis)
    effs$coef[2] <- abs(effs$coef[2]) # because the President's party variable is coded as being *in* the president's party
    
  } else {
    effs <- sapply(models, function(mod){
      # Pull coefficients
      coef_tab <- summary(mod)$coefficients
      coefs <- coef_tab[intersect(plot_vars, rownames(mod$coefficients)),'Estimate']
      cis <- coef_tab[intersect(plot_vars, rownames(mod$coefficients)),'Std. Error']*1.96
      
      # Combine/return
      eff <- data.frame(coef=coefs, ci=cis)
      eff
    }) %>% 
      t() %>%
      as.data.frame() %>%
      mutate(coef=num(coef), ci=num(ci))
  }
  # Deal with president's party reverse coding
  if(any(str_detect(plot_vars, 'Party'))){
    effs$coef <- abs(effs$coef)
  }
  effs
}

# President's party and electoral safety
effs_ps <- effPlotDat(models=list(govmodel), plot_vars=c('Pres.Party', 'PVIABS'))

# Two versions of ideological extremity
effs_ie <- effPlotDat(models=list(govmodel, DWmodel), plot_vars=c('govdist', 'DWdist'))

# Pre and post trump
effs_tr <- effPlotDat(models=list(DWmodelpre, DWmodelpost), plot_vars='Pres.Party')

# Plot president's party
barCenters <- barplot(c(effs_ps[1,1], effs_tr[,1]), ylim=c(0, 0.15))
pdf('~/Dropbox/Projects/Twitter/incivilityMods_effects_party.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(c(effs_ps[1,1], effs_tr[,1]), ylim=c(0, 0.15), ylab='Change in Proportion Polarizing', names.arg=c("Overall", 'Obama', 'Trump'))
segments(x0=barCenters[1], y0=effs_ps[1,1]-effs_ps[1,2], y1=effs_ps[1,1]+effs_ps[1,2], lwd=3)
arrows(x0=barCenters[1], y0=effs_ps[1,1]-effs_ps[1,2], y1=effs_ps[1,1]+effs_ps[1,2], lwd=3, angle=90, code=3, length=0.05)  
text(x=barCenters[1], y=effs_ps[1,1]/4, paste0(round(effs_ps[1,1], 2), ' +/- ', round(effs_ps[1,2], 3)))
for(ii in 1:2){
  segments(x0=barCenters[ii+1], y0=effs_tr[ii,1]-effs_tr[ii,2], y1=effs_tr[ii,1]+effs_tr[ii,2], lwd=3)
  arrows(x0=barCenters[ii+1], y0=effs_tr[ii,1]-effs_tr[ii,2], y1=effs_tr[ii,1]+effs_tr[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii+1], y=effs_tr[ii,1]/4, paste0(round(effs_tr[ii,1], 2), ' +/- ', round(effs_tr[ii,2], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Electoral safety
barCenters <- barplot(effs_ps[2,1], ylim=c(0, 0.005))
pdf('~/Dropbox/Projects/Twitter/incivilityMods_effects_electoral_safety.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs_ps[2,1], ylim=c(0, 0.005), xlim=c(-0.5, 2), ylab='Change in Proportion Polarizing', names.arg='')
segments(x0=barCenters, y0=effs_ps[2,1]-effs_ps[2,2], y1=effs_ps[2,1]+effs_ps[2,2], lwd=3)
arrows(x0=barCenters, y0=effs_ps[2,1]-effs_ps[2,2], y1=effs_ps[2,1]+effs_ps[2,2], lwd=3, angle=90, code=3, length=0.05)  
text(x=barCenters, y=effs_ps[2,1]/4, paste0(round(effs_ps[2,1], 3), ' +/- ', round(effs_ps[2,2], 3)))
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Plot two ways to measure ideological extremity
barCenters <- barplot(effs_ie[,1], ylim=c(0, 0.4))
pdf('~/Dropbox/Projects/Twitter/incivilityMods_effects_ideology.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs_ie[,1], ylim=c(0, 0.4), ylab='Change in Proportion Polarizing', 
        names.arg=c("GovTrack", 'DW-NOMINATE'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs_ie[ii,1]-effs_ie[ii,2], y1=effs_ie[ii,1]+effs_ie[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs_ie[ii,1]-effs_ie[ii,2], y1=effs_ie[ii,1]+effs_ie[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs_ie[ii,1]/4, paste0(round(effs_ie[ii,1], 2), ' +/- ', round(effs_ie[ii,2], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Plot before and after Trump
barCenters <- barplot(effs_tr[,1], ylim=c(0, 0.15))
pdf('~/Dropbox/Projects/Twitter/incivilityMods_effects_trump.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs_tr[,1], ylim=c(0, 0.15), ylab='Change in Proportion Polarizing', names.arg=c("Obama", 'Trump'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs_tr[ii,1]-effs_tr[ii,2], y1=effs_tr[ii,1]+effs_tr[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs_tr[ii,1]-effs_tr[ii,2], y1=effs_tr[ii,1]+effs_tr[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs_tr[ii,1]/4, paste0(round(effs_tr[ii,1], 2), ' +/- ', round(effs_tr[ii,2], 3)))
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

# Effect sizes in terms of the SD of the distribution
effs[,'coef']/sd(twm$pct_polarizing)

# Model table
stargazer(govmodel, DWmodel, NPmodel, govmodel2, DWmodel2, NPmodel2, govmodel3, DWmodel3, NPmodel3,
          type="html",  out="Predictive Polarizing Models.htm")



##########################################################################################################################################################################
##Official verus Campaign / Account level Models
govmodel <- felm(pct.polarizing ~ govdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican + Official| congress.x, data = handletweets)
DWmodel <- felm(pct.polarizing ~ DWdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican + Official| congress.x, data = handletweets)
NPmodel <- felm(pct.polarizing ~ NPdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican + Official| congress.x, data = handletweets)



###########################################################################################################################################################################
##Pre-Trump split




stargazer(govmodelpre, DWmodelpre, NPmodelpre, govmodelpost, DWmodelpost, NPmodelpost,
          type="html",  out="Predictive Polarizing Models Trump.htm")