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
library(lfe)
setwd('~/Dropbox/Projects/Twitter/Twitter')


### Load data

# Load tweets data
tweets <- fread("~/Dropbox/Projects/Twitter/Twitter/covariateData/Merged Data/Final Analysis/Divisive/RR Files/activetweets111-116.csv", 
                data.table = FALSE, stringsAsFactors = FALSE)

# Load handles data
handles <- read.csv("~/Dropbox/Projects/Twitter/Twitter/US Congress Handles Master List.csv")
handles <- dplyr::select(handles, icpsr, twitter_lower, Official)

# Merge to tweets data
tweets <- left_join(tweets, handles, by='twitter_lower')


### Create and merge percent different from average engagement by handle-year

# Add year variable
tweets$year <- str_extract(tweets$created_at, '^[0-9]{4}')

# Create year-handle identifier
tweets$yearhandle <- paste0(as.character(tweets$twitter_lower), "-", as.character(tweets$year))

# Create year-name averages for likes and retweets
avgs <- tweets %>%
  group_by(yearhandle) %>%
  dplyr::summarize(
    likeavg = mean(public_metrics.like_count),
    retweetavg = mean(public_metrics.retweet_count)
  ) %>%
  ungroup()

# Merge engagement averages to tweets
tweets <- merge(tweets, avgs, by = "yearhandle")

# Difference from average engagement by handle-year
tweets$likediff <- (tweets$public_metrics.like_count - tweets$likeavg)
tweets$retweetdiff <- (tweets$public_metrics.retweet_count - tweets$retweetavg)

# Turn differences into percentages
tweets$likepct <- ((tweets$public_metrics.like_count - tweets$likeavg) / tweets$likeavg) * 100
tweets$retweetpct <- ((tweets$public_metrics.retweet_count - tweets$retweetavg) / tweets$retweetavg) * 100


### Merge covariates by congress

# NOMINATE data
nominate <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/HSall_members2021.csv")

# Member data by Congress
c111 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 111th.csv")
c112 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 112th.csv")
c113 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 113th.csv")
c114 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 114th.csv")
c115 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 115th.csv")
c116 <- read.csv("~/Dropbox/Projects/Twitter/Twitter/covariateData/RR/Members by Congress 116th.csv")

# Format GovTrack scores
c111$GovTrack <- as.numeric(c111$GovTrack)
c112$GovTrack <- as.numeric(c112$GovTrack)
c113$GovTrack <- as.numeric(c113$GovTrack)
c114$GovTrack <- as.numeric(c114$GovTrack)
c115$GovTrack <- as.numeric(c115$GovTrack)
c116$GovTrack <- as.numeric(c116$GovTrack)

# Distance based on GovTrack scores
c111$govdist <- abs(c111$GovTrack - mean(c111$GovTrack,na.rm=TRUE))
c112$govdist <- abs(c112$GovTrack - mean(c112$GovTrack,na.rm=TRUE))
c113$govdist <- abs(c113$GovTrack - mean(c113$GovTrack,na.rm=TRUE))
c114$govdist <- abs(c114$GovTrack - mean(c114$GovTrack,na.rm=TRUE))
c115$govdist <- abs(c115$GovTrack - mean(c115$GovTrack,na.rm=TRUE))
c116$govdist <- abs(c116$GovTrack - mean(c116$GovTrack,na.rm=TRUE))

# Split out NOMINATE data by Congress
nominate111 <- dplyr::select(filter(nominate, congress == "111"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate112 <- dplyr::select(filter(nominate, congress == "112"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate113 <- dplyr::select(filter(nominate, congress == "113"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate114 <- dplyr::select(filter(nominate, congress == "114"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate115 <- dplyr::select(filter(nominate, congress == "115"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate116 <- dplyr::select(filter(nominate, congress == "116"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)

# Merge member data with NOMINATE data by Congress
m111 <- merge(c111, nominate111, by = "icpsr")
m112 <- merge(c112, nominate112, by = "icpsr")
m113 <- merge(c113, nominate113, by = "icpsr")
m114 <- merge(c114, nominate114, by = "icpsr")
m115 <- merge(c115, nominate115, by = "icpsr")
m116 <- merge(c116, nominate116, by = "icpsr")

# Distance from NOMINATE data
m111$DWdist <- abs(m111$nominate_dim1 - mean(m111$nominate_dim1))
m112$DWdist <- abs(m112$nominate_dim1 - mean(m112$nominate_dim1))
m113$DWdist <- abs(m113$nominate_dim1 - mean(m113$nominate_dim1))
m114$DWdist <- abs(m114$nominate_dim1 - mean(m114$nominate_dim1))
m115$DWdist <- abs(m115$nominate_dim1 - mean(m115$nominate_dim1))
m116$DWdist <- abs(m116$nominate_dim1 - mean(m116$nominate_dim1))

# Distance from Nokken-Poole data
m111$NPdist <- abs(m111$nokken_poole_dim1 - mean(m111$nokken_poole_dim1))
m112$NPdist <- abs(m112$nokken_poole_dim1 - mean(m112$nokken_poole_dim1))
m113$NPdist <- abs(m113$nokken_poole_dim1 - mean(m113$nokken_poole_dim1))
m114$NPdist <- abs(m114$nokken_poole_dim1 - mean(m114$nokken_poole_dim1))
m115$NPdist <- abs(m115$nokken_poole_dim1 - mean(m115$nokken_poole_dim1))
m116$NPdist <- abs(m116$nokken_poole_dim1 - mean(m116$nokken_poole_dim1))

# Merge data from all Congresses
combined <- rbind(m111, m112, m113, m114, m115, m116)

# Create icpsrcongress variable
combined$icpsrcongress <- paste0(as.character(combined$icpsr),"-", as.character(combined$congress))

# Merge to tweets
tweets$icpsrcongress <-  paste0(as.character(tweets$icpsr),"-", as.character(tweets$congress))
tweets <- left_join(tweets, combined, by = "icpsrcongress")

# Filter to Ds and Rs
tweets %<>% 
  filter(party %in% c('D', 'R') & !is.na(party))

# Create indicator for being in the president's party
majDF <- data.frame(
  congress=111:116,
  majH=c('D', rep('R', 4), 'D'),
  majS=c(rep('D', 3), rep('R', 3)),
  pres=c(rep('D', 4), 'R', 'R'),
  stringsAsFactors = FALSE
)
tweets %<>% 
  dplyr::select(-congress.x) %>%
  dplyr::rename(congress=congress.y)
tweets <- left_join(tweets, majDF, by='congress')
tweets %<>% 
  mutate(memberMajH=ifelse(majH==party, 1, 0)) %>%
  mutate(memberMajS=ifelse(majS==party, 1, 0)) %>%
  mutate(memberPresPty=ifelse(pres==party, 1, 0))

# Create ideological extremity variable
ideolMed <- tweets %>%
  group_by(congress) %>%
  dplyr::summarise(
    ideolMed=median(GovTrack, na.rm=TRUE),
    ideolMedD=median(GovTrack[party=='D'], na.rm=TRUE),
    ideolMedR=median(GovTrack[party=='R'], na.rm=TRUE)
  ) %>% 
  ungroup()
tweets <- left_join(tweets, ideolMed, by='congress')
tweets %<>%
  mutate(ideolDiff=GovTrack-ideolMed) %>%
  mutate(absIdeolDiff=abs(ideolDiff))

# Multiply ideolDiff by negative 1 for Dems
tweets$ideolDiffPos <- tweets$ideolDiff
tweets$ideolDiffPos[tweets$party=='D'] <- (-1)*tweets$ideolDiffPos[tweets$party=='D']

# Create House variable
tweets %<>% 
  dplyr::select(-chamber.x) %>%
  dplyr::rename(chamber=chamber.y) %>%
  mutate(house=ifelse(chamber=='House', 1, 0))

# Format seniority variable
tweets %<>% 
  dplyr::rename(years=Seniority)

# Format absolute PVI variable
tweets %<>%
  dplyr::rename(absPVI=PVIABS)

# Format ICPSR variable
tweets %<>% 
  dplyr::select(-icpsr.x) %>% 
  dplyr::rename(icpsr=icpsr.y) %>%
  mutate(icpsr=char(icpsr))



#########################################################################################################################################################################################################################################
### Models predicting exposure

# Run models, save (takes about 40min per model)
mRT <- lm(retweetpct ~ uncivil2 + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tweets)
saveRDS(mRT, file='~/Dropbox/Projects/Twitter/rtMod_uncivil.RData'); rm(mRT); gc()
mFave <- lm(faveDiff ~ uncivil2 + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tweets)
saveRDS(likepct, file='~/Dropbox/Projects/Twitter/faveMod_uncivil.RData'); rm(mFave); gc()
#mRTCiv <- speedlm(rtDiffCiv ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
#saveRDS(mRTCiv, file='~/Dropbox/Projects/Twitter/rtCivMod.RData'); rm(mRTCiv); gc()
#mFaveCiv <- speedlm(faveDiffCiv ~ polarizing + memberPresPty + ideolDiffPos + absPVI + house + female + years + congress + icpsr, data=tw)
#saveRDS(mFaveCiv, file='~/Dropbox/Projects/Twitter/faveCivMod.RData'); rm(mFaveCiv); gc()
mRT <- readRDS('~/Dropbox/Projects/Twitter/rtMod_uncivil.RData')
mFave <- readRDS('~/Dropbox/Projects/Twitter/faveMod_uncivil.RData')

# Regression table
stargazer(mFave, mRT, font.size='small', label='exposureTab', colnames=FALSE,
          keep=c('uncivil', 'memberPresPty', 'absPVI', 'house', 'female', 'ideolDiffPos', 'years', 
                 'congress114', 'congress115'),
          covariate.labels=c('Uncivil', "President's Party", 'Ideological Extremity', '|PVI|', 'House', 'Female', 'Seniority',
                             '114th Congress', '115th Congress'),
          keep.stat=c('n', 'adj.rsq'), dep.var.labels = c('Difference from Average Likes', 'Difference from Average Retweets'),
          title="OLS models predicting the number of likes (column 1) and retweets (column 2) relative to the average tweet for each member.")



###################################################################################################################################################################################################################################
### Effects plots

# Pull effect size (coefficient in LMs) and CI (1.96*SE)
effPlotDat <- function(model){
  # Pull coefficient
  coefs <- summary(model)$coefficients
  coef <- coefs['uncivil2','Estimate']
  # Pull CI
  ci <- coefs['uncivil2', 'Std. Error']*1.96
  # Combine/return
  effs <- c(coef=coef, ci=ci)
  effs
}
effs <- sapply(list(mFave, mRT), function(x){
  effPlotDat(x)
}) %>% 
  t()

# Plot and save
barCenters <- barplot(effs[,1], ylim=c(0, 1000))
pdf('~/Dropbox/Projects/Twitter/engageMods_effects_incivility.pdf', width=6, height=6)
par(mar=c(3.1, 4.1, 2.1, 1.1))
barplot(effs[,1], ylim=c(0, 1000), ylab='Effect of Political Incivility', names.arg=c('Likes above\nAverage', 'Retweets above\nAverage'))
for(ii in 1:2){
  segments(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3)
  arrows(x0=barCenters[ii], y0=effs[ii,1]-effs[ii,2], y1=effs[ii,1]+effs[ii,2], lwd=3, angle=90, code=3, length=0.05)  
  text(x=barCenters[ii], y=effs[ii,1]/2, paste0(round(effs[ii,1], 2), ' +/- ', round(effs[ii,2], 2)))
}
dev.off()
par(mar=c(5.1, 4.1, 4.1, 2.1))



###############################################################################################################################################################################
### Effects of polarizing rhetoric on engagement in each year

# Numeric year variable for 
tweets$years_num <- num(tweets$year)

# Set up empty lists to save models
years <- 2010:2020
likes_mods <- vector(mode='list', length=d(years))
rts_mods <- vector(mode='list', length=d(years))

# Run models
for(ii in seq_along(years)){
  mod_df <- tweets[tweets$years_num==years[ii],]
  likes_mod <- felm(likepct ~ uncivil2 | yearhandle + created_at, data = mod_df)
  rts_mod <- felm(retweetpct ~ uncivil2 | yearhandle + created_at, data = mod_df)
  likes_mods[[ii]] <- likes_mod
  rts_mods[[ii]] <- rts_mod
}

stargazer(likes10, likes11, likes12, likes13, likes14, likes15, likes16, likes17, 
          likes18, likes19, likes20, rts10, rts11, rts12, rts13, rts14, rts15, rts16, rts17, rts18, rts19, rts20,
          out="Engagement Tweet Models Yearly.tex")

#### extract coefficients and errors

effs_likes <- sapply(likes_mods, function(x){
  effPlotDat(x)
}) %>% 
  t()
effs_rts <- sapply(rts_mods, function(x){
  effPlotDat(x)
}) %>% 
  t()

# Add year and CI range values to effects data
effs_likes <- as.data.frame(effs_likes) %>%
  mutate(year=years,
         ci_lo=coef-ci,
         ci_hi=coef+ci) 
effs_rts <- as.data.frame(effs_rts) %>%
  mutate(year=years,
         ci_lo=coef-ci,
         ci_hi=coef+ci)

# Add overall effect to yearly effects data
effs_likes$eff_ovr <- effs[1,'coef']
effs_likes$ci_lo_ovr <- effs[1,'coef']-effs[1,'ci']
effs_likes$ci_hi_ovr <- effs[1,'coef']+effs[1,'ci']
effs_rts$eff_ovr <- effs[2,'coef']
effs_rts$ci_lo_ovr <- effs[2,'coef']-effs[2,'ci']
effs_rts$ci_hi_ovr <- effs[2,'coef']+effs[2,'ci']


### Make effects plots

# Likes
likes_plot <- ggplot(data=effs_likes, mapping=aes(year, coef)) + 
  geom_hline(yintercept=0, linetype='dotdash') +
  geom_point(size=3) +
  geom_smooth(method='loess', se=FALSE, color='black', linetype='dashed') +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), width=0.2, size=1) +
  ylab('Effect of Political Incivility') +
  xlab('') +
  ggtitle('Likes above Average') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  ylim(-100, 300) +
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(breaks=seq(-100, 300, 20)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x=element_blank())

# Retweets
rts_plot <- ggplot(data=effs_rts, mapping=aes(year, coef)) + 
  geom_hline(yintercept=0, linetype='dotdash') +
  geom_point(size=3) +
  geom_smooth(method='loess', se=FALSE, color='black', linetype='dashed') +
  geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), width=0.2, size=1) +
  ylab('Effect of Political Incivility') +
  xlab('') +
  ggtitle('Retweets above Average') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  ylim(-100, 200) +
  scale_x_continuous(breaks=seq(2010, 2020, 1)) +
  scale_y_continuous(breaks=seq(-100, 200, 20)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x=element_blank())

# Plot together
pdf(file='~/Dropbox/Projects/Twitter/incivility_engagement_effects_over_time.pdf', width=6, height=6)
cowplot::plot_grid(likes_plot, rts_plot, ncol=1)
dev.off()
