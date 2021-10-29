library(ggplot2)
library(dplyr)
library(stargazer)
library(sjPlot)
library(jtools)
library(lfe)
library(data.table)
library(lubridate)
library(Hmisc)

##Covariate Merging by Congress

nominate <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/RR/HSall_members2021.csv")

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

nominate111 <- select(filter(nominate, congress == "111"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate112 <- select(filter(nominate, congress == "112"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate113 <- select(filter(nominate, congress == "113"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate114 <- select(filter(nominate, congress == "114"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate115 <- select(filter(nominate, congress == "115"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)
nominate116 <- select(filter(nominate, congress == "116"), icpsr, chamber, nominate_dim1, nokken_poole_dim1)

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
    polarizing = sum(polarizing)
  ) %>%
  ungroup()

handles <- read.csv("C:/Users/User/Dropbox/Twitter/US Congress Handles Master List.csv")

handles <- select(handles, icpsr, twitter_lower)

congresstweets <- merge(congresstweets, handles, by = "twitter_lower")

congresstweets <- congresstweets %>%
  group_by(icpsr, congress) %>%
  dplyr::summarize(
    tweets = sum(tweets),
    polarizing = sum(polarizing)
  ) %>%
  ungroup()

congresstweets$pct.polarizing <- ((congresstweets$polarizing / congresstweets$tweets) *100)

congresstweets$icpsrcongress <-  paste0(as.character(congresstweets$icpsr),"-", as.character(congresstweets$congress))

##Percentage Polarizing Models

congresstweets <- merge(congresstweets, combined, by = "icpsrcongress")

congresstweets$republican <- ifelse(congresstweets$party == "R",1,0)

govmodel <- felm(pct.polarizing ~ govdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican | congress.x, data = congresstweets)
DWmodel <- felm(pct.polarizing ~ DWdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican| congress.x, data = congresstweets)
NPmodel <- felm(pct.polarizing ~ NPdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican| congress.x, data = congresstweets)

govmodel2 <- felm(pct.polarizing ~ govdist + Pres.Party + PVIABS + Chamber.Majority | congress.x + icpsr.x, data = congresstweets)
DWmodel2 <- felm(pct.polarizing ~ DWdist + Pres.Party + PVIABS + Chamber.Majority | congress.x + icpsr.x, data = congresstweets)
NPmodel2 <- felm(pct.polarizing ~ NPdist + Pres.Party + PVIABS + Chamber.Majority | congress.x + icpsr.x, data = congresstweets)

govmodel3 <- lm(pct.polarizing ~ govdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican, data = congresstweets)
DWmodel3 <- lm(pct.polarizing ~ DWdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican, data = congresstweets)
NPmodel3 <- lm(pct.polarizing ~ NPdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female + republican, data = congresstweets)


stargazer(govmodel, DWmodel, NPmodel, govmodel2, DWmodel2, NPmodel2, govmodel3, DWmodel3, NPmodel3,
          type="html",  out="Predictive Polarizing Models.htm")


##Pre-Trump split

congresstweetspre <- filter(congresstweets, congress.x < 115)
congresstweetspost <- filter(congresstweets, congress.x > 114)

govmodelpre <- felm(pct.polarizing ~ govdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female | congress.x, data = congresstweetspre)
DWmodelpre <- felm(pct.polarizing ~ DWdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female | congress.x, data = congresstweetspre)
NPmodelpre <- felm(pct.polarizing ~ NPdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female | congress.x, data = congresstweetspre)

govmodelpost <- felm(pct.polarizing ~ govdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female | congress.x, data = congresstweetspost)
DWmodelpost <- felm(pct.polarizing ~ DWdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female | congress.x, data = congresstweetspost)
NPmodelpost <- felm(pct.polarizing ~ NPdist + Pres.Party + PVIABS + Chamber.Majority + chamber.y + female | congress.x, data = congresstweetspost)


stargazer(govmodelpre, DWmodelpre, NPmodelpre, govmodelpost, DWmodelpost, NPmodelpost,
          type="html",  out="Predictive Polarizing Models Trump.htm")
