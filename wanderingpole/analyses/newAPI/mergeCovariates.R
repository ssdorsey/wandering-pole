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
setwd('~/Dropbox/Projects/Twitter/Twitter')

# Load classified tweets
tw <- fread('~/Dropbox/Projects/Twitter/tweets_newapi.csv', data.table=FALSE, stringsAsFactors=FALSE)
tw %<>% 
  mutate(screen_name=tolower(author_username)) %>%
  filter(screen_name!='') %>%
  dplyr::rename(retweets=public_metrics.retweet_count,
                favorites=public_metrics.like_count)

# Load master list of handles to merge with member data
hand <- fread('master_handles.csv', data.table = FALSE, stringsAsFactors = FALSE) %>%
  dplyr::rename(screen_name=twitter) %>%
  mutate(screen_name=tolower(screen_name))
hand$district[hand$chamber=='Senate'] <- 0
hand$district[hand$state %in% c('AK', 'DE', 'MT', 'ND', 'SD', 'VT', 'WY')] <- 1

# Merge by handle
tw <- left_join(tw, hand, by='screen_name')

# Load data on members to merge in
mcs <- readRDS('Data/mergedMemberERData.rds')

# Convert date in TW to a date
tw$datetime <- ymd_hms(tw$created_at)
tw$date <- date(tw$datetime)
tw %<>% filter(!is.na(date))

# Add Congress to tw
dates <- list(`110`=interval(ymd('2007-01-03'), ymd('2009-01-02')),
              `111`=interval(ymd('2009-01-03'), ymd('2011-01-02')),
              `112`=interval(ymd('2011-01-03'), ymd('2013-01-02')),
              `113`=interval(ymd('2013-01-03'), ymd('2015-01-02')),
              `114`=interval(ymd('2015-01-03'), ymd('2017-01-02')),
              `115`=interval(ymd('2017-01-03'), ymd('2019-01-02')),
              `116`=interval(ymd('2019-01-03'), ymd('2021-01-02')))
tw$congress <- NA
for(ii in 1:d(dates)){
  cong <- names(dates)[ii] %>% num()
  tw$congress[tw$date %within% dates[[ii]]] <- cong
}

# Filter to 110th-115th congresses
tw %<>% filter(congress %in% 110:115)
mcs %<>% filter(congress %in% 110:115)

# Are there any duplicated mc entries?
mcs %<>% mutate(id=paste0(congress, '_', icpsr))
dupe_ids <- filter(mcs, duplicated(id)) %>% pull(id)
dupes <- filter(mcs, id %in% dupe_ids) # 9 duplicates, 18 rows

# Which of the duplicates are because somebody was in both chambers in the same congress?
promote <- dupes %>%
  group_by(id) %>%
  dplyr::summarise(
    both=ifelse(d(unique(chamber))>1, 1, 0)
  ) %>%
  ungroup()

# Deal with these by hand
mcs <- filter(mcs, !id %in% dupe_ids)

dupes <- dupes %>%
  # For LA 3rd (Clay Higgins) use 77,671 for repvotes
  filter(!(state_abbrev=='LA' & year=='2016' & district==3 & repvotes=='91,532')) %>% 
  # For LA 4th (Mike Johnson) use 87,370 for repvotes
  filter(!(state_abbrev=='LA' & year=='2016' & district==4 & repvotes=='70,580')) %>% 
  # For LA Senate (Bill Cassidy) use 536,191 for repvotes
  filter(!(state_abbrev=='LA' & year=='2016' & district==0 & icpsr==20919 & repvotes=='482,591')) %>% 
  # For LA Senate (John Kennedy) use 536,191 for repvotes
  filter(!(state_abbrev=='LA' & year=='2016' & district==0 & icpsr==41703 & repvotes=='482,591')) 

mcs <- rbind(mcs, dupes)

# Load in PVI data for 115th, format to merge with mcs
pvi <- fread('pviData115.csv', data.table=FALSE, stringsAsFactors=FALSE) %>%
  dplyr::select(-Incumbent) %>%
  dplyr::rename(clinton16=`Clinton %`) %>%
  dplyr::rename(trump16=`Trump %`) %>%
  dplyr::rename(obama12=`Obama %`) %>%
  dplyr::rename(romney12=`Romeny %`) %>%
  mutate(state_abbrev=str_extract(Dist, '^[A-Z]{2}')) %>%
  mutate(district=str_extract(Dist, '[0-9]{2}') %>% num()) %>% 
  dplyr::select(-Dist) %>%
  dplyr::rename(pvi=PVI) %>%
  mutate(pvi=str_replace(pvi, 'R\\+', '-')) %>%
  mutate(pvi=str_replace(pvi, 'D\\+', '')) %>%
  mutate(pvi=str_replace(pvi, 'EVEN', '0')) %>%
  mutate(pvi=num(pvi)) %>%
  mutate(district=car::recode(district, "NA=1"))

# Merge pvi and mcs
mcs <- left_join(mcs, pvi, by=c('state_abbrev', 'district'))

# Fill in missing House NAs for the 115th House
mcs$district_pvi[is.na(mcs$district_pvi) & mcs$chamber=='house' & mcs$congress==115] <- mcs$pvi[is.na(mcs$district_pvi) & mcs$chamber=='house' & mcs$congress==115]

# Load historical PVI (pviH[istorical]) data, merge with mcs
pviH <- fread('PVI_data_104to114.csv', data.table=FALSE, stringsAsFactors=FALSE) %>%
  dplyr::rename(pviH=pvi) %>%
  filter(congress %in% 110:115) %>%
  mutate(state_abbrev=state.abb[match(state, state.name)]) %>% 
  dplyr::select(-state) #use abbreviation because `state` has missingness in `mcs`
mcs <- left_join(mcs, pviH, by=c('congress', 'chamber', 'district', 'state_abbrev'))

# Fill in missing House NAs for PVI (110-114)
mcs$district_pvi[is.na(mcs$district_pvi) & mcs$chamber=='house'] <- mcs$pviH[is.na(mcs$district_pvi) & mcs$chamber=='house']

# Fill in missing Senate NAs for PVI
mcs$district_pvi[is.na(mcs$district_pvi) & mcs$chamber=='senate'] <- mcs$pviH[is.na(mcs$district_pvi) & mcs$chamber=='senate']

# For those still missing PVI values for the Senate, us the average PVI in their state (all in 115th)
avgPVI <- mcs %>%
  filter(congress==115 & chamber=='house') %>%
  group_by(state_abbrev) %>%
  dplyr::summarise(avgPVI=mean(district_pvi, na.rm=TRUE) %>% round()) %>%
  ungroup() %>%
  mutate(congress=115)
mcs <- left_join(mcs, avgPVI, by=c('state_abbrev', 'congress'))
mcs$district_pvi[is.na(mcs$district_pvi)] <- mcs$avgPVI[is.na(mcs$district_pvi)]

# Clean up extra PVI values (leaving only district_pvi, renaming to pvi)
mcs %<>% dplyr::select(-c(pvi, pviH, avgPVI)) %>%
  dplyr::rename(pvi=district_pvi)

# Fix state NAs in mcs
mcs %<>% mutate(state=state.name[match(state_abbrev, state.abb)])

# Merge tw and mcs
tw <- mcs %>%
  dplyr::rename(elect_year=year) %>%
  dplyr::select(-c(state, chamber, district)) %>%
  left_join(tw, ., by=c('icpsr', 'congress'))

# Fix party variable
tw %<>% mutate(party=car::recode(party, "'I'='D'; ''=NA"))

# Majority party variables (house, senate, white house)
majDF <- data.frame(
  congress=110:115,
  majH=c(rep('D', 2), rep('R', 4)),
  majS=c(rep('D', 4), rep('R', 2)),
  pres=c('R', rep('D', 4), 'R'),
  stringsAsFactors = FALSE
)
tw <- left_join(tw, majDF, by='congress')

# Member of majority in H, S, WH
tw %<>% mutate(memberMajH=ifelse(majH==party, 1, 0)) %>%
  mutate(memberMajS=ifelse(majS==party, 1, 0)) %>%
  mutate(memberPresPty=ifelse(pres==party, 1, 0))

# Unified government dummy
tw %<>% mutate(unified=ifelse(majH==majS & majH==pres, 1, 0))

# Cut out 110th congress (only 4119 tweets)
tw %<>% filter(congress != 110)

# Party center for DW-NOMINATE
dwnom <- tw %>%
  group_by(party, congress) %>%
  dplyr::summarise(
    med=median(dim1, na.rm=TRUE)
  ) %>%
  ungroup %>%
  filter(!is.na(party))
tw <- left_join(tw, dwnom, by=c('congress', 'party'))
tw %<>% mutate(dwDist=dim1-med) %>%
  mutate(absDWDist=abs(dim1-med))

# Make PVI absolute value
tw %<>% mutate(absPVI=abs(pvi))

# Cut out NAs in district_pvi (these are because the member wasn't in congress at the time but was tweeting--i.e. they kept the same account)
tw %<>% filter(!is.na(pvi))


### Add Govtrack ideology scores for 113th-115th

# Load data
ideol <- fread('~/Dropbox/Projects/Twitter/ideology_master_congress_113_114_115.csv', data.table=FALSE, stringsAsFactors=FALSE)

# Clean
ideol %<>% 
  mutate(name=str_replace(name, "^b'", '') %>% str_replace(., "'$", '')) %>%
  mutate(house=House) %>%
  mutate(congress=Congress) %>%
  dplyr::select(-c(state, district, name, House, Senate, id, Congress))

# Join to twitter data
tw <- left_join(tw, ideol, by=c('bioguide_id', 'congress'))

# Pull medians for each congress and party/congress
ideolMed <- tw %>%
  group_by(congress) %>%
  dplyr::summarise(
    ideolMed=median(ideology, na.rm=TRUE),
    ideolMedD=median(ideology[party=='D'], na.rm=TRUE),
    ideolMedR=median(ideology[party=='R'], na.rm=TRUE)
  ) %>% 
  ungroup()

# Join ideology medians to tw
tw <- left_join(tw, ideolMed, by='congress')

# Compute difference (and absolute difference) from chamber and party medians
tw %<>%
  mutate(ideolDiff=ideology-ideolMed) %>%
  mutate(absIdeolDiff=abs(ideolDiff))
tw$ideolDiffPty <- NA
tw$ideolDiffPty[tw$party=='D'] <- tw$ideology[tw$party=='D']-tw$ideolMedD[tw$party=='D']
tw$ideolDiffPty[tw$party=='R'] <- tw$ideology[tw$party=='R']-tw$ideolMedR[tw$party=='R']
tw$absIdeolDiffPty <- NA
tw$absIdeolDiffPty[tw$party=='D'] <- abs(tw$ideolDiffPty[tw$party=='D'])
tw$absIdeolDiffPty[tw$party=='R'] <- abs(tw$ideolDiffPty[tw$party=='R'])

# Multiply ideolDiff by negative 1 for Dems
tw$ideolDiffPos <- tw$ideolDiff
tw$ideolDiffPos[tw$party=='D'] <- (-1)*tw$ideolDiffPos[tw$party=='D']

# Clean up variables for modeling
tw %<>% 
mutate(congress=factor(congress)) %>%
  mutate(icpsr=factor(icpsr)) %>%
  mutate(chamber=car::recode(chamber, "'House'=1; 'Senate'=0")) %>%
  dplyr::select(-house) %>%
  dplyr::rename(house=chamber)

# Average number of likes and retweets per member (on all tweets and on "civil" tweets)
avgEngage <- tw %>%
  group_by(screen_name) %>%
  dplyr::summarise(
    avgRT=mean(retweets),
    avgFave=mean(favorites),
    avgRTCiv=mean(retweets[polarizing==0]),
    avgFaveCiv=mean(favorites[polarizing==0])
  ) %>% 
  ungroup()

# Average number of likes and retweets per member per congress
engageCong <- tw %>%
  group_by(screen_name, congress) %>%
  dplyr::summarise(
    avgRTCong=mean(retweets),
    avgFaveCong=mean(favorites),
    avgRTCivCong=mean(retweets[polarizing==0]),
    avgFaveCivCong=mean(favorites[polarizing==0])
  ) %>%
  ungroup()

# Join to tw
tw <- left_join(tw, avgEngage, by='screen_name')
tw <- left_join(tw, engageCong, by=c('screen_name', 'congress'))

# Compute difference between the likes/RTs on each tweet and the averages
tw %<>%
  mutate(
    rtDiff = retweets - avgRT,
    rtDiffCiv = retweets - avgRTCiv,
    faveDiff = favorites - avgFave,
    faveDiffCiv = favorites - avgFaveCiv,
    rtDiffCong = retweets - avgRTCong,
    rtDiffCivCong = retweets - avgRTCivCong,
    faveDiffCong = favorites - avgFaveCong,
    faveDiffCivCong = favorites - avgFaveCivCong
  )

# Load gender data
gender <- read.csv('covariateData/trimmedHandles.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(c(icpsr, female)) %>%
  mutate(icpsr=as.factor(icpsr))

# Join to tw
tw <- left_join(tw, gender, by='icpsr')

# Load seniority data, subset, convert terms to years, join House and Senate
house <- fread('covariateData/house_terms.csv', stringsAsFactors = FALSE, data.table=FALSE) %>%
  mutate(chamber='House') %>%
  mutate(years=terms*2)
senate <- fread('covariateData/senate_terms.csv', stringsAsFactors = FALSE, data.table=FALSE) %>%
  mutate(chamber='Senate') %>%
  mutate(years=terms*6)
seniority <- rbind(house, senate) %>%
  dplyr::select(c(id.icpsr, year, terms, years, chamber)) %>%
  dplyr::rename(icpsr=id.icpsr) %>%
  filter(year >= 2009 & year <= 2018) %>%
  mutate(icpsr=as.factor(icpsr))

# Convert year to Congress, drop year, finish formatting to merge with tw
congs <- num(sort(unique(tw$congress)))
years <- sort(unique(seniority$year))
seniority$congress <- NA
for(ii in 1:d(years)){
  index <- ceiling(ii/2)
  seniority$congress[seniority$year==years[ii]] <- congs[index]
}
seniority %<>% 
  dplyr::select(-year) %>%
  mutate(chamber=car::recode(chamber, "'House'=1; 'Senate'=0")) %>%
  dplyr::rename(house=chamber) %>% 
  mutate(congress=as.factor(congress)) %>% 
  distinct()

# Join to tw
tw <- left_join(tw, seniority, by=c('icpsr', 'congress', 'house'))

# Save
saveRDS(tw, '~/Dropbox/Projects/Twitter/modelData_polarizing_newAPI.rds')
