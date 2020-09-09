library(ggplot2)
library(dplyr)
library(lubridate)
library(plm)

library(stargazer)
library(sjPlot)
library(MatchIt)
library(Matching)
library(sandwich)
library(AER)
library(estimatr)
library(stargazer)
library(jtools)
library(rgenoud)
library(plm)
library(gridExtra)
library(ggpubr)


boatwright <- read.csv("C:/Users/User/Dropbox/MyData/DataSets/Boatwright Data/Boatwright Candidates 2018.csv")

TwitterTrim <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/TrimmedHandles.csv")

boatwright <- filter(boatwright, elect_year > 2005)
boatwright <- filter(boatwright, office_id > 1)
boatwright <- filter(boatwright, office_id < 6)

TwitterTrim <- select(TwitterTrim, icpsr, first, last, state, chamber, district, party, female)

boattrim<- select(boatwright, icpsr, name, short_name, office_id, general_result, elect_year, cong, state_postal, 
                      state_name, seniority, bonica_score, dwnom1, Dsex, Dethnicity, Pplace, Pelect_date, 
                      Pcontested, Pwinner, Pvote_share, Gplace, Gelect_date, Gcontested, Gmargin_pct, Gvote_share,
                      Gvote_share_2pty, Gvote_share_2pty_lagged)

boatmerge <- merge(TwitterTrim, boattrim, by = "icpsr")

write.csv(boatmerge, file = "boatmerge.csv")




daytweets <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/handle-day.csv")

Twitters <- read.csv("C:/Users/User/Dropbox/Twitter/master_handles w FEC and Official Tag.csv")

Twitters <- select(Twitters, icpsr, twitter)

daysmerged <- merge(daytweets, Twitters, by.x = "screen_name", by.y = "twitter")

write.csv(daysmerged, file = "daysicpsr.csv")




tweets <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/preelectiontweetsgrouped.csv")


boat <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/boatmerge.csv") 


electionweektweets <- merge(tweets, boat, by = "yearicpsr")

write.csv(electionweektweets, file = "electionweektweetsresults.csv")


####Analysis

results <- read.csv("C:/Users/User/Dropbox/Twitter/covariateData/preelectiontweetsresults.csv")

results <- filter(results, elect_year > 2008)
results <- filter(results, Gvote_share_2pty_lagged < 1)
results <- filter(results, Gvote_share_2pty < 1)

results$Gvote_share_2pty_diff <- as.numeric(results$Gvote_share_2pty_diff)

mod1 <- plm(Gvote_share_2pty_diff ~ tweets_diff, data = results, index = c("icpsr.x", "yearparty"), model = "within", effect = "twoways")
mod2 <- plm(Gvote_share_2pty_diff ~ civil_diff, data = results, index = c("icpsr.x", "yearparty"), model = "within", effect = "twoways")
mod3 <- plm(Gvote_share_2pty_diff ~ incivil_diff, data = results, index = c("icpsr.x", "yearparty"), model = "within", effect = "twoways")
mod4 <- plm(Gvote_share_2pty_diff ~ incivilpp_diff_scale, data = results, index = c("icpsr.x", "yearparty"), model = "within", effect = "twoways")

tab_model(mod1, mod2, mod3, mod4)

stargazer(mod1, mod2, mod3, mod4, Title = "Cycle-on-cycle Vote Share Change, Predicted by Pre-Election Twitter Activity", type="html", out="testelectionsresults.htm")

plot1 <- plot_coefs(mod1,
                    coefs=c(`Difference in Total Tweets`='tweets_diff'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('') +
  xlim(-5, 5) +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- plot_coefs(mod2,
                    coefs=c(`Difference in Civil Tweets   `='civil_diff'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('') +
  xlim(-5, 5) +
  theme(plot.title = element_text(hjust = 0.5))

plot3 <- plot_coefs(mod3,
                    coefs=c(`Difference in Uncivil Tweets`='incivil_diff'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('Change in 2-Party Vote Share') +
  xlim(-5, 5) +
  theme(plot.title = element_text(hjust = 0.5))

plot4 <- plot_coefs(mod4,
                    coefs=c(`Change in Percentage Uncivl`='incivilpp_diff_scale'),
                    scale=TRUE,
                    inner_ci_level=0.9,
                    colors=c('black')) +
  xlab('Change in 2-Party Vote Share') +
  xlim(-5, 5) +
  theme(plot.title = element_text(hjust = 0.5))

resultscombined <- ggarrange(plot1, plot2, plot3, plot4,
                                   labels = c("", ""),
                                   ncol = 2, nrow = 2, legend = NULL)
resultscombined


####Matching
matchresults <- dplyr::select(results, incivil_diff, incivil_lag, Gvote_share_2pty_lagged, Gvote_share_2pty_diff, female, elect_year, republican, house)

m.out <- matchit(incivil_diff ~ elect_year + republican + house + incivil_lag + Gvote_share_2pty_lagged, data = matchresults)
matchedresults <- match.data(m.out)

rescale!

