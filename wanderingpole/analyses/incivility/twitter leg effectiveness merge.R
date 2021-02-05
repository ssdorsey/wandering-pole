library(dplyr)
Twitter <- read.csv("C:/Users/User/Dropbox/Twitter/master_handles w FEC.csv")
LegEff <- read.csv("C:/Users/User/Dropbox/MyData/DataSets/111th Leg Effect.csv")

merge <- merge(Twitter, LegEff, by = "icpsr")

write.csv(merge, file = "111thTwitterInfoall.csv")

# .... Repeat for each Congress

LegEff <- read.csv("C:/Users/User/Dropbox/MyData/DataSets/112th Leg Effect.csv")

merge <- merge(Twitter, LegEff, by = "icpsr")

write.csv(merge, file = "112thTwitterInfo.csv")

LegEff <- read.csv("C:/Users/User/Dropbox/MyData/DataSets/113th Leg Effect.csv")

merge <- merge(Twitter, LegEff, by = "icpsr")

write.csv(merge, file = "113thTwitterInfo.csv")

LegEff <- read.csv("C:/Users/User/Dropbox/MyData/DataSets/114th Leg Effect.csv")

merge <- merge(Twitter, LegEff, by = "icpsr")

write.csv(merge, file = "114thTwitterInfo.csv")

LegEff <- read.csv("C:/Users/User/Dropbox/MyData/DataSets/115th Leg Effect.csv")

merge <- merge(Twitter, LegEff, by = "icpsr")

write.csv(merge, file = "115thTwitterInfo.csv")