# T1 limesurvey 
T1lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T1_results.csv", stringsAsFactors = F)
T1lime <- T1lime[which(T1lime$Datum.Abgeschickt!=""),]
T1lime$Wann.bist.du.nach.Spanien..oder.Lateinamerika..umgezogen..oder.wann.wirst.du.umziehen..Â.[15] <- "2018-09-01 00:00:00"
T1lime$Wann.bist.du.nach.Spanien..oder.Lateinamerika..umgezogen..oder.wann.wirst.du.umziehen..Â.[159] <- "2018-09-12 00:00:00"

leave <- matrix(NA, nrow(T1lime),4)
leave[,1] <- T1lime$Wann.bist.du.nach.Spanien..oder.Lateinamerika..umgezogen..oder.wann.wirst.du.umziehen..Â.
leave[,2] <- T1lime$Datum.Abgeschickt

leave[,3] <- as.Date(leave[,2])-as.Date(leave[,1])
leavedf <- as.data.frame(leave)
leavedf$V3 <- as.numeric(as.character(leavedf$V3))

leavedf[,4] <- T1lime$ZugangsschlÃ.ssel

#subset this dataframe to only the people that are coded 
library(readxl)
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T3 <- read_excel("T3_PicNaming_DataCoded_new.xlsx", guess_max = 1048576)
T2 <- read_excel("T2_PicNaming_DataCoded.xlsx", guess_max = 1048576)

### clean data ####
# subset to coded people 
T3[T3=='NA'] <- NA
T2[T2=='NA'] <- NA
T3[is.na(T3$error) == 0,]-> T3sub
T2[is.na(T2$error) == 0,]-> T2sub

# exlcude people who have less than 144 trials 
T3sub$ppn <- as.factor(T3sub$ppn)
T2sub$ppn <- as.factor(T2sub$ppn)

nums <- which(table(T2sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  T2sub <- T2sub[-which(T2sub$ppn == nums),]
}
nums <- which(table(T3sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  T3sub <- T3sub[-which(T3sub$ppn == nums),]
}

# subset to people who have been coded for both sessions
T2sub <- T2sub[T2sub$ppn %in% T3sub$ppn,]
T2sub$ppn <- droplevels(T2sub$ppn)
T3sub <- T3sub[T3sub$ppn %in% T2sub$ppn,]
T3sub$ppn <- droplevels(T3sub$ppn)

leavedf <- leavedf[leavedf$V4 %in% T3sub$ppn,]

#check how many people did T1 on time
length(leavedf[which(leavedf$V3>7),3])
