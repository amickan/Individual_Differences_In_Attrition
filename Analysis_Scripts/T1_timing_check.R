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
ppn <- read.delim("PPN_final.txt", header = F)

leavedf <- leavedf[leavedf$V4 %in% ppn$V1,]
colnames(leavedf) <- c("MovedToSpain", "T1date", "TimingT1", "ppn")

# check how many people did T1 on time
hist(leavedf$V3)
length(leavedf[which(leavedf$V3>14),3])


### T2 timing 
T2lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T2_results_new.csv", stringsAsFactors = F)
T2lime <- T2lime[which(T2lime$Datum.Abgeschickt!=""),]
leavedf$T2date <- NA
leavedf$LeaveSpain <- NA
leavedf$T2timing <- NA
for (i in 1:nrow(leavedf)){
  num <- which(T2lime$ZugangsschlÃ.ssel == leavedf$ppn[i])
  leavedf$T2date[i] <- T2lime$Datum.Abgeschickt[num]
  leavedf$LeaveSpain[i] <- T2lime$Wann.genau.hast.du.Spanien.endgÃ.ltig.verlassen..bzw..wirst.du.Spanien..verlassen.[num]
  leavedf$T2timing[i] <- as.Date(leavedf$T2date[i])-as.Date(leavedf$LeaveSpain[i])
}

# how many people did T2 before Christmas? 
nrow(leavedf[which(leavedf$T2date < as.Date("2018-12-24")),])
# how many did T2 within their last two weeks in Spain 
subset <- leavedf[-which(leavedf$T2date < as.Date("2018-12-24")),]
nrow(subset[subset$T2timing > 0,])

leavedf$differenceT1T2 <- NA
leavedf$differenceT1T2 <- as.Date(leavedf$T2date)-as.Date(leavedf$T1date)
sd(leavedf$differenceT1T2)

### T3 timing 
T3lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T3_results_new.csv", stringsAsFactors = F)
T3lime$Datum.Abgeschic...[146] <- T3lime$Datum.letzte.Ak...[146]
T3lime <- T3lime[which(T3lime$Datum.Abgeschic...!=""),]
leavedf$T3date <- NA
leavedf$T3timing <- NA
for (i in 1:nrow(leavedf)){
  num <- which(T3lime$ZugangsschlÃ.ss... == leavedf$ppn[i])
  leavedf$T3date[i] <- T3lime$Datum.Abgeschic...[num]
  leavedf$T3timing[i] <- as.Date(leavedf$T3date[i])-as.Date(leavedf$T2date[i])
}
