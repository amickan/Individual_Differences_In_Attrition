### Doors performance ####

library(plyr)
library(dplyr)
library(here)

### Read in the local participant database
reg <- read.delim("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION/REGISTRATION.txt", header = T, stringsAsFactors = F)

### check whether Pic naming and fluency and doors for T3 have been completed
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION/LogfilesT3")
filescsv <- file.info(list.files(getwd(),pattern = "data.*.csv"))
comptasks <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F, sep = ";")
doors <- comptasks[grep("doors",comptasks$fileName), ]

# subset to relevant information 
doorssub <- doors[is.na(doors$score_response)==0,]

# check if some people did this test multiple times 
table(doorssub$fileName)

# only select the first instance of each trials for each person 
finaldoors <- tbl_df(doorssub) %>% arrange(fileName, trial_num, time_elapsed) %>% group_by(fileName, trial_num) %>% filter(time_elapsed==min(time_elapsed))

# calculate scores per person 
scores<- tapply(finaldoors$score_response, finaldoors$fileName, mean)
hist(scores)
scores <- as.data.frame(scores)
scores[,2] <- as.numeric(gsub("_doors.dat", "",row.names(scores)))
colnames(scores) <- c("score", "ppn")

## add these scores to the individual differences data frame 
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")
## add predictors to the full dataframe 
colnames(individualdiff)[1] <- "ppn"
for (i in 1:nrow(individualdiff)){
  num <- which(scores$ppn == individualdiff$ppn[i])
  if (length(num)==1){
    individualdiff$DoorsScore[i] <- scores$score[num]
  } else {
    individualdiff$DoorsScore[i] <- NA
  }
}
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
write.table(individualdiff, "T1_T2_T3_lime_clean.txt", sep = "\t", quote = F, row.names = F)


#for (i in 1:nrow(individualdiff)){
#  num <- which(T2per$V2 == individualdiff$ppn[i])
#  if (length(num)==1){
#    individualdiff$T2perf[i] <- T2per$T2per[num]
#  } else {
#    individualdiff$T2perf[i] <- NA
#  }
#}

individualdiff$Mot_T1_pos_avg <- (individualdiff$Mot_T1_attitude + individualdiff$Mot_T1_instrumental+ individualdiff$Mot_T1_interest+ individualdiff$Mot_T1_integrative)/4
individualdiff$Mot_T2_pos_avg <- (individualdiff$Mot_T2_attitude + individualdiff$Mot_T2_instrumental+ individualdiff$Mot_T2_interest+ individualdiff$Mot_T2_integrative)/4
individualdiff$Mot_T3_pos_avg <- (individualdiff$Mot_T3_attitude +individualdiff$Mot_T3_instrumental+ individualdiff$Mot_T3_interest+ individualdiff$Mot_T3_integrative)/4
