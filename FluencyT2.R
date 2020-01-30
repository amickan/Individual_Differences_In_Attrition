### Fluency T2 ###

# read in data 
library(readxl)
setwd("//ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T2flu <- read_xlsx("Fluency_all.xlsx", sheet = "T2_fluency_all")

ppn <- read.delim("PPN_final.txt", header = F)

# subset T2 to only the people that are in ppn 
T2sub <- T2flu[which(T2flu$PP %in% ppn$V1),]
T2sub$PP <- as.factor(T2sub$PP)
nums <- as.data.frame(unclass(table(T2sub$PP, T2sub$`Category/Letter`)))
nums[nums$Freq == 0,]

cor(nums)
# all scores correlate positively 
