### checking for correlations of fixed effects

library(ggcorrplot)

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
df <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

## add predictors to the full dataframe 
colnames(df)[1] <- "ppn"
df$GermanEngRatio_T2_T3 <- df$T2_T3_English/df$T2_T3_German
df$Mot_T2_T3_avg         <- (df$Mot_T3_pos_avg+df$Mot_T2_pos_avg)/2
df$IntegMot_T2_T3_avg    <- ((df$Mot_T2_attitude + df$Mot_T2_integrative + df$Mot_T2_interest)/3 + (df$Mot_T3_attitude + df$Mot_T3_integrative+ df$Mot_T3_interest)/3)/2
#df$Mot_T2_T3_diff       <- df$Mot_T3_pos_avg-df$Mot_T2_pos_avg
df$SRP_Spanish_T2_T3     <- df$T3_SRP_Spanish_avg - df$T2_SRP_Spanish_avg
df$Immersion             <- (df$T3_LivingSitGermany_Spanish + df$T3_WatchSpanishMovies + df$T3_ReadSpanishBooks)/3
df$Mot_T2_T3_anxiety_avg <- (df$Mot_T3_anxiety+df$Mot_T2_anxiety)/2
df$Mot_T2_T3_instr_avg   <- (df$Mot_T3_instrumental+df$Mot_T2_instrumental)/2


# subset this dataframe to people who have been coded 
# subset to people who are included in analysis
ppn <- read.delim("PPN_final.txt", header = F)
df <- df[df$ppn %in% ppn$V1,]
df$ppn <- as.factor(df$ppn)



## subset to motivation questionnaire variables 
corrmot <- df[,c(130:134, 136:140)]
corrdetail <- round(cor(corrmot, use = "complete.obs"), 1)
ggcorrplot(corrdetail, method = "circle") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10))

## subset to proficiency questionnaire variables 
corrprof <- df[,c(61:64, 96:99)]
corrdetail <- round(cor(corrprof, use = "complete.obs"), 1)
ggcorrplot(corrdetail, method = "circle") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10))


## subset to all variables for model
corrmat <- df[,c(2, 120,121,122,123,142,146, 148:152)]
corrmat <- corrmat[,-6]
colnames(corrmat) <- c("Spanish_AoA", "T2_T3_Spanish", "T2_T3_English", "T2_T3_German", "MemoryCapacity", "German_Eng_Ratio", "Mot_T2_T3_Integ", "SRP_Spanish_T2_T3", "Immersion", "Mot_T2_T3_anxiety", "Mot_T2_T3_instrumental")
corrmat2 <- corrmat[, c(1, 2, 3, 4, 6, 5, 7, 10,11,8,9)]


corrdetail <- round(cor(corrmat2, use = "complete.obs"), 1)
ggcorrplot(corrdetail, method = "circle") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10))

