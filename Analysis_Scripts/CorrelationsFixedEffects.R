### checking for correlations of fixed effects

library(ggcorrplot)

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
df <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

## add predictors to the full dataframe 
colnames(df)[1] <- "ppn"
df$GermanEngRatio_T2_T3 <- df$T2_T3_English/df$T2_T3_German
df$Mot_T2_T3_avg <- (df$Mot_T3_pos_avg+df$Mot_T2_pos_avg)/2
df$SRP_Spanish_T2_T3 <- df$T3_SRP_Spanish_avg - df$T2_SRP_Spanish_avg
df$Immersion <- (df$T3_LivingSitGermany_Spanish + df$T3_KeepUpSpanishActively + df$T3_MaintainedSpanishContact + df$T3_WatchSpanishMovies + df$T3_ReadSpanishBooks)/5
df$Mot_T2_T3_anxierty_avg <- (df$Mot_T3_anxiety+df$Mot_T2_anxiety)/2

# subset this dataframe to people who have been coded 
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

# exlcude people that typed on too many trials
counts <- as.data.frame(table(T2sub$typing, T2sub$ppn))
counts[,4] <- as.data.frame(table(T3sub$typing, T3sub$ppn))[3]
pnex <- counts[which(counts$Freq > 30 | counts$Freq.1 > 30),]$Var2

for (i in 1:length(pnex)){
  T2sub <- T2sub[-which(T2sub$ppn==pnex[i]),]
  T3sub <- T3sub[-which(T3sub$ppn==pnex[i]),]
}
T2sub$ppn <- droplevels(T2sub$ppn)
T3sub$ppn <- droplevels(T3sub$ppn)

df <- df[which(df$ppn %in% T3sub$ppn),]

corrmat <- df[,c(2, 120,121,122,123,142,146:150)]

# correlation matrix 
rcorr(as.matrix(corrmat))

# plot correlation 
corrdetail <- round(cor(corrmat, use = "complete.obs"), 1)
ggcorrplot(corrdetail, method = "circle") +
  theme(axis.text.x = element_text(size= 5),
        axis.text.y = element_text(size= 5))

## correlate only the motivation scores with one another 
## subset data frame to only motivation test scores
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
df <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")
dfmot <- df[,c(130:134, 136:140)]
corrdetail <- round(cor(dfmot, use = "complete.obs"), 1)
ggcorrplot(corrdetail, method = "circle") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10))

