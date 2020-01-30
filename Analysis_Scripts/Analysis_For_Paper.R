#### Analysis script for final write-up ####
#### Analysis of T2 and T3 data - forgetting rates and their predictors #### 

#### load packages ####
library(readxl)
library(ggplot2)
library(plyr)
library(lme4)
library(lmerTest)
library(dplyr)
library(effects)
library(interactions)

#### read data #####
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T3 <- read_excel("T3_PicNaming_DataCoded_new.xlsx", guess_max = 1048576)
T2 <- read_excel("T2_PicNaming_DataCoded.xlsx", guess_max = 1048576)

ppn <- read.delim("PPN_final.txt", header = F)

### clean data ####
# subset to coded people 
T3[T3=='NA'] <- NA
T2[T2=='NA'] <- NA
T3[is.na(T3$error) == 0,]-> T3sub
T2[is.na(T2$error) == 0,]-> T2sub

## exlcude people who have less than 144 trials 
nums <- which(table(T2sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  for (i in 1:length(nums)){
    T2sub <- T2sub[-which(T2sub$ppn == nums[i]),]}
}
nums <- which(table(T3sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  T3sub <- T3sub[-which(T3sub$ppn == nums),]
}

T3sub$ppn <- as.factor(T3sub$ppn)
T2sub$ppn <- as.factor(T2sub$ppn)

# subset to people who are included in analysis
T2sub <- T2sub[T2sub$ppn %in% ppn$V1,]
T2sub$ppn <- droplevels(T2sub$ppn)
T3sub <- T3sub[T3sub$ppn %in% ppn$V1,]
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

# set numeric 
T3sub$error <- as.numeric(T3sub$error)
T3sub$phoncorr <- as.numeric(T3sub$phoncorr)
T3sub$phonincorr <- as.numeric(T3sub$phonincorr)
T2sub$error <- as.numeric(T2sub$error)
T2sub$phoncorr <- as.numeric(T2sub$phoncorr)
T2sub$phonincorr <- as.numeric(T2sub$phonincorr)

# calculate the ratio of correct for all partially correct trials
for (i in 1:nrow(T3sub)){
  if (is.na(T3sub$error[i]) == 0 && T3sub$error[i] == 999){
    T3sub$error[i] <- T3sub$phonincorr[i]/(T3sub$phonincorr[i]+T3sub$phoncorr[i])
  }
}

for (i in 1:nrow(T2sub)){
  if (is.na(T2sub$error[i]) == 0 && T2sub$error[i] == 999){
    T2sub$error[i] <- T2sub$phonincorr[i]/(T2sub$phonincorr[i]+T2sub$phoncorr[i])
  }
}

# based on items plots below, exclude ambiguous items 
ambitems <- c("envelope.png", "pearl.png", "screen.png")
for (i in 1:length(ambitems)){
  T2sub <- T2sub[-which(T2sub$imgFilename == ambitems[i]),]
}
for (i in 1:length(ambitems)){
  T3sub <- T3sub[-which(T3sub$imgFilename == ambitems[i]),]
}
#T3sub$imgFilename <- droplevels(T3sub$imgFilename)
#T2sub$imgFilename <- droplevels(T2sub$imgFilename)

# delete practice trials 
T2sub <- T2sub[T2sub$imgIndex > 3,] #excluding the first 4 items, trial index starts at 0
T3sub <- T3sub[T3sub$imgIndex > 3,]

# combined the datasets into one
Mcombined <- rbind(T2sub, T3sub)
Mcombined$session <- rep(c(2,3), each = nrow(T2sub)) 
Mcombined$error <- Mcombined$error*100

# going for the dichotomous error coding
Mcombined$errorbin <- Mcombined$error
for (i in 1:nrow(Mcombined)){
  if (Mcombined$error[i] != 0 & Mcombined$error[i] != 100){
    Mcombined$errorbin[i] <- 100
  }
}

T2sub$errorbin <- T2sub$error
for (i in 1:nrow(T2sub)){
  if (T2sub$error[i] != 0 & T2sub$error[i] != 100){
    T2sub$errorbin[i] <- 100
  }
}

T3sub$errorbin <- T3sub$error
for (i in 1:nrow(T3sub)){
  if (T3sub$error[i] != 0 & T3sub$error[i] != 100){
    T3sub$errorbin[i] <- 100
  }
}


## Adjusting phoneme correct and incorrect counts 
# read in the file that contains the phoneme count 
setwd("//ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
lenwords <- read.delim("FullListWords_SpanishNaming.txt")

# adjust existing phoneme correct and incorrect counts 
Mcombined$OrigLen <- NA
Mcombined$img <- gsub(".png", "", Mcombined$imgFilename)

for (j in 1:nrow(Mcombined)) {
  pos <- which(tolower(as.character(lenwords$English)) == tolower(as.character(Mcombined$img[j])))
  Mcombined$OrigLen[j] <- lenwords$TotalPhon[pos]
  
  if (is.na(lenwords$AltPhon[pos])==1) {}
  else if (is.na(lenwords$AltPhon[pos])==0 && is.na(Mcombined$response[j])==0) {
    if (grepl("man", Mcombined$response[j]) && Mcombined$imgFilename[j] == "peanut.png") {
      # cacahuete synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "mani"
    } else if (grepl("pil", Mcombined$response[j]) && Mcombined$imgFilename[j] == "battery.png") {
      # batteria synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "pila"
    } else if (grepl("are", Mcombined$response[j]) && Mcombined$imgFilename[j] == "earring.png") {
      # pendientes synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "aretes"
    } else if (grepl("^aro", Mcombined$response[j]) && Mcombined$imgFilename[j] != "ring.png") {
      # anillo synonym
      Mcombined$OrigLen[j] <- lenwords$X[pos]
      Mcombined$imgFilename[j] <- "aro"
    } else if (grepl("co", Mcombined$response[j]) && Mcombined$imgFilename[j] == "pillow.png") {
      # almohada synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "cojin"
    } else if (grepl("ana", Mcombined$response[j]) && Mcombined$imgFilename[j] == "pineapple.png") {
      # pinya synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "ananas"
    } else if (grepl("cob", Mcombined$response[j]) && Mcombined$imgFilename[j] == "blanket.png") {
      # manta synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "cobija"
    } else if (grepl("cit", Mcombined$response[j]) && Mcombined$imgFilename[j] == "lemon.png") {
      # limon synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "citron"
    } else if (grepl("ori", Mcombined$response[j]) && Mcombined$imgFilename[j] == "sausage.png") {
      # salchicha synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "chorizo"
    } else if (grepl("bomb", Mcombined$response[j]) && Mcombined$imgFilename[j] == "straw.png") {
      # paja synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "bombilla"
    } else if (grepl("ca", Mcombined$response[j]) && Mcombined$imgFilename[j] == "candle.png") {
      # vela synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "candela"
    } else if (grepl("cer", Mcombined$response[j]) && Mcombined$imgFilename[j] == "chain.png") {
      # cadena synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "cerradura"
    } else if (grepl("stam", Mcombined$response[j]) && Mcombined$imgFilename[j] == "stamp.png") {
      # sello synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "estampilla"
    } else if (grepl("zap", Mcombined$response[j]) && Mcombined$imgFilename[j] == "pumpkin.png") {
      # calabaza synonym
      Mcombined$OrigLen[j] <- lenwords$AltPhon[pos]
      Mcombined$imgFilename[j] <- "zapallo"
    }
  } 
  
  rm(pos)
}

for (j in 1:nrow(Mcombined)) {
  if (Mcombined$error[j]==0){
    Mcombined$Corr[j] <- Mcombined$OrigLen[j]
    Mcombined$Incorr[j] <- 0
    Mcombined$phoncorr[j]  <- Mcombined$OrigLen[j]
    Mcombined$phonincorr[j]  <- 0
  } else if (Mcombined$error[j]==100){
    Mcombined$Corr[j] <- 0
    Mcombined$Incorr[j] <- Mcombined$OrigLen[j]
    Mcombined$phoncorr[j]  <- 0
    Mcombined$phonincorr[j]  <- Mcombined$OrigLen[j]
  } 
}

Mcombined$Total <- Mcombined$phoncorr + Mcombined$phonincorr

Mcombined$CorrPer <- Mcombined$phoncorr/Mcombined$Total
Mcombined$Corr <- round(Mcombined$CorrPer*Mcombined$OrigLen,0)
Mcombined$Incorr <- Mcombined$OrigLen-Mcombined$Corr
Mcombined$Ratio <- (Mcombined$Corr/Mcombined$OrigLen)*100


#### Full model with all data from both sessions ####
Mcombined$session <- as.factor(Mcombined$session)
contrasts(Mcombined$session) <- c(-0.5, 0.5)

## what predicts changes in error rate from T2 to T3?
individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

## add predictors to the full dataframe 
colnames(individualdiff)[1] <- "ppn"
df <- merge(Mcombined, individualdiff, by = "ppn")
df$GermanEngRatio_T2_T3 <- df$T2_T3_English/df$T2_T3_German
df$imgFilename <- as.factor(df$imgFilename)
df$Mot_T2_T3_avg <- (df$Mot_T3_pos_avg+df$Mot_T2_pos_avg)/2
#df$Mot_T2_T3_diff <- df$Mot_T3_pos_avg-df$Mot_T2_pos_avg
df$SRP_Spanish_T2_T3 <- df$T3_SRP_Spanish_avg - df$T2_SRP_Spanish_avg
df$Immersion <- (df$T3_LivingSitGermany_Spanish + df$T3_WatchSpanishMovies + df$T3_ReadSpanishBooks)/3
df$Mot_T2_T3_anxiety_avg <- (df$Mot_T3_anxiety+df$Mot_T2_anxiety)/2

# add cognate status to dataframe 
lenwords <- read.delim("FullListWords_SpanishNaming.txt")
lenwords$img <- gsub(".png", "", lenwords$Picture)
lenwords2 <- dplyr::select(lenwords, img, Cognate.)

df <- left_join(df, lenwords2, by = "img")
df$Cognate.[df$Cognate.==2] <- 1
df$Cognate. <- as.factor(df$Cognate.)
#contrasts(dfcog$Cognate.) <- c(-0.5, 0.5)
df$CogN <- (as.numeric(as.character(df$Cognate.))-0.5)  # cognates are 0.5, non-cognates are -0.5

# add frequency in German to dataframe 
frequencies <- read.delim("Germanfrequencies.txt")
colnames(frequencies) <- c("German", "German2", "img", "Spanish", "SubLog", "SubMln")
freq2 <- dplyr::select(frequencies, img, SubLog)
df <- merge(df, freq2, by = "img")

#T2perf <- as.data.frame(tapply(T2sub$error, T2sub$ppn, mean))
#for (i in 1:length(unique(df$ppn))){
#  pnum <- unique(df$ppn)[i]
#  num <- which(rownames(T2perf)==pnum)
#  for (k in 1:nrow(df[df$ppn == pnum,])){
#    df[df$ppn == pnum,]$T2perf[k] <- T2perf[num,1]}
#}

  
# does average Spanish frequency of use after study abroad predict forgetting / learning rates between T2 and T3 

# test each predictor seperately and assess whether its inclusion improves model fit 
modelsess <- glmer(cbind(Corr, Incorr) ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)

summary(modelsess)
modelSpanishFreq <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_Spanish) +
                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelSpanishFreq)
anova(modelsess, modelSpanishFreq) ### Spanish frequency of use improves model fit  :-)

modelGermanEngl <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_Spanish)*scale(GermanEngRatio_T2_T3) +
                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelGermanEngl)
anova(modelSpanishFreq, modelGermanEngl) ### Ratio of German to English frequency interacts with Spanish use, but does not improve model fit compared to a model that only includes Spanish use

modelEnglFreq <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_English) +
                         (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelEnglFreq)
anova(modelsess, modelEnglFreq) ### English frequency of use does not improve model fit.

modelGerFreq <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_German) +
                        (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelGerFreq)
anova(modelsess, modelGerFreq) ### German frequency of use improves model fit. :-)

modelMotivation <- glmer(cbind(Corr, Incorr) ~ session*scale(Mot_T2_T3_avg) +
                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelMotivation)
anova(modelsess, modelMotivation) ### Motivation improves model fit :-)

modelMotivationAnxiety <- glmer(cbind(Corr, Incorr) ~ session*scale(Mot_T2_T3_avg)*scale(Mot_T2_T3_anxiety_avg) +
                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelMotivationAnxiety)
anova(modelMotivation, modelMotivationAnxiety) ### anxiety added improves model fit :-)

modelSRpSpanish <- glmer(cbind(Corr, Incorr) ~ session*scale(SRP_Spanish_T2_T3) +
                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelSRpSpanish)
anova(modelsess, modelSRpSpanish) ### Spansh SRP improves model fit :-)

modelImmersion <- glmer(cbind(Corr, Incorr) ~ session*scale(Immersion) +
                          (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelImmersion)
anova(modelsess, modelImmersion) ### Immersion improves model fit :-)

modelSpanishAoA <- glmer(cbind(Corr, Incorr) ~ session*scale(SpanishAoA) +
                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelSpanishAoA)
anova(modelsess, modelSpanishAoA) ### Spanish AoA does not improve model fit

modelDoor <- glmer(cbind(Corr, Incorr) ~ session*DoorsScore +
                     (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelDoor)
anova(modelsess, modelDoor) ### Memory capacity does NOT improve model fit

modelfreq <- glmer(cbind(Corr, Incorr) ~ session*scale(SubLog) +
                          (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelfreq)
anova(modelsess, modelfreq) ### frequency improves model fit :-)

modelcognate <- glmer(cbind(Corr, Incorr) ~ session*CogN +
                     (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelcognate)
anova(modelsess, modelcognate) ### cognate status improves model fit :-) 

### full model with all significant predictors in it 
modelfull <- glmer(cbind(Corr, Incorr) ~ 
                     session*scale(T2_T3_Spanish) +
                     session*scale(T2_T3_German) +
                     session*scale(SRP_Spanish_T2_T3) +
                     session*scale(Mot_T2_T3_avg) +
                     session*scale(Mot_T2_T3_anxiety_avg) +
                   #  session*scale(Immersion) +
                     session*scale(SubLog) +
                     session*CogN +
                     (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelfull)

# plotting the significant interactions from the final model only 

# Spanish frequency of use
#interact_plot(modelfull, pred = T2_T3_Spanish, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e2 <- predictorEffect("T2_T3_Spanish", modelfull)
plot(e2, lines=list(multiline=TRUE), confint=list(style="auto"), type='response')
accuracy_mean_pp <- ddply(df, .(session, ppn, T2_T3_Spanish), 
                          plyr::summarise,
                          mean = mean(Ratio),
                          sem = sd(Ratio)/sqrt(length(Ratio)))
ggplot(df, aes(x = T2_T3_Spanish, y= Ratio/100, color = session)) + 
  geom_smooth(method = "glm",method.args=list(family = "binomial")) +
  geom_point(data = accuracy_mean_pp, aes(y=mean/100)) +
  xlab("Average frequency of use of Spanish in the 6 months post return to Germany (in %)") +
  ylab("% correct Spanish productions") +
  scale_color_discrete(name = "Session", labels = c("T2", "T3")) +
  theme_bw()

# Germany frequency of use
e21 <- predictorEffect("T2_T3_German", modelfull)
plot(e21, lines=list(multiline=TRUE), confint=list(style="auto"), type='response')
accuracy_mean_pp <- ddply(df, .(session, ppn, T2_T3_German), 
                          plyr::summarise,
                          mean = mean(Ratio),
                          sem = sd(Ratio)/sqrt(length(Ratio)))
ggplot(df, aes(x = T2_T3_German, y= Ratio/100, color = session)) + 
  geom_smooth(method = "glm",method.args=list(family = "binomial")) +
  geom_point(data = accuracy_mean_pp, aes(y=mean/100)) +
  xlab("Average frequency of use of German in the 6 months post return to Germany (in %)") +
  ylab("% correct Spanish productions") +
  scale_color_discrete(name = "Session", labels = c("T2", "T3")) +
  theme_bw()

# Spanish proficiency self-rated
#interact_plot(modelfull, pred = SRP_Spanish_T2_T3, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e3 <- predictorEffect("SRP_Spanish_T2_T3", modelfull)
plot(e3, lines=list(multiline=TRUE), confint=list(style="auto"), type='response')
accuracy_mean_pp2 <- ddply(df, .(session, ppn, SRP_Spanish_T2_T3), 
                          plyr::summarise,
                          mean = mean(Ratio),
                          sem = sd(Ratio)/sqrt(length(Ratio)))
ggplot(df, aes(x = SRP_Spanish_T2_T3, y= Ratio/100, color = session)) + 
  geom_smooth(method = "glm",method.args=list(family = "binomial")) +
  geom_point(data = accuracy_mean_pp2, aes(y=mean/100)) +
  xlab("Self-perceived increase in Spanish proficiency from T2 to T3 (scale from -7 - 100% worse at T3 compared to T2 to 7 - 100% better at T3 compared to T2)") +
  ylab("% correct Spanish productions") +
  scale_color_discrete(name = "Session", labels = c("T2", "T3")) +
  theme_bw()

# Anxiety to speak Spanish after return to Germany
#interact_plot(modelfull, pred = Mot_T2_T3_anxiety_avg, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e4 <- predictorEffect("Mot_T2_T3_anxiety_avg", modelfull)
plot(e4, lines=list(multiline=TRUE), confint=list(style="auto"), type='response')
accuracy_mean_pp3 <- ddply(df, .(session, ppn, Mot_T2_T3_anxiety_avg), 
                           plyr::summarise,
                           mean = mean(Ratio),
                           sem = sd(Ratio)/sqrt(length(Ratio)))
ggplot(df, aes(x = Mot_T2_T3_anxiety_avg, y= Ratio/100, color = session)) + 
  geom_smooth(method = "glm",method.args=list(family = "binomial")) +
  geom_point(data = accuracy_mean_pp3, aes(y=mean/100)) +
  xlab("Average motivation to learn and maintain Spanish at T2 and T3 (ona  scale from 1 to 7)") +
  ylab("% correct Spanish productions") +
  scale_color_discrete(name = "Session", labels = c("T2", "T3")) +
  theme_bw()

# Cognate status
#interact_plot(modelfull, pred = CogN, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e5 <- predictorEffect("CogN", modelfull)
plot(e5, lines=list(multiline=TRUE), confint=list(style="auto"), type='response')
accuracy_mean_pp4 <- ddply(df, .(session, ppn, CogN), 
                           plyr::summarise,
                           mean = mean(Ratio),
                           sem = sd(Ratio)/sqrt(length(Ratio)))
ggplot(df, aes(x = CogN, y= Ratio/100, color = session)) + 
  geom_smooth(method = "glm",method.args=list(family = "binomial")) +
  geom_point(data = accuracy_mean_pp4, aes(y=mean/100)) +
  xlab("Cognate status", labels = c("Non-cognate", "Cognate")) +
  ylab("% correct Spanish productions") +
  scale_color_discrete(name = "Session", labels = c("T2", "T3")) +
  theme_bw()

# plot raw data for cognate differences
ddply(df, .(Cognate., session, ppn), 
      summarise, N=length(error), 
      mean   = mean(error, na.rm = TRUE), 
      sem = sd(error, na.rm = TRUE)/sqrt(N)) -> aggregatedCog

aggregatedCog_mean <- ddply(aggregatedCog, .(Cognate., session), 
                            summarise,
                            condition_mean = mean(mean,na.rm = T),
                            condition_sem = sd(mean,na.rm = T)/sqrt(length(mean[!is.na(mean)])))
aggregatedCog_mean$Cognate.Lab <- factor(aggregatedCog_mean$Cognate., 
                                         levels=0:1,
                                         labels=c("Non-cognate","Cognate"))
aggregatedCog_mean$condition_mean <- aggregatedCog_mean$condition_mean*100
aggregatedCog_mean$condition_sem <- aggregatedCog_mean$condition_sem*100

ggplot(aggregatedCog_mean, aes(x=session, y=condition_mean, group= Cognate.Lab)) +
  geom_bar(stat="identity", position =position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  facet_wrap(~Cognate.Lab) +
  xlab("Session") +
  scale_x_discrete(labels=c("T2", "T3"), breaks = 2:3) +
  ylab("% incorrect") +
  theme_bw()

# plot raw data for frequency differences
#df$Freq <- NA
#df[df$SubLog<2.3475,]$Freq <- "Low"
#df[df$SubLog>2.3475,]$Freq <- "High"

#ddply(df, .(Freq, session, ppn), 
#      summarise, N=length(error), 
#      mean   = mean(error, na.rm = TRUE), 
#      sem = sd(error, na.rm = TRUE)/sqrt(N)) -> aggregatedCog

#aggregatedCog_mean <- ddply(aggregatedCog, .(Freq, session), 
#                            summarise,
#                            condition_mean = mean(mean,na.rm = T),
#                            condition_sem = sd(mean,na.rm = T)/sqrt(length(mean[!is.na(mean)])))
#aggregatedCog_mean$condition_mean <- aggregatedCog_mean$condition_mean*100
#aggregatedCog_mean$condition_sem <- aggregatedCog_mean$condition_sem*100

# ggplot(aggregatedCog_mean, aes(x=Freq, y=condition_mean, fill= session)) +
#   geom_bar(stat="identity", position =position_dodge()) +
#   geom_errorbar(aes(ymin=condition_mean-condition_sem,
#                     ymax=condition_mean+condition_sem),
#                 width = 0.5, position=position_dodge(0.9)) +
#   xlab("Frequency") +
#   scale_fill_manual(values=c("grey70", "grey25")) +
#   ylab("% incorrect") +
#   theme_bw()

#### Regression hypothesis  ####
# check for whether the words learned between T1 and T2, are also the words that were forgotten more often

# first load T1 data
T1 <- read_excel("T1_PicNaming_DataCoded.xlsx")
T1[T1=='NA'] <- NA
T1sub <- T1[T1$ppn %in% ppn$V1,]
T1$ppn <- as.factor(T1$ppn)

T1sub$img <- gsub(".png", "", T1sub$imgFilename)
T1sub <- dplyr::select(T1sub, ppn, img, error)

T2sub$img <- gsub(".png", "", T2sub$imgFilename)
T3sub$img <- gsub(".png", "", T3sub$imgFilename)
T2sub2 <- dplyr::select(T2sub, ppn, img, error)
T3sub2 <- merge(T3sub, T2sub2, by = c("ppn", "img"))
colnames(T3sub2)[ncol(T3sub2)] <- "error_T2"

T3sub3 <- T3sub2[T3sub2$error_T2 == 0,]
#T2sub3 <- T2sub[T2sub$error == 0,]
T3sub3 <-T3sub3[,-20]
colnames(T3sub3)[10] <- "error"
T3sub3 <- T3sub3[,names(T2sub3)]

#Mcombined2 <- rbind(T2sub3, T3sub3)
#Mcombined2$session <- rep(c(2,3), each = nrow(T2sub3)) 

T1sub2 <- merge(T1sub, T2sub2, by = c("ppn", "img"))
colnames(T1sub2)[ncol(T1sub2)] <- "error_T2"
T1sub2 <- T1sub2[T1sub2$error_T2 == 0,]
T1sub2 <- T1sub2[,-4]
colnames(T1sub2)[ncol(T1sub2)] <- "error_T1"

df <- merge(T3sub3, T1sub2, by = c("ppn", "img"))

# change all 999 errors to full errors at T1
for (i in 1:nrow(df)){
  if (is.na(df$error_T1[i])==0 & df$error_T1[i] == 999){
    df$error_T1[i] <- 1
  }
}

# change error coding to binary
df[df$error>0,]$error <- 1

# modelling
df$error_t1N <- as.numeric(as.character(df$error_T1))
df$error_T1 <- as.factor(df$error_T1)
contrasts(df$error_T1) <- c(-0.5, 0.5)

# for binary error coding
modelerror <- glmer(error ~ error_t1N + 
                      (1|ppn) + (1|img), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelerror)

e <- effect("error_t1N",modelerror)
plot(e, type='response')
# Regression hypothesis is confirmed: words that were only learned between T1 and T2, 
# have higher error rates at T3, and are thus forgotten more than words that were already known at T1

# same analysis with fine-grained error rates
setwd("//ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
lenwords <- read.delim("FullListWords_SpanishNaming.txt")

# adjust existing phoneme correct and incorrect counts 
df$OrigLen <- NA
df$img <- gsub(".png", "", df$imgFilename)

for (j in 1:nrow(df)) {
  pos <- which(tolower(as.character(lenwords$English)) == tolower(as.character(df$img[j])))
  df$OrigLen[j] <- lenwords$TotalPhon[pos]
  
  if (is.na(lenwords$AltPhon[pos])==1) {}
  else if (is.na(lenwords$AltPhon[pos])==0 && is.na(df$response[j])==0) {
    if (grepl("man", df$response[j]) && df$imgFilename[j] == "peanut.png") {
      # cacahuete synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "mani"
    } else if (grepl("pil", df$response[j]) && df$imgFilename[j] == "battery.png") {
      # batteria synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "pila"
    } else if (grepl("are", df$response[j]) && df$imgFilename[j] == "earring.png") {
      # pendientes synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "aretes"
    } else if (grepl("^aro", df$response[j]) && df$imgFilename[j] != "ring.png") {
      # anillo synonym
      df$OrigLen[j] <- lenwords$X[pos]
      df$imgFilename[j] <- "aro"
    } else if (grepl("co", df$response[j]) && df$imgFilename[j] == "pillow.png") {
      # almohada synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "cojin"
    } else if (grepl("ana", df$response[j]) && df$imgFilename[j] == "pineapple.png") {
      # pinya synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "ananas"
    } else if (grepl("cob", df$response[j]) && df$imgFilename[j] == "blanket.png") {
      # manta synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "cobija"
    } else if (grepl("cit", df$response[j]) && df$imgFilename[j] == "lemon.png") {
      # limon synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "citron"
    } else if (grepl("ori", df$response[j]) && df$imgFilename[j] == "sausage.png") {
      # salchicha synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "chorizo"
    } else if (grepl("bomb", df$response[j]) && df$imgFilename[j] == "straw.png") {
      # paja synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "bombilla"
    } else if (grepl("ca", df$response[j]) && df$imgFilename[j] == "candle.png") {
      # vela synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "candela"
    } else if (grepl("cer", df$response[j]) && df$imgFilename[j] == "chain.png") {
      # cadena synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "cerradura"
    } else if (grepl("stam", df$response[j]) && df$imgFilename[j] == "stamp.png") {
      # sello synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "estampilla"
    } else if (grepl("zap", df$response[j]) && df$imgFilename[j] == "pumpkin.png") {
      # calabaza synonym
      df$OrigLen[j] <- lenwords$AltPhon[pos]
      df$imgFilename[j] <- "zapallo"
    }
  } 
  
  rm(pos)
}

for (j in 1:nrow(df)) {
  if (df$error[j]==0){
    df$Corr[j] <- df$OrigLen[j]
    df$Incorr[j] <- 0
    df$phoncorr[j]  <- df$OrigLen[j]
    df$phonincorr[j]  <- 0
  } else if (df$error[j]==1){
    df$Corr[j] <- 0
    df$Incorr[j] <- df$OrigLen[j]
    df$phoncorr[j]  <- 0
    df$phonincorr[j]  <- df$OrigLen[j]
  } else if (df$error[j]==100){
    df$Corr[j] <- 0
    df$Incorr[j] <- df$OrigLen[j]
    df$phoncorr[j]  <- 0
    df$phonincorr[j]  <- df$OrigLen[j]
  }
}

df$Total <- df$phoncorr + df$phonincorr
df$CorrPer <- df$phoncorr/df$Total
df$Corr <- round(df$CorrPer*df$OrigLen,0)
df$Incorr <- df$OrigLen-df$Corr
df$Ratio <- (df$Corr/df$OrigLen)*100

modelerror2 <- glmer(cbind(Corr, Incorr) ~ error_T1 + 
                      (1|ppn) + (1|img), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
summary(modelerror)

e <- effect("error_T1",modelerror2)
plot(e, type='response', confint=list(style="auto"))

accuracy_mean_pp <- ddply(df, .(ppn, error_T1), 
                           plyr::summarise,
                           mean = mean(Ratio),
                           sem = sd(Ratio)/sqrt(length(Ratio)))

accuracy_mean_pp_m <- ddply(accuracy_mean_pp, .(error_T1), 
                            summarise,
                            condition_mean = mean(mean,na.rm = T),
                            condition_sem = sd(mean,na.rm = T)/sqrt(length(mean[!is.na(mean)])))
                            #aggregatedCog_mean$condition_mean <- aggregatedCog_mean$condition_mean*100
                            #aggregatedCog_mean$condition_sem <- aggregatedCog_mean$condition_sem*100
accuracy_mean_pp_m$error_T1 <- as.factor(accuracy_mean_pp_m$error_T1)
                     
ggplot(accuracy_mean_pp_m, aes(x=error_T1, y=condition_mean)) +
  geom_bar(stat="identity", position =position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  xlab("Knowledge at T1") +
  scale_x_discrete(labels=c("known", "not known"), breaks = 0:1) +
  ylab("% correct Spanish productions at T3") +
  theme_bw()


## to make a plot including T2
dfcomp <- rbind(df,df)
dfcomp$session <- rep(c(3,2), each = nrow(df)) 
dfcomp[dfcomp$session==2,]$Ratio <- 100

accuracy_mean_pp <- ddply(dfcomp, .(ppn, session, error_T1), 
                          plyr::summarise,
                          mean = mean(Ratio),
                          sem = sd(Ratio)/sqrt(length(Ratio)))

accuracy_mean_pp_m <- ddply(accuracy_mean_pp, .(session, error_T1), 
                            summarise,
                            condition_mean = mean(mean,na.rm = T),
                            condition_sem = sd(mean,na.rm = T)/sqrt(length(mean[!is.na(mean)])))
#aggregatedCog_mean$condition_mean <- aggregatedCog_mean$condition_mean*100
#aggregatedCog_mean$condition_sem <- aggregatedCog_mean$condition_sem*100
accuracy_mean_pp_m$error_T1 <- as.factor(accuracy_mean_pp_m$error_T1)

ggplot(accuracy_mean_pp_m, aes(x=session, y=condition_mean, color = error_T1)) +
  geom_line() +
  geom_point() + 
  #geom_errorbar(aes(ymin=condition_mean-condition_sem,
  #                  ymax=condition_mean+condition_sem),
  #              width = 0.5, position=position_dodge(0.9)) +
  xlab("Knowledge at T1") +
 scale_color_discrete(name = "T1", labels = c("Known", "Unknown")) +
  ylab("% correct Spanish productions") +
  theme_bw()
