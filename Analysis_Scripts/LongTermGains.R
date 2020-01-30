#### Does a study abroad have any long-term benefits? 

#### load packages ####
library(readxl)
library(ggplot2)
library(plyr)
library(lme4)
library(lmerTest)
library(dplyr)

#### read data #####
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T3 <- read_excel("T3_PicNaming_DataCoded_new.xlsx", guess_max = 1048576)
T2 <- read_excel("T2_PicNaming_DataCoded.xlsx", guess_max = 1048576)
T1 <- read_excel("T1_PicNaming_DataCoded.xlsx", guess_max = 1048576)

### clean data ####
# subset to coded people 
T3[T3=='NA'] <- NA
T2[T2=='NA'] <- NA
T1[T1=='NA'] <- NA
T1[is.na(T1$error) == 0,]-> T1sub
T3[is.na(T3$error) == 0,]-> T3sub
T2[is.na(T2$error) == 0,]-> T2sub

# exlcude people who have less than 144 trials 
T3sub$ppn <- as.factor(T3sub$ppn)
T2sub$ppn <- as.factor(T2sub$ppn)
T1sub$ppn <- as.factor(T1sub$ppn)

nums <- which(table(T1sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  for (i in 1:length(nums)){
    T1sub <- T1sub[-which(T1sub$ppn == nums[i]),]}
}

rm(nums)
nums <- which(table(T2sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  for (i in 1:length(nums)){
  T2sub <- T2sub[-which(T2sub$ppn == nums[i]),]}
}
rm(nums)
nums <- which(table(T3sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  for (i in 1:length(nums)){
  T3sub <- T3sub[-which(T3sub$ppn == nums[i]),]}
}

T1sub$ppn <- droplevels(T1sub$ppn)
T2sub$ppn <- droplevels(T2sub$ppn)
T3sub$ppn <- droplevels(T3sub$ppn)

# subset to people who have been coded for both sessions
pps <- intersect(intersect(T1sub$ppn,T2sub$ppn),T3sub$ppn)
T2sub2 <- T2sub[which(T2sub$ppn %in% pps),]
T2sub2$ppn <- droplevels(T2sub2$ppn)
T1sub2 <- T1sub[which(T1sub$ppn %in% pps),]
T1sub2$ppn <- droplevels(T1sub2$ppn)
T3sub2 <- T3sub[which(T3sub$ppn %in% pps),]
T3sub2$ppn <- droplevels(T3sub2$ppn)

cols <- colnames(T2sub2)[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
cols <- cols[c(1:16)]
T1sub3 = T1sub2[cols]
T2sub3 = T2sub2[cols]
T3sub3 = T3sub2[cols]

# combined the datasets into one
Mcombined <- rbind(T1sub3, T2sub3, T3sub3)
Mcombined$session <- rep(c(1,2,3), each = nrow(T2sub3)) 

# exlcude people that typed on too many trials
counts <- as.data.frame(table(T2sub2$typing, T2sub2$ppn))
counts[,4] <- as.data.frame(table(T3sub2$typing, T3sub2$ppn))[3]
counts[,5] <- as.data.frame(table(T1sub2$typing, T1sub2$ppn))[3]
pnex <- counts[which(counts$Freq > 30 | counts$Freq.1 > 30  | counts$Freq.2 > 30),]$Var2

for (i in 1:length(pnex)){
  Mcombined <- Mcombined[-which(Mcombined$ppn==pnex[i]),]
}
Mcombined$ppn <- droplevels(Mcombined$ppn)

# set numeric 
Mcombined$error <- as.numeric(Mcombined$error)
Mcombined$phoncorr <- as.numeric(Mcombined$phoncorr)
Mcombined$phonincorr <- as.numeric(Mcombined$phonincorr)

# calculate the ratio of correct for all partially correct trials
for (i in 1:nrow(Mcombined)){
  if (is.na(Mcombined$error[i]) == 0 && Mcombined$error[i] == 999){
    Mcombined$error[i] <- Mcombined$phonincorr[i]/(Mcombined$phonincorr[i]+Mcombined$phoncorr[i])
  }
}

# based on items plots below, exclude ambiguous items 
ambitems <- c("envelope.png", "pearl.png", "screen.png")
for (i in 1:length(ambitems)){
  Mcombined <- Mcombined[-which(Mcombined$imgFilename == ambitems[i]),]
}
#T3sub$imgFilename <- droplevels(T3sub$imgFilename)
#T2sub$imgFilename <- droplevels(T2sub$imgFilename)

# delete practice trials 
Mcombined <- Mcombined[Mcombined$imgIndex > 4,] #excluding the first 5 items, trial index starts at 0
Mcombined$error <- Mcombined$error*100
# going for the dichotomous error coding
Mcombined$errorbin <- Mcombined$error
for (i in 1:nrow(Mcombined)){
  if (is.na(Mcombined$error[i])== 0 & Mcombined$error[i] != 0 & Mcombined$error[i] != 100){
    Mcombined$errorbin[i] <- 100
  }
}

### decide whether to plot and do analyses with dichotomous codings or not
Mcombined$errorfine <- Mcombined$error
dichotomous = TRUE
if (dichotomous == T){
  Mcombined$error <- Mcombined$errorbin
} else {
  Mcombined$error <- Mcombined$errorfine
}

#### run a model that tests whether performance goes back to baseline 
Mcombined$session <- as.factor(Mcombined$session)
#Mcombined$session <- relevel(Mcombined$session, ref = "2")
contrasts(Mcombined$session) <- contr.treatment(3)
colnames(contrasts(Mcombined$session)) <- c("T1-T2", "T1-T3")
Mcombined[Mcombined$error==100,]$error <- 1

modelerror <- glmer(error ~ session + 
                      (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = Mcombined)
summary(modelerror)

e <- effect("session",modelerror)
plot(e)


# plot the raw data

aggregatedfull <- ddply(Mcombined, .(session, ppn), 
                        plyr::summarise,
                        mean = mean(error, na.rm = T),
                        sem = sd(error, na.rm = T)/sqrt(length(error)))

aggregatedmean <- ddply(aggregatedfull, .(session), 
                        plyr::summarise,
                        condition_mean = mean(mean),
                        condition_sem = sd(mean)/sqrt(length(mean)))

combinedfull <- merge(aggregatedfull, aggregatedmean, by = c("session"))

## plot absolute error rates per person and grand mean in red on top 
lineplot <- ggplot(combinedfull, aes(y = mean, x = session, group = ppn))
lineplot + geom_point(color = "darkgrey") +
  geom_line(color = "darkgrey") +
  geom_point(aes(y = condition_mean), color = "red") + 
  geom_line(aes(y = condition_mean), color = "red", size = 1) + 
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, color = "red", size = 1) +
  geom_text(data = combinedfull[combinedfull$session==3,], aes(label=ppn), size = 3, nudge_x = c(0.2)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_discrete(name = "Session", breaks = c(1,2,3),labels=c("T1","T2", "T3"), expand = c(0.1,0.1)) +
  ylab("Error rates") +
  #scale_color_manual(guide=F, "Language Condition", values=c("dodgerblue4","firebrick"),labels=c("Dutch","English")) +
  theme_bw()

