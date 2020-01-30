### Read in fluency data 

library(readxl)
library(tidyr)
library(MASS)
library(lme4)

### Fluency performance control group ####
reference <- read.delim("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/Control_Fluency.txt")
#reference <- reference[which(is.na(reference$Word)==0),]
refmeans <- as.data.frame(tapply(reference$Word,list(reference$PP, reference$Category.Letter), sum, na.rm = T))
#refmeans$T1_grandmean_Eng <- (refmeans$Berufe + refmeans$F + refmeans$`Tiere an Land`)/3
#refmeans$T2_grandmean_Eng <- (refmeans$A + refmeans$Kleidung + refmeans$Obst)/3
#refmeans$T3_grandmean_Eng <- (refmeans$D + refmeans$Körperteile + refmeans$Elektronika)/3
#refmeans$Control_grandmean_English <- (refmeans$R + refmeans$Möbel)/2
#refmeans$T1_grandmean_Ger <- (refmeans$Küchenutensilien + refmeans$P + refmeans$Gemüse)/3
#refmeans$T2_grandmean_Ger <- (refmeans$Fortbewegungsmittel + refmeans$`Hygiene/Badartikel` + refmeans$M)/3
#refmeans$T3_grandmean_Ger <- (refmeans$Sport + refmeans$Büroartikel + refmeans$B)/3
#refmeans$Control_grandmean_German <- (refmeans$T + refmeans$`Tiere im Wasser`)
refmeans$Ppn <- row.names(refmeans)

# converting the dataframe to long format
data_long <- gather(refmeans, task, score, A:`Tiere im Wasser`, factor_key=TRUE)
data_long$Group <- NA
for (i in 1:nrow(data_long)){
  if (data_long$Ppn[i] %in% c("3013","3016", "3057", "3072", "3078", "3090", "3099", "3127", "3139", "3212", "3215", "3217", "3232")){
    data_long$Group[i] <- 1
  }
  if (data_long$Ppn[i] %in% c("3023", "3039" ,"3042" ,"3064" ,"3070" ,"3073", "3079", "3085", "3106" ,"3112" ,"3162", "3175", "3199" ,"3265")){
    data_long$Group[i] <- 2
  }
  if (data_long$Ppn[i] %in% c("3037", "3059", "3062", "3065", "3080", "3147", "3153", "3173" ,"3180", "3181" ,"3250" ,"3254", "3270")){
    data_long$Group[i] <- 3
  }
}
data_long$Language <- NA
for (i in 1:nrow(data_long)){
  if (data_long$task[i] %in% c("F","A", "D", "R", "Tiere an Land", "Berufe", "Kleidung", "Obst", "Körperteile", "Elektronika", "Möbel")){
    data_long$Language[i] <- "English"
  }
  if (data_long$task[i] %in% c("P", "M" ,"B" ,"T" ,"Küchenutensilien" ,"Gemüse", "Hygiene/Badartikel", "Fortbewegungsmittel", "Sport" ,"Büroartikel" ,"Tiere im Wasser")){
    data_long$Language[i] <- "German"
  }
  
}

#### Are there differences between groups in ability? ####
data_sub <- data_long[data_long$task %in% c('R', 'T', 'Möbel', 'Tiere im Wasser'),]
data_sub$Group <- as.factor(data_sub$Group)
contrasts(data_sub$Group) <- contr.sdif(3)
data_sub$Language <- as.factor(data_sub$Language)
contrasts(data_sub$Language) <- c(0.5, -0.5)
data_sub$Ppn <- as.factor(data_sub$Ppn)
data_sub$task <- droplevels(data_sub$task)
#contrasts(data_sub$task) <- ? # not sure which contrast coding to go for here, there is no meaningful reference level, but there is also no meaningful other comparison, I would like to compare every task with every other task, so I guess the contrast coding is not the important, as there is no contrast coding that will give me exactly that anyway

## we can either run a model with language and group, or a model with task and group, or just a model with group
# (a certain task is only ever in one language, so putting both in the model at the same doesn't make sense)
# which of the two is to be preferred? I ultimately care about performance on individual tasks, of course, 
# but with this first analysis I just want to know whether groups differ overall, and not on how they possibly differ 
# on individual tasks, so maybe I should only test for group differences at this stage? 
model <- lmer(score ~ Language*Group + (1|Ppn), data = data_sub)
summary(model)
## Group 1 differs from group 2, but group 2 does not differ from group 3 
## in addition groups differ in how much they differ between the languages (group two just seems to be better at English than the other two, hence the difference)
model2 <- lmer(score ~ task*Group + (1|Ppn), data = data_sub)
summary(model2)
## This time, overall the groups don't seem to differ from each other
## but for some of the categories (i.e. tasks) they do: the pattern is a bit complex to interpret due to the contrast coding: but if I understand correctly,
## group 2 and 3 differ in how different;y they score on T vs. furniture (this is the reference category for task) and similarly, 
## it appears that group 1 and 2 differ marginally in how they differ in performance on R vs. furniture
## there might be other group difference between tasks that we don't see with 'furniture' as the reference task... 
## but I guess this just reinforces the earlier model that there are difference between groups in certain tasks, which I guess ultimately stem from differences
## in how the groups perform on German vs. English tasks --> it seems to me that the first model is then the easier one to interpret, am I right? 
## I'm a bit puzzled as to why the overall group differences are different in the two models, based on the second we'd conclude the groups don't differ overall, 
## based on the first we'd conclude groups 1 and 2 differ

# change the contrast coding of the group variable to test also for difference between group 1 and 3 
c <- contr.treatment(3)  # dummy coding
my.coding<-matrix(rep(1/3, 6), ncol=2)
my.simple<-c-my.coding # contrast 1 compares group 1 and 2, and contrast 2 compares group 1 and 3, with the intercept reflecting the grand mean
contrasts(data_sub$Group) <- my.simple
model3 <- lmer(score ~ Language*Group + (1|Ppn), data = data_sub)
summary(model3)
model4 <- lmer(score ~ task*Group + (1|Ppn), data = data_sub)
summary(model4)
## no matter which model you run, group 1 does differ from group 3 overall, and the two groups also don't seem to differ in how they perform on English vs German, 
## or on any of the tasks specifically either, for that matter 

### So based on these models, I guess we can conclude that groups 1 and 3 are reasonably comparable, but group 2 is the odd one out, 
## the difference being their English performance
## So how do we proceed from here? Given that groups differ slightly, I'm not sure it makes sense to look for item differences between groups
## on the non-control tasks, differences will be hard to interpret
## so I thought that now it would make sense to add a control task performance score for each group to the model, and given that they differ
## between languages, I guess it would make sense to have a control measure for each group (or each person even?) and language combination, 
## I just don't know how to add that to the model - would it be two columns: one for each persons average score on all English control tasks and one for 
## each persons score on all German control tasks? And then add those two as covariates to subsequent models? 



### Are there difference between items? ###
# subset to all tasks but the control tasks 
data_sub <- data_long[-which(data_long$task %in% c('R', 'T', 'Möbel', 'Tiere im Wasser')),]
data_sub$Group <- as.factor(data_sub$Group)
contrasts(data_sub$Group) <- contr.sdif(3)
data_sub$Language <- as.factor(data_sub$Language)
contrasts(data_sub$Language) <- c(0.5, -0.5)
data_sub$Ppn <- as.factor(data_sub$Ppn)
data_sub$task <- droplevels(data_sub$task)
## how should we set up the contrasts for task? I ultimately want to know how the T1 tasks compare to the T2 tasks and T3 tasks, 
## but this might be different for letter and category fluency and for the two languages

## run a model with only task, Group makes no sense to add here because each task is only done by one group
## a model with Group would be rank deficient
model5 <- lmer(score ~ task + (1|Ppn), data = data_sub)
summary(model5)


levels()# compare the two control groups in performance with each other 
t.test(refmeans[is.na(refmeans$T1_grandmean_Eng)==1,]$Control_grandmean_English, refmeans[is.na(refmeans$T1_grandmean_Eng)==0,]$Control_grandmean_English)
# unfortunately C2 is better than C1 overall
t.test(refmeans[is.na(refmeans$T1_grandmean_Eng)==1,]$Möbel, refmeans[is.na(refmeans$T1_grandmean_Eng)==0,]$Möbel)
t.test(refmeans[is.na(refmeans$T1_grandmean_Eng)==1,]$R, refmeans[is.na(refmeans$T1_grandmean_Eng)==0,]$R)
# and also on both subparts 
t.test(refmeans[is.na(refmeans$T1_grandmean_Ger)==1,]$Control_grandmean_German, refmeans[is.na(refmeans$T1_grandmean_Ger)==0,]$Control_grandmean_German)

# check whether the groups differ in performance on T1 and T2 when taking the control category performance into account
refmeans$DiffEng <- NA
refmeans$DiffGer <- NA
refmeans$Group <- NA
refmeans[is.na(refmeans$T1_grandmean_Eng)==1,]$DiffEng <- refmeans[is.na(refmeans$T1_grandmean_Eng)==1,]$T2_grandmean_Eng 
refmeans[is.na(refmeans$T2_grandmean_Eng)==1,]$DiffEng <- refmeans[is.na(refmeans$T2_grandmean_Eng)==1,]$T1_grandmean_Eng
refmeans[is.na(refmeans$T1_grandmean_Ger)==1,]$DiffGer <- refmeans[is.na(refmeans$T1_grandmean_Ger)==1,]$T2_grandmean_Ger
refmeans[is.na(refmeans$T2_grandmean_Ger)==1,]$DiffGer <- refmeans[is.na(refmeans$T2_grandmean_Ger)==1,]$T1_grandmean_Ger
refmeans[is.na(refmeans$T1_grandmean_Eng)==1,]$Group <- 2
refmeans[is.na(refmeans$T2_grandmean_Eng)==1,]$Group <- 1

model <- lm(DiffEng ~ Group + Control_grandmean_English, data = refmeans)
summary(model)
model2 <- lm(DiffGer ~ Group + Control_grandmean_German, data = refmeans)
summary(model2)






### read in fluency data T2 English 
fluencyT2 <- read.delim("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/FluencyT2.txt", sep = "\t", stringsAsFactors = F)
fluencyT2$PP <- as.factor(fluencyT2$PP)
fluencyT2[fluencyT2$Ouput=="?"]$Word <- NA

### read in fluency data T1 EnglisF
fluencyT1 <- read.delim("FluencyT1.txt", sep = "\t", stringsAsFactors = F)
fluencyT1$PP <- as.factor(fluencyT1$PP)
fluencyT1[fluencyT1$Ouput=="?"]$Word <- NA

# subset to people who were coded in both Ts
fluencyT1_sub <- fluencyT1[fluencyT1$PP %in% fluencyT2$PP,]
fluencyT1_sub$PP <- droplevels(fluencyT1_sub$PP)
fluencyT2_sub <- fluencyT2[fluencyT2$PP %in% fluencyT1$PP,]
fluencyT2_sub$PP <- droplevels(fluencyT2_sub$PP)
fluencyT1_sub <- fluencyT1_sub[-which(fluencyT1_sub$Language == "Deutsch"),]
#fluencyT2_sub <- fluencyT2_sub[-which(fluencyT2_sub$Language == "Deutsch"),]

fluencyT1_sub$Word <- as.numeric(fluencyT1_sub$Word)
fluencyT2_sub$Word <- as.numeric(fluencyT2_sub$Word)
dataflu <- as.data.frame(tapply(fluencyT1_sub$Word, list(fluencyT1_sub$PP, fluencyT1_sub$Category.Letter), sum, na.rm = T))
dataflu[,4] <- (dataflu$Berufe + dataflu$`Tiere an Land`) / 2
dataflu[,5] <- (dataflu$Berufe + dataflu$`Tiere an Land` + dataflu$F) / 3
dataflu[,6] <- rownames(dataflu)
colnames(dataflu)[4] <- "T1_catmean"
colnames(dataflu)[5] <- "T1_grandmean"
colnames(dataflu)[6] <- "PP"

dataflu2 <- as.data.frame(tapply(fluencyT2_sub$Word, list(fluencyT2_sub$PP, fluencyT2_sub$Category.Letter), sum, na.rm = T))
dataflu2[,4] <- (dataflu2$clothing + dataflu2$Obst) / 2
dataflu2[,5] <- (dataflu2$clothing + dataflu2$Obst + dataflu2$A) / 3
dataflu2[,6] <- rownames(dataflu2)
colnames(dataflu2)[4] <- "T2_catmean"
colnames(dataflu2)[5] <- "T2_grandmean"
colnames(dataflu2)[6] <- "PP"
datafull <- merge(dataflu, dataflu2, by = "PP")

#converting to long format for plotting 
dataflulong <- datafull %>% gather(category, sum, c(Berufe:T2_grandmean))
dataflulong$session <- NA
dataflulong[1: nrow(dataflulong)/2,]$session <- "T1"
dataflulong[(nrow(dataflulong)/2 + 1) : (nrow(dataflulong)),]$session <- "T2"
dataflulong$session <- as.factor(dataflulong$session)

subset <- dataflulong[which(dataflulong$category=="T1_grandmean" | dataflulong$category=="T2_grandmean"),]

lineplot <- ggplot(subset, aes(y = sum, x = session, group = PP))
lineplot + geom_point(color = "darkgrey") +
  geom_line(color = "darkgrey") +
  #geom_text(aes(label=PP)) +
  #coord_cartesian(ylim=c(0,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  ylab("English fluency performance (mean of absolute words produced for the 3 categories)") +
  theme_bw()

subset2 <- dataflulong[which(dataflulong$category=="T1_grandmean" | dataflulong$category=="T2_grandmean" | dataflulong$category=="T1_catmean" | dataflulong$category=="T2_catmean" | dataflulong$category=="A" | dataflulong$category=="F"),]

datadiff <- as.data.frame(subset2[subset2$session=="T2",]$sum - subset2[subset2$session=="T1",]$sum)
datadiff[,2] <- subset2[subset2$session=="T1",]$PP
datadiff[,3] <- subset2[subset2$session=="T1",]$category
colnames(datadiff) <- c("Diff", "PP", "category")
datadiff[datadiff$category=="F",]$category <- "Letter"
datadiff[datadiff$category=="T1_catmean",]$category <- "Category"
datadiff[datadiff$category=="T1_grandmean",]$category <- "Grandmean"

# take learning rate from other script 
combined2sub <- combined2[combined2$pp %in% fluencyT2_sub$PP,]
combinedsub <- combined[combined$pp %in% fluencyT2_sub$PP,]
learndiff <- as.data.frame(combined2sub[combined2sub$session==1,]$mean - combined2sub[combined2sub$session==2,]$mean)
learndiff[,2] <- combined2sub[combined2sub$session==1,]$pp
learndiff[,3] <- combinedsub[combinedsub$session==1,]$mean
learndiff[,4] <- combinedsub[combinedsub$session==2,]$mean
colnames(learndiff) <- c("Diff","PP", "T1mean", "T2mean")

# merging dataframes 
difference <- merge(datafull, learndiff, by = "PP")
difference$diff_cat <- difference$T2_catmean - difference$T1_catmean
difference$diff_ga <- difference$T2_grandmean - difference$T1_grandmean
difference$diff_letter <- difference$A - difference$F
#full <- merge(difference, dataflu, by = "PP")

# discard outliers, i.e. people with +- 2sd of the mean
#difference <- difference[-which(is.na(difference$T1_grandmean)==1),]
#for (i in 1: nrow(difference)){
#  val1 <- mean(difference$T1_grandmean, na.rm = T) - 2*sd(difference$T1_grandmean, na.rm = T)
#  val2 <- mean(difference$T1_grandmean, na.rm = T) + 2*sd(difference$T1_grandmean, na.rm = T)
#  
#   if (difference$T1_grandmean[i] < val1 || difference$T1_grandmean[i] > val2){
#     difference$T1_grandmean[i] <- NA
#   }
#  
#  if (difference$Berufe[i] < 5 | difference$F[i] < 5 | difference$`Tiere an Land`[i] < 5 | difference$A[i] < 5 | difference$clothing[i] < 5 | difference$Obst[i] < 5) {
#    difference$T1_grandmean[i] <- NA
#  }
#}

#difference <- difference[-which(is.na(difference$T1_grandmean)==1),]
 
# combine with frequency of use ratings
#combined$PP <- row.names(combined)
#combined <- combined[combined$PP %in% difference$PP,]
#full <- cbind(difference, combined, by = "PP")
#for (i in 1:nrow(difference)){
#  if (full$T2English[i] <20){
#    full$T2Engcat[i] <- "Infrequent"
#  } else {
#    full$T2Engcat[i] <- "Frequent"
#  }
#  }
#full$T2Engcat <- as.factor(full$T2Engcat)
#model <- lm(Diff ~ diff_ga*T2Engcat, data = full)
#ggplot(full, aes(x=Diff, y=diff_ga, group = T2Engcat)) + 
#  geom_point() +
#  geom_smooth(method="lm", se=TRUE, fullrange=T, level=0.95)+
#  facet_wrap(~T2Engcat) +
#  xlab("Spanish learning rate from T1 to T2 in %") +
#  ylab("Absolute fluency difference from T1 to T2")

# linear model 
model <- lm(Diff ~ diff_ga, data = difference)
summary(model)

# correlate them 
cor.test(difference$diff_ga, difference$Diff)

# plot the relationship 
ggplot(difference, aes(x=Diff, y=diff_ga)) + 
  geom_point() +
  geom_smooth(method="lm", se=TRUE, fullrange=T, level=0.95)+
  xlab("Spanish learning rate from T1 to T2 in %") +
  ylab("Absolute fluency difference from T1 to T2")

ggplot(difference, aes(x=T2mean, y=diff_ga)) + 
  geom_point() +
  geom_smooth(method="lm", se=TRUE, fullrange=T, level=0.95)+
  xlab("Spanish learning rate from T1 to T2 in %") +
  ylab("Absolute fluency difference from T1 to T2")

#### calculating z-scores for fluency tests
#difference <- difference[-which(is.na(difference$T1_grandmean)==1),]
#difference$T1_pop_sd <- sd(difference$T1_grandmean)*sqrt((length(difference$T1_grandmean)-1)/(length(difference$T1_grandmean)))
#difference$T1_pop_mean <- mean(difference$T1_grandmean)
#for (i in 1: nrow(difference)){
#  difference$T1_z <- (difference$T1_grandmean - difference$T1_pop_mean) / difference$T1_pop_sd
#}##

#difference$T2_pop_sd <- sd(difference$T2_grandmean)*sqrt((length(difference$T2_grandmean)-1)/(length(difference$T2_grandmean)))
#difference$T2_pop_mean <- mean(difference$T2_grandmean)
#for (i in 1: nrow(difference)){
#  difference$T2_z <- (difference$T2_grandmean - difference$T2_pop_mean) / difference$T2_pop_sd
#}

#difference$diff_z <- difference$T1_z - difference$T2_z

#model <- lm(Diff ~ diff_z, data = difference)
#summary(model)

# correlate them 
#cor.test(difference$diff_z, difference$Diff)

# plot the relationship 
#ggplot(difference, aes(x=Diff, y=diff_z)) + 
#  geom_point() +
#  geom_smooth(method="lm", se=TRUE, fullrange=T, level=0.95)+
#  xlab("Spanish learning rate from T1 to T2 in %") +
#  ylab("Absolute fluency difference from T1 to T2")



difference$Ref_T1_pop_sd <- sd(refmeans[is.na(refmeans$Berufe)==0,]$T1_grandmean)*sqrt((length(refmeans[is.na(refmeans$Berufe)==0,]$T1_grandmean)-1)/(length(refmeans[is.na(refmeans$Berufe)==0,]$T1_grandmean)))
difference$Ref_T1_pop_mean <- mean(refmeans[is.na(refmeans$Berufe)==0,]$T1_grandmean)
for (i in 1: nrow(difference)){
  difference$T1_ref <- (difference$T1_grandmean - difference$Ref_T1_pop_mean) / difference$Ref_T1_pop_sd
}

difference$Ref_T2_pop_sd <- sd(refmeans[is.na(refmeans$A)==0,]$T2_grandmean)*sqrt((length(refmeans[is.na(refmeans$A)==0,]$T2_grandmean)-1)/(length(refmeans[is.na(refmeans$A)==0,]$T2_grandmean)))
difference$Ref_T2_pop_mean <- mean(refmeans[is.na(refmeans$A)==0,]$T2_grandmean)
for (i in 1: nrow(difference)){
  difference$T2_ref <- (difference$T2_grandmean - difference$Ref_T2_pop_mean) / difference$Ref_T2_pop_sd
}

difference$diff_ref <- difference$T2_ref - difference$T1_ref

model <- lm(Diff ~ diff_ref, data = difference)
summary(model)

# correlate them 
cor.test(difference$diff_ref, difference$T2mean)

# plot the relationship 
ggplot(difference, aes(x=Diff, y=diff_ref)) + 
  geom_point() +
  geom_smooth(method="lm", se=TRUE, fullrange=T, level=0.95)+
  xlab("Spanish learning rate from T1 to T2 in %") +
  ylab("Reference-group weighted fluency difference from T1 to T2")

# directly compare the two reference groups 
t.test(refmeans[is.na(refmeans$A)==0,]$T2_grandmean, refmeans[is.na(refmeans$F)==0,]$T1_grandmean, type = 2)
t.test(refmeans[is.na(refmeans$A)==0,]$Control_grandmean,  refmeans[is.na(refmeans$F)==0,]$Control_grandmean, type = 2, na.rm = T)

### other stuff ###

hist(tapply(fluency$Word, list(fluency$PP), sum,  na.rm = T, data = fluency))
hist(by(fluency$Word, fluency$PP, sum, na.rm = T))

data <- aggregate(fluency$Word, list(fluency$PP), sum, na.rm = T)
data2 <- as.data.frame(table(fluency$Word, fluency$PP))
data$errors <- table(fluency$Word, fluency$PP)[1,]

lime <- read.csv("T2_questionnaire_download.csv")

for (i in 1:nrow(data)){
  num <- which(lime$ZugangsschlÃ.ssel == data$Group.1[i])
  data$sex[i] <- as.character(lime$Geschlecht[num])
}

model <- lm(x ~ sex, data = data)
summary(model)

cor.test(data$x, data$errors)
