### Fluency performance control group ####

## What are we looking at in this script?
# I have 3 control groups, who each did either the T1, T2, or T3 fluency tasks from the actual experiment, as well as 4 common 
# control fluency tasks (2 in German and 2 in English). So this means we have data that looks like this: 
# C1: T1 fluency tasks (1 letter English, 2 categories English, 1 letter German, 2 categories German), 2 English control tasks (letter R, category furniture), 2 German control tasks (letter T, animals in water)
# C2: T2 fluency tasks (1 letter English, 2 categories English, 1 letter German, 2 categories German), 2 English control tasks (letter R, category furniture), 2 German control tasks (letter T, animals in water)
# C3: T3 fluency tasks (1 letter English, 2 categories English, 1 letter German, 2 categories German), 2 English control tasks (letter R, category furniture), 2 German control tasks (letter T, animals in water)
## see below for details on which category and letter was part of which T sessionn

## With this script, I'm actually only trying to figure out whether the T1, T2 and T3 tasks are comparable in difficulty.
# For that I first need to know whether my control groups are comparable 
# and then whether there are task differences (assessed between groups, so comparability of groups is a necessary prerequisite for this second analysis)
# Finally, I want to know whether or not I can just use the raw scores of the fluency tests for my real attitriters (so not the control group, 
# but the actual participants in the online study) as a predictor for Spanish proficiency changes over time. I would hope I can confirm with the control group 
# that all tasks are equally difficult and hence no adjustments are necessary, but if I find out that tasks aren't comparable in the control group, 
# then I'll need to figure out a way of accounting for that in the main analysis with the real participants, more on that later.

# read in packages
library(readxl)
library(tidyr)
library(MASS)
library(lme4)

## read in data
reference <- read.delim("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/Control_Fluency.txt")
# get the sum of words produced for each participant and each task (words that are counted correct have a 1 in the word column, hence a simple sum will do the job)
refmeans <- as.data.frame(tapply(reference$Word,list(reference$PP, reference$Category.Letter), sum, na.rm = T))
refmeans$Ppn <- row.names(refmeans)

# converting the wide dataframe to long format, add group and language column
data_long <- gather(refmeans, task, score, A:`Tiere im Wasser`, factor_key=TRUE)
data_long$Group <- NA
for (i in 1:nrow(data_long)){
  if (data_long$Ppn[i] %in% c("3013","3016", "3057", "3072", "3078", "3090", "3099", "3127", "3139", "3212", "3215", "3217", "3232")){
    data_long$Group[i] <- 1 # C1
  }
  if (data_long$Ppn[i] %in% c("3023", "3039" ,"3042" ,"3064" ,"3070" ,"3073", "3079", "3085", "3106" ,"3112" ,"3162", "3175", "3199" ,"3265")){
    data_long$Group[i] <- 2 # C2
  }
  if (data_long$Ppn[i] %in% c("3037", "3059", "3062", "3065", "3080", "3147", "3153", "3173" ,"3180", "3181" ,"3250" ,"3254", "3270")){
    data_long$Group[i] <- 3 # C3
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
# subset to control tasks only because here we are comparing groups on the tasks that they all did, the common control tasks thus
data_sub <- data_long[data_long$task %in% c('R', 'T', 'Möbel', 'Tiere im Wasser'),]
# prepare dataframe 
data_sub$Group <- as.factor(data_sub$Group)
contrasts(data_sub$Group) <- contr.sdif(3) # comparing C1 to C2 and C2 to C3, see below for a contrast coding that compares C1 to C2 and C1 to C3 
data_sub$Language <- as.factor(data_sub$Language)
contrasts(data_sub$Language) <- c(0.5, -0.5)
data_sub$Ppn <- as.factor(data_sub$Ppn)
data_sub$task <- droplevels(data_sub$task) # this was a factor already, so we need to drop levels
#contrasts(data_sub$task) <- ? # not sure which contrast coding to go for here, there is no meaningful reference level, 
# but there is also no meaningful other comparison, I would like to compare every task with every other task, for which there is no contrast coding anyway

## we can either run a model with language and group, or a model with task and group, or just a model with group
# (a certain task is only ever done in one language, so putting both Language and Task in the model together doesn't make sense)
# Which of the three is to be preferred? I ultimately care about performance on individual tasks, of course, 
# but with this first analysis I just want to know whether groups differ overall, and not on how they possibly differ 
# on individual tasks, so maybe I should only test for group differences at this stage? 
model0 <- lmer(score ~ Group + (1|Ppn), data = data_sub)
summary(model0)
# Group 1 differs from group 2, group 2 and 3 don't differ; see further below for group 1 and 3 comparison with different contrast coding
model1 <- lmer(score ~ Language*Group + (1|Ppn), data = data_sub)
summary(model1)
## Again, group 1 differs from group 2, but group 2 does not differ from group 3 
## in addition, interestingly, groups differ in how much they differ between the languages (group two just seems to be better at English than the other two, hence the difference)
model2 <- lmer(score ~ task*Group + (1|Ppn), data = data_sub)
summary(model2)
## This time, overall the groups don't seem to differ from each other
## but for some of the categories (i.e. tasks) they do: the pattern is a bit complex to interpret due to the contrast coding: but if I understand correctly,
## group 2 and 3 differ in how differently they score on T vs. furniture (this is the reference category for task) and similarly, 
## it appears that group 1 and 2 differ marginally in how they differ in performance on R vs. furniture
## there might be other group difference between tasks that we don't see with 'furniture' as the reference task... 
## but I guess this just reinforces the earlier model that there are differences between groups in certain tasks, which I guess ultimately stem from differences
## in how the groups perform on German vs. English tasks --> it seems to me that the first model is then the easier one to interpret, am I right? 
## I'm a bit puzzled as to why the overall group differences are different in this model, based on the third model we'd conclude the groups don't differ overall, 
## based on the first two though we'd conclude groups 1 and 2 differ -- which one is it? 

# change the contrast coding of the group variable to test also for difference between group 1 and 3 
c <- contr.treatment(3)  # dummy coding
my.coding<-matrix(rep(1/3, 6), ncol=2)
my.simple<-c-my.coding # contrast 1 compares group 1 and 2, and contrast 2 compares group 1 and 3, with the intercept reflecting the grand mean
contrasts(data_sub$Group) <- my.simple
model5 <- lmer(score ~ Group + (1|Ppn), data = data_sub)
summary(model5)
model3 <- lmer(score ~ Language*Group + (1|Ppn), data = data_sub)
summary(model3)
model4 <- lmer(score ~ task*Group + (1|Ppn), data = data_sub)
summary(model4)
## no matter which model you run, group 1 does not differ from group 3 overall, and the two groups also don't seem to differ in how they perform on English vs German, 
## or on any of the tasks specifically either, for that matter 

### So based on these models, I guess we can conclude that groups 1 and 3 are reasonably comparable, but group 2 is the odd one out, 
## the difference being their English performance
## So how do we proceed from here? Given that groups differ slightly, I'm not sure it makes sense to look for item differences between groups
## on the non-control tasks, differences will be hard to interpret 

## I thought that now it would make sense to add a control task performance score for each person (or group?) to the model, or better even 
## a control score for each person (or each group?) and language combination, and add this (or these) as covariates to the model

# calculate mean performance on German and English control tasks per person using the wide dataframe from the beginning 
refmeans$Control_mean_German <- (refmeans$T + refmeans$`Tiere im Wasser`)
refmeans$Control_mean_English <- (refmeans$R + refmeans$Möbel)/2
# add this new score to the long dataframe 
data_long$ControlGerman <- NA
data_long$ControlEnglish <- NA
for (i in 1:length(unique(data_long$Ppn))){
  pnum <- unique(data_long$Ppn)[i]
  data_long[data_long$Ppn==pnum,]$ControlGerman <- refmeans[refmeans$Ppn==pnum,]$Control_mean_German
  data_long[data_long$Ppn==pnum,]$ControlEnglish <- refmeans[refmeans$Ppn==pnum,]$Control_mean_English
}

### Are there differences between items? ###
# subset to leave out the control tasks 
data_sub <- data_long[-which(data_long$task %in% c('R', 'T', 'Möbel', 'Tiere im Wasser')),]
data_sub$Group <- as.factor(data_sub$Group)
contrasts(data_sub$Group) <- contr.sdif(3)
data_sub$Language <- as.factor(data_sub$Language)
contrasts(data_sub$Language) <- c(0.5, -0.5)
data_sub$Ppn <- as.factor(data_sub$Ppn)
data_sub$task <- droplevels(data_sub$task)
## how should we set up the contrasts for task? same issue as before... 

## run a model with only task, Group makes no sense to add here because each task is only done by one group, so a model with Group would be rank deficient
## add the control task performance as covariates
model6 <- lmer(score ~ task + ControlGerman + ControlEnglish + (1|Ppn), data = data_sub)
summary(model6)
# not quite sure how meaningful this model is, it basically tells us that all tasks differ from the reference task, 
# I would need to repeat this model lots of times with a different reference category every time... seems very inefficient to me
# I ultimately want to know how the T1 tasks compare to the T2 tasks and T3 tasks and vice versa, so maybe I should actually be calculating averages over all 
# German and English tasks at each of the time points, and then add Language and Session as fixed effects and also the control performance as covariate?

# adding a session variable to the dataframe 
data_long$session <- NA
for (i in 1:nrow(data_long)){
  if (data_long$task[i] %in% c("F","P", "Tiere an Land", "Berufe", "Küchenutensilien", "Gemüse")){
    data_long$session[i] <- "T1"
  }
  if (data_long$task[i] %in% c("A", "M" ,"Kleidung" ,"Obst", "Hygiene/Badartikel", "Fortbewegungsmittel")){
    data_long$session[i] <- "T2"
  }
  if (data_long$task[i] %in% c("D", "B", "Sport" ,"Büroartikel" ,"Körperteile", "Elektronika")){
    data_long$session[i] <- "T3"
  }
  if (data_long$task[i] %in% c("R","T" ,"Möbel" ,"Tiere im Wasser")){
    data_long$session[i] <- "Control"
  }
}

# subset and prepare dataframe 
data_sub <- data_long[-which(data_long$task %in% c('R', 'T', 'Möbel', 'Tiere im Wasser')),]
data_sub$Group <- as.factor(data_sub$Group)
contrasts(data_sub$Group) <- contr.sdif(3)
data_sub$Language <- as.factor(data_sub$Language)
contrasts(data_sub$Language) <- c(0.5, -0.5)
data_sub$Ppn <- as.factor(data_sub$Ppn)
data_sub$task <- droplevels(data_sub$task)
data_sub$session <- as.factor(data_sub$session)
contrasts(data_sub$session) <- contr.sdif(3) # repeated makes most sense to me here, comparing T1 to T2, and T2 to T3 - those are also the comparisons I care about in the actual analysis with the real participants

model7 <- lm(score ~ session*Language + ControlGerman + ControlEnglish, data = data_sub) # there is no longer a need for a random intercept over subjects, hence just a linear model here
summary(model7)
## based on this model, if I understand correctly what the covariates are doing, I would conclude that:
## T1 and T2 tasks don't differ, i.e. are comaprable, when taking a priori group differences into account 
## tasks from T2 and T3 do differ though (main effect for that contrast), both the English and the German tasks of those sessions since there is no interaction with Language 
## There is also a main effect of language but that's neither suprising nor relevant really, it just means that English tasks are harder overall than German tasks for the German natives we tested here 
## Do I need to take anything from the fact that there is a main effect of control task performance in German, but not in English - 
## what does that mean here? control performance in German is significantly different from 0 and in English it is not? 

## Iff I did this right, and interpreted the outcome correctly, I need to figure out a way to control / account for task difficulty differences between T2 and T3 
## in the main analysis with the real attriters. In the actual analysis with the real attriters (the Germans that went to Spain), I want to include fluency 
## in German and English (the difference between T2 and T3 per participant, simply the session averages T2 - T3) as a predictor for the 
## Spanish picture naming performance (the interaction of Spanish score and session which indicates forgetting / learning from session 2 to 3). 
## The prediction would be that as fluency in other languages (i.e. German and English) goes up, performance in Spanish goes down. there is a proficiency trade-off, simply put.
## Of course for such a fluency difference score to be meaningful I need the T2 and T3 fluency tasks to be comparable in difficulty, which apparently they are not.
## I have no idea how I could account for differences in difficulty between the sessions in a model with a completely unrelated DV (performance in Spanish). 
## Should I adjust each participants fluency difference score by some value that captures T2 and T3 fluency difficulty differences? Not sure how I would do that. 

## Alternatively, I could argue that the differences in difficulty between fluency tasks are the same for all participants, 
## so the differences between participants in the fluency difference score are still meaningful. 
## So I could run the final analyses with the real attritiers without making any adjustments to the fluency scores, 
## the only downside here is that I then cannot interpret the direction of the effect:
## because an increase in fluency from T2 to T3 in German, for example, in such a scenario doesn't necessarily mean that fluency actually increased, 
## it could just be that the T3 fluency tasks were easier and that a positive difference score actually reflects no change in fluency at all. 
## Because of that interpreting any interaction between the fluency difference score and the Spanish forgetting rate in the final model would be
## difficult: a positive correlation between the two could mean either:
## - that people with a fluency increase in German forgot more Spanish, or 
## - (if the fluency increase is really just an index of T3 being easier than T2) it could mean that people whose German fluency did not change much (they have a positive fluency difference, but only because the tasks at T3 were easier), forgot more, 
## and people who decreased in German fluency (and hence have possibly a fluency difference score of 0) forgot less. 
## Essentially conclusions would be opposite depending on what the fluency difference indicates. You see what I mean?  
## So I guess what I want to say is that I'd prefer to have a sure way of knowing what the fluency difference score means (i.e. a positive score = fluency increase), and I'm not sure how to do that.

## Sorry for all the text! 
## Any thoughts on the analyses above (maybe I did them wrong and tasks are comparable after all!) as well as how to do the ultimate analysis (which is really what matters most to me!) would be very much appreciated. 

## Also: Merry Christmas :-) 