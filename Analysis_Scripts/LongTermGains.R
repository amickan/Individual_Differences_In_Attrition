#### Does a study abroad have any long-term benefits? 

#### load packages ####
library(readxl)
library(ggplot2)
library(plyr)
library(lme4)
library(lmerTest)
library(dplyr)
library(effects)

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
Mcombined <- Mcombined[Mcombined$imgIndex > 3,] #excluding the first 5 items, trial index starts at 0
Mcombined$error <- Mcombined$error*100
# going for the dichotomous error coding
Mcombined$errorbin <- Mcombined$error
for (i in 1:nrow(Mcombined)){
  if (is.na(Mcombined$error[i])== 0 & Mcombined$error[i] != 0 & Mcombined$error[i] != 100){
    Mcombined$errorbin[i] <- 100
  }
}

# same analysis with fine-grained error rates
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


#### run a model that tests whether performance goes back to baseline 
Mcombined$session <- as.factor(Mcombined$session)
#Mcombined$session <- relevel(Mcombined$session, ref = "2")
contrasts(Mcombined$session) <- contr.treatment(3)
colnames(contrasts(Mcombined$session)) <- c("T1-T2", "T1-T3")

modelerror <- glmer(cbind(Corr, Incorr) ~ session + (1|ppn) + (1|imgFilename), 
                    family = binomial, 
                    control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), 
                    data = Mcombined)
summary(modelerror)

e <- effect("session",modelerror)
plot(e)


# plot the raw data
aggregatedfull <- ddply(Mcombined, .(session, ppn), 
                        plyr::summarise,
                        mean = mean(Ratio, na.rm = T),
                        sem = sd(Ratio, na.rm = T)/sqrt(length(Ratio)))

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
  #geom_text(data = combinedfull[combinedfull$session==3,], aes(label=ppn), size = 3, nudge_x = c(0.2)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_continuous(name = "Session", breaks = c(1,2,3),labels=c("T1","T2", "T3"), expand = c(0.1,0.1)) +
  ylab("% correct in Spanish") +
  #scale_color_manual(guide=F, "Language Condition", values=c("dodgerblue4","firebrick"),labels=c("Dutch","English")) +
  theme_bw()

