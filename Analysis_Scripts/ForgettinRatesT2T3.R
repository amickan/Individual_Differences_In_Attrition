#### Analysis of T2 and T3 data - forgetting rates and their predictors #### 

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

ppn <- read.delim("PPN_final.txt", header = F)

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
  for (i in 1:length(nums)){
  T2sub <- T2sub[-which(T2sub$ppn == nums[i]),]}
}
nums <- which(table(T3sub$ppn) < 144)
nums <- names(nums)
if (length(nums) > 0){
  T3sub <- T3sub[-which(T3sub$ppn == nums),]
}

T2sub$ppn <- droplevels(T2sub$ppn)
T3sub$ppn <- droplevels(T3sub$ppn)

# subset to people who have been coded for both sessions
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
T2sub <- T2sub[T2sub$imgIndex > 4,] #excluding the first 5 items, trial index starts at 0
T3sub <- T3sub[T3sub$imgIndex > 4,]

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

### decide whether to plot and do analyses with dichotomous codings or not
Mcombined$errorfine <- Mcombined$error
T2sub$errorfine <- T2sub$error
T3sub$errorfine <- T3sub$error
dichotomous = TRUE
if (dichotomous == T){
  Mcombined$error <- Mcombined$errorbin
  T2sub$error <- T2sub$errorbin
  T3sub$error <- T3sub$errorbin
} else {
  Mcombined$error <- Mcombined$errorfine
  T2sub$error <- T2sub$errorfine
  T3sub$error <- T3sub$errorfine
}

####### PLOTTING #############
# plot performance (error in %)
#hist(tapply(T3sub$error, list(T3sub$ppn), mean, na.rm = T))
#hist(tapply(T2sub$error, list(T2sub$ppn), mean, na.rm = T))

#### Plot forgetting rates ####
aggregated <- ddply(Mcombined, .(session, ppn), 
                             plyr::summarise,
                             mean = mean(error),
                             sem = sd(error)/sqrt(length(error)))


aggregatedmean <- ddply(aggregated, .(session), 
                    plyr::summarise,
                    condition_mean = mean(mean),
                    condition_sem = sd(mean)/sqrt(length(mean)))

combined <- merge(aggregated, aggregatedmean, by = c("session"))

combined$diff <- rep((combined[combined$session == 3,]$mean - combined[combined$session == 2,]$mean), 2)

for (i in 1:nrow(combined)){
  if (combined$diff[i] > 0){
    combined$sign[i] <- "pos"
  } else {
    combined$sign[i] <- "neg"
  }
}
combined$sign <- as.factor(combined$sign)
combined$session <- as.factor(combined$session)

## plot absolute error rates per person and grand mean in red on top 
lineplot <- ggplot(combined, aes(y = mean, x = session, group = ppn, fill = sign))
lineplot + geom_point(aes(color = sign), show.legend = FALSE) +
  geom_line(aes(color = sign)) +
  scale_color_manual(name = "direction", values = c("darkblue", "lightblue"))+
  geom_point(aes(y = condition_mean), color = "red", show.legend = FALSE) + 
  geom_line(aes(y = condition_mean), color = "red", size = 1, show.legend = FALSE) + 
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                    width = 0.5, color = "red", size = 0.5, show.legend = FALSE) +
  geom_text(aes(label=ppn), size = 3, nudge_x = c(0.1), show.legend = FALSE) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_discrete(name = "Session", breaks = c(2,3),labels=c("T2", "T3"), expand = c(0.1,0.1)) +
  ylab("Error rates") +
  theme_bw()

## plot absolute error rates per person by color, no grand mean on top
lineplot <- ggplot(combined, aes(y = mean, x = session, group = ppn))
lineplot + geom_point(aes(color = ppn)) +
  geom_line(aes(color = ppn)) +
  geom_text(data = combined[combined$session==3,], aes(label=ppn), size = 3, nudge_x = c(0.1)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_discrete(name = "Session", breaks = c(2,3),labels=c("T2", "T3"), expand = c(0.1,0.1)) +
  ylab("Error rates") +
  theme_bw() +
  theme(legend.position = "none")

### Partial analysis ###
# only look at forgetting within the words that were known at T2
T3sub$errort2 <- NA
ppn <- unique(T2sub$ppn)
for (i in 1:length(ppn)) {
  ppnn <- ppn[i]
  for (k in 1:length(T3sub[T3sub$ppn == ppnn,]$imgFilename)){
    num <- which(T2sub[T2sub$ppn == ppnn,]$imgFilename == T3sub[T3sub$ppn == ppnn,]$imgFilename[k])
      if (length(num) == 1){
        T3sub[T3sub$ppn == ppnn,]$errort2[k] <- T2sub[T2sub$ppn == ppnn,]$error[num]
      }
  }
}

T3sub3 <- T3sub[T3sub$errort2 == 0,]
T2sub3 <- T2sub[T2sub$error == 0,]

T3sub3[is.na(T3sub3$ppn) == 0,]-> T3final
T2sub3[is.na(T2sub3$ppn) == 0,]-> T2final

T3sub3 <-T3sub3[,-18]
Mcombined2 <- rbind(T2sub3, T3sub3)
Mcombined2$session <- rep(c(2,3), each = nrow(T2sub3)) 
Mcombined2$error <- Mcombined2$error*100

aggregated2 <- ddply(Mcombined2, .(session, ppn), 
                    plyr::summarise,
                    mean = mean(error),
                    sem = sd(error)/sqrt(length(error)))


aggregatedmean2 <- ddply(aggregated2, .(session), 
                        plyr::summarise,
                        condition_mean = mean(mean),
                        condition_sem = sd(mean)/sqrt(length(mean)))

combined2 <- merge(aggregated2, aggregatedmean2, by = c("session"))

### plot percentage forgotten from T2 to T3 (based on words known at T2)
lineplot <- ggplot(combined2, aes(y = mean, x = session, group = ppn))
lineplot + geom_point(color = "darkgrey") +
  geom_line(color = "darkgrey") +
  geom_point(aes(y = condition_mean), color = "red") + 
  geom_line(aes(y = condition_mean), color = "red", size = 1) + 
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, color = "red", size = 1) +
  geom_text(aes(label=ppn), size = 3, nudge_x = c(0.1)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_discrete(name = "Session", labels=c("T2", "T3"), breaks = c(2,3), expand = c(0.1,0.1)) +
  ylab("Error rates (only including words that were known at T2)") +
  #scale_color_manual(guide=F, "Language Condition", values=c("dodgerblue4","firebrick"),labels=c("Dutch","English")) +
  theme_bw()

### plot percentage forgotten from T2 to T3 (based on words known at T2) withouth grand mean
lineplot <- ggplot(combined2, aes(y = mean, x = session, group = ppn))
lineplot + geom_point(aes(color = ppn)) +
  geom_line(aes(color = ppn)) +
  geom_text(data = combined2[combined2$session=="3",], aes(label=ppn), size = 3, nudge_x = c(0.1)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_discrete(name = "Session", labels=c("T2", "T3"), breaks = c(2,3), expand = c(0.1,0.1)) +
  ylab("Error rates (only including words that were known at T2)") +
  theme_bw() +
  theme(legend.position = "none")

### plot how many out of the unknown words at T2 people actualy learned by T3
T3sub3_learn <- T3sub[T3sub$errort2 == 1,]
T2sub3_learn <- T2sub[T2sub$error == 1,]
T3sub3_learn[is.na(T2sub3_learn$ppn) == 0,]-> T3final_learn
T2sub3_learn[is.na(T2sub3_learn$ppn) == 0,]-> T2final_learn

T3sub3_learn <-T3sub3_learn[,-18]
Mcombined2_learn <- rbind(T2sub3_learn, T3sub3_learn)
Mcombined2_learn$session <- rep(c(2,3), each = nrow(T2sub3_learn)) 
Mcombined2_learn$error <- Mcombined2_learn$error*100

aggregated2_learn <- ddply(Mcombined2_learn, .(session, ppn), 
                     plyr::summarise,
                     mean = mean(error),
                     sem = sd(error)/sqrt(length(error)))


aggregatedmean2_learn <- ddply(aggregated2_learn, .(session), 
                         plyr::summarise,
                         condition_mean = mean(mean),
                         condition_sem = sd(mean)/sqrt(length(mean)))

combined2_learn <- merge(aggregated2_learn, aggregatedmean2_learn, by = c("session"))

### plot percentage learned from T2 to T3 (based on words unknown at T2) with grand mean
lineplot <- ggplot(combined2_learn, aes(y = mean, x = session, group = ppn))
lineplot + geom_point(color = "darkgrey") +
  geom_line(color = "darkgrey") +
  geom_point(aes(y = condition_mean), color = "red") + 
  geom_line(aes(y = condition_mean), color = "red", size = 1) + 
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, color = "red", size = 1) +
  geom_text(data = combined2_learn[combined2_learn$session=="3",], aes(label=ppn), size = 3, nudge_x = c(0.1)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_continuous(name = "Session", labels=c("T2", "T3"), breaks = c(2,3), expand = c(0.1,0.1)) +
  ylab("Error rates (only including words that were unknown at T2)") +
  theme_bw() +
  theme(legend.position = "none")

### plot percentage learned from T2 to T3 (based on words unknown at T2) withouth grand mean
lineplot <- ggplot(combined2_learn, aes(y = mean, x = session, group = ppn))
lineplot + geom_point(aes(color = ppn)) +
  geom_line(aes(color = ppn)) +
  geom_text(data = combined2_learn[combined2_learn$session=="3",], aes(label=ppn), size = 3, nudge_x = c(0.1)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_continuous(name = "Session", labels=c("T2", "T3"), breaks = c(2,3), expand = c(0.1,0.1)) +
  ylab("Error rates (only including words that were unknown at T2)") +
  theme_bw() +
  theme(legend.position = "none")


#### Other descriptive stats ####
### item plots ###
# turn all partially known items into unknown items for the sake of this plot 
Mcombined$error2 <- Mcombined$error
Mcombined[which(Mcombined$error<100 & Mcombined$error>0),]$error2 <- 100
items<- ddply(Mcombined, .(session, imgFilename), 
                           plyr::summarise,
                           sum = sum(error2/100),
                           mean = mean(error2),
                           sem = sd(error2)/sqrt(length(error2)))
items$img <- gsub(".png", "", items$imgFilename)
itemsT2 <- items[items$session==2,]
itemsT3 <- items[items$session==3,]

itemsT2 <- itemsT2[order(-itemsT2$sum),]
coms <- itemsT2$img
numbers <- NA
for (i in 1:length(coms)){
  num <- which(itemsT3$img == coms[i])
  numbers[i] <- num 
}
itemsT3 <- itemsT3[numbers,]

itemsT3$img <- factor(itemsT3$img, levels = itemsT3$img)
itemsT2$img <- factor(itemsT2$img, levels = itemsT2$img)
itemsall <- rbind(itemsT2, itemsT3)
itemsall$session <- as.factor(itemsall$session)

barplot <- ggplot(itemsall, aes(x = img, y = sum, fill = session))
barplot + geom_bar(stat = "identity", position = "identity", alpha=.7) +
  theme(axis.title = element_text(size = 20)) + 
  scale_fill_manual(values = c("darkblue", "lightblue")) +
  ylab("Number of people who got the word wrong") +
  xlab("Items") +
  coord_flip()+
  theme_bw() +
  theme(axis.text=element_text(size=5))

## plot difference between T2 and T3 per items
itemsdiff <- items[items$session == "3",]
itemsdiff$diff <- items[items$session == "3",]$sum -  items[items$session == "2",]$sum 
itemsdiff <- transform(itemsdiff, img = reorder(img, -diff))

barplot <- ggplot(itemsdiff, aes(x = img, y = diff))
barplot + geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.title = element_text(size = 20)) + 
  ylab("N people who did not know the word at T3 - N people who did not know the word at T2") +
  xlab("Items") +
  coord_flip()+
  theme_bw() +
  theme(axis.text=element_text(size=5), legend.position = "none")

### which items out of the incorrect items were given synonyms (rather than NA)
Mcombincorr <- Mcombined[Mcombined$error==100,]
itemssyn<- ddply(Mcombincorr, .(session, imgFilename), 
              plyr::summarise,
              sum = sum(!is.na(response)))
itemssyn$img <- gsub(".png", "", itemssyn$imgFilename)

# choose session for which you want this plot (2 or 3)
sess = 3
if (sess == 3){
  itemssynT3 <- itemssyn[itemssyn$session=="3",]
  itemsT3 <- itemsT3[order(-itemsT3$sum),]
  itemsT3$img <- factor(itemsT3$img, levels = itemsT3$img)
  itemsT3 <- itemsT3[, -c(4,5)]
  k = 1
  extra <- matrix(NA,nrow(subset(itemsT3,!(itemsT3$imgFilename %in% itemssynT3$imgFilename))),4)
  for (i in 1: nrow(itemsT3)){
    if (itemsT3$imgFilename[i] %in% itemssynT3$imgFilename){
    } else {
      extra[k,1] <- itemsT3$session[i]
      extra[k,2] <- itemsT3$imgFilename[i]
      extra[k,3] <- 0
      extra[k,4] <- gsub(".png", "", itemsT3$imgFilename[i])
      k <- k+1
    }}
  colnames(extra) <- c("session", "imgFilename", "sum", "img")
  itemssynT3 <- rbind(itemssynT3, extra)
  itemsT3$img <- gsub(".png", "", itemsT3$imgFilename)
  
  itemsT3 <- itemsT3[order(-itemsT3$sum),]
  coms <- itemsT3$img
  numbers <- NA
  for (i in 1:length(coms)){
    num <- which(itemssynT3$img == coms[i])
    numbers[i] <- num 
  }
  itemssynT3 <- itemssynT3[numbers,]
  itemssynT3$img <- factor(itemssynT3$img, levels = itemssynT3$img)
  itemsT3$img <- factor(itemsT3$img, levels = itemsT3$img)
  itemssynT3_T3 <- rbind(itemsT3, itemssynT3)
  itemssynT3_T3$type <- rep(c("Number of unknown words at T3","Number of not-NA values for this word at T3"), c(nrow(itemsT3), nrow(itemssynT3))) 
  itemssynT3_T3$sum <- as.numeric(itemssynT3_T3$sum)
  
  barplot <- ggplot(itemssynT3_T3, aes(x = img, y = sum, fill = type))
  barplot + geom_bar(stat = "identity", position = "identity") +
    theme(axis.title = element_text(size = 20)) + 
    scale_fill_manual(values = c("plum4", "plum2")) +
    #ylab("Number of words") +
    xlab("Items") +
    coord_flip()+
    theme_bw() +
    theme(axis.text=element_text(size=5))
  
  } else if (sess == 2){
  itemssynT2 <- itemssyn[itemssyn$session=="2",]
  itemsT2 <- itemsT2[order(-itemsT2$sum),]
  itemsT2$img <- factor(itemsT2$img, levels = itemsT2$img)
  itemsT2 <- itemsT2[, -c(4,5)]
  k = 1
  extra <- matrix(NA,nrow(subset(itemsT2,!(itemsT2$imgFilename %in% itemssynT2$imgFilename))),4)
    for (i in 1: nrow(itemsT2)){
      if (itemsT2$imgFilename[i] %in% itemssynT2$imgFilename){
      } else {
        extra[k,1] <- itemsT2$session[i]
        extra[k,2] <- itemsT2$imgFilename[i]
        extra[k,3] <- 0
        extra[k,4] <- gsub(".png", "", itemsT2$imgFilename[i])
        k <- k+1
      }}
  colnames(extra) <- c("session", "imgFilename", "sum", "img")
  itemssynT2 <- rbind(itemssynT2, extra)
  itemsT2$img <- gsub(".png", "", itemsT2$imgFilename)
  
  itemsT2 <- itemsT2[order(-itemsT2$sum),]
  coms <- itemsT2$img
  numbers <- NA
  for (i in 1:length(coms)){
    num <- which(itemssynT2$img == coms[i])
    numbers[i] <- num 
  }
  itemssynT2 <- itemssynT2[numbers,]
  
  itemssynT2$img <- factor(itemssynT2$img, levels = itemssynT2$img)
  itemsT2$img <- factor(itemsT2$img, levels = itemsT2$img)
  itemssynT2_T2 <- rbind(itemsT2, itemssynT2)
  itemssynT2_T2$type <- rep(c("Number of unknown words at T2","Number of not-NA values for this word at T2"), c(nrow(itemsT2), nrow(itemssynT2)))
  itemssynT2_T2$sum <- as.numeric(itemssynT2_T2$sum)
  
  barplot <- ggplot(itemssynT2_T2, aes(x = img, y = sum, fill = type))
  barplot + geom_bar(stat = "identity", position = "identity") +
    theme(axis.title = element_text(size = 20)) + 
    scale_fill_manual(values = c("plum4", "plum2")) +
    #ylab("Number of words") +
    xlab("Items") +
    coord_flip()+
    theme_bw() +
    theme(axis.text=element_text(size=5))
  }

#### ANALYSIS #####

if (dichotomous == F){
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
          # pendientes synonym
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
    }

#### Full model with all data from both sessions ####
Mcombined$session <- as.factor(Mcombined$session)
contrasts(Mcombined$session) <- c(-0.5, 0.5)

## what predicts changes in error rate from T2 to T3?
individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

## add predictors to the full dataframe 
colnames(individualdiff)[1] <- "ppn"
df <- merge(Mcombined, individualdiff, by = "ppn")
df$GermanEngRatio_T2_T3 <- df$T2_T3_English/df$T2_T3_German
#df <- df[which(is.na(df$Ratio)==0),]
#df$ppn <- droplevels(df$ppn)
df$imgFilename <- as.factor(df$imgFilename)
df$Mot_T2_T3_avg <- (df$Mot_T3_pos_avg+df$Mot_T2_pos_avg)/2
#df$Mot_T2_T3_diff <- df$Mot_T3_pos_avg-df$Mot_T2_pos_avg
df$SRP_Spanish_T2_T3 <- df$T3_SRP_Spanish_avg - df$T2_SRP_Spanish_avg
df$Immersion <- (df$T3_LivingSitGermany_Spanish + df$T3_KeepUpSpanishActively + df$T3_MaintainedSpanishContact + df$T3_WatchSpanishMovies + df$T3_ReadSpanishBooks)/5
df$Mot_T2_T3_anxiety_avg <- (df$Mot_T3_anxiety+df$Mot_T2_anxiety)/2
T2perf <- as.data.frame(tapply(T2sub$error, T2sub$ppn, mean))
for (i in 1:length(unique(df$ppn))){
  pnum <- unique(df$ppn)[i]
  num <- which(rownames(T2perf)==pnum)
  for (k in 1:nrow(df[df$ppn == pnum,])){
    df[df$ppn == pnum,]$T2perf[k] <- T2perf[num,1]}
}



if (dichotomous = F) {
      ## full model with session as a factor in the model 
      modelfull <- glmer(cbind(Corr, Incorr) ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = Mcombined)
      summary(modelfull)
      # model comparisons 
      modelnull <- glmer(cbind(Corr, Incorr) ~ 1 + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = Mcombined)
      anova(modelnull, modelfull)
      
      # does average Spanish frequency of use after study abroad predict forgetting / learning rates between T2 and T3 
      
      # test each predictor seperately and assess whether its inclusion improves model fit 
      modelsess <- glmer(cbind(Corr, Incorr) ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelsess)
      modelSpanishFreq <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_Spanish) +
                                  (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelSpanishFreq)
      anova(modelsess, modelSpanishFreq) ### Spanish frequency of use improves model fit  :-)
      
      modelGermanEngl <- glmer(cbind(Corr, Incorr) ~ session*scale(GermanEngRatio_T2_T3) +
                                  (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelGermanEngl)
      anova(modelsess, modelGermanEngl) ### Ratio of German to English frequency of use idoes not improve model fit.
      
      modelEnglFreq <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_English) +
                                 (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelEnglFreq)
      anova(modelsess, modelEnglFreq) ### English frequency of use improves model fit.
      
      modelGerFreq <- glmer(cbind(Corr, Incorr) ~ session*scale(T2_T3_German) +
                               (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelGerFreq)
      anova(modelsess, modelGerFreq) ### German frequency of use improves model fit.
      
      modelMotivation <- glmer(cbind(Corr, Incorr) ~ session*scale(Mot_T2_T3_avg) +
                                 (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelMotivation)
      anova(modelsess, modelMotivation) ### Motivation improves model fit :-)
      
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
      
      modelAge <- glmer(cbind(Corr, Incorr) ~ session*scale(Age) +
                                 (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelAge)
      anova(modelsess, modelAge) ### Age improves model fit
      
      modelGender <- glmer(cbind(Corr, Incorr) ~ session*Gender +
                          (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelGender)
      anova(modelsess, modelGender) ### Age improves model fit
      
      
      modelDoor <- glmer(cbind(Corr, Incorr) ~ session*DoorsScore +
                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelDoor)
      anova(modelsess, modelDoor) ### Memory capacity marginally improves model fit
      
      modelT2success <- glmer(cbind(Corr, Incorr) ~ session*scale(T2perf) +
                                (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelT2success)
      anova(modelsess, modelT2success) ### T2 performance does improve model fit
      
      ### full model with all significant predictors in it 
      modelfull <- glmer(cbind(Corr, Incorr) ~ session*scale(Immersion) +
                           session*scale(T2_T3_Spanish) +
                           session*scale(T2_T3_German) +
                           session*scale(SRP_Spanish_T2_T3) +
                           session*scale(Mot_T2_T3_avg) +
                           session*scale(T2perf) +
                          (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
      summary(modelfull)
      
      # plotting the significant interactions from the final model only 
      
      # Spanish frequency of use
      interact_plot(modelfull, pred = T2_T3_Spanish, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
      e <- effect("session*scale(T2_T3_Spanish)",modelfull)
      plot(e)
      # German frequency of use 
      interact_plot(modelfull, pred = T2_T3_German, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
      e <- effect("session*scale(T2_T3_German)",modelfull)
      plot(e)
      # Spanish proficiency self-rated
      interact_plot(modelfull, pred = SRP_Spanish_T2_T3, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
      e <- effect("session*scale(SRP_Spanish_T2_T3)",modelfull)
      plot(e)
      # Continued immersion in Spanish culture after return to Germany
      interact_plot(modelfull, pred = Immersion, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
      e <- effect("session*scale(Immersion)",modelfull)
      plot(e)
      # T2 performance 
      interact_plot(modelfull, pred = T2perf, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
      e <- effect("session*scale(T2perf)",modelfull)
      plot(e)
  
      
} else {
  df[df$error==100,]$error <- 1
  ## full model with session as a factor in the model 
  modelfull <- glmer(error ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelfull)
  # model comparisons 
  modelnull <- glmer(error ~ 1 + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  anova(modelnull, modelfull)
  
  # does average Spanish frequency of use after study abroad predict forgetting / learning rates between T2 and T3 
  
  # test each predictor seperately and assess whether its inclusion improves model fit 
  modelsess <- glmer(error ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelsess)
  modelSpanishFreq <- glmer(error ~ session*scale(T2_T3_Spanish) +
                              (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelSpanishFreq)
  anova(modelsess, modelSpanishFreq) ### Spanish frequency of use improves model fit  :-)
  
  modelGermanEngl <- glmer(error ~ session*scale(GermanEngRatio_T2_T3) +
                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelGermanEngl)
  anova(modelsess, modelGermanEngl) ### Ratio of German to English frequency of use idoes not improve model fit.
  
  modelEnglFreq <- glmer(error ~ session*scale(T2_T3_English) +
                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelEnglFreq)
  anova(modelsess, modelEnglFreq) ### English frequency of use does NOT improve model fit.
  
  modelGerFreq <- glmer(error ~ session*scale(T2_T3_German) +
                          (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelGerFreq)
  anova(modelsess, modelGerFreq) ### German frequency of use improves model fit.
  
  modelMotivation <- glmer(error ~ session*scale(Mot_T2_T3_avg) +
                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelMotivation)
  anova(modelsess, modelMotivation) ### Motivation improves model fit :-)
  
  modelSRpSpanish <- glmer(error ~ session*scale(SRP_Spanish_T2_T3) +
                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelSRpSpanish)
  anova(modelsess, modelSRpSpanish) ### Spansh SRP improves model fit :-)
  
  modelImmersion <- glmer(error ~ session*scale(Immersion) +
                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelImmersion)
  anova(modelsess, modelImmersion) ### Immersion improves model fit :-)
  
  modelSpanishAoA <- glmer(error ~ session*scale(SpanishAoA) +
                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelSpanishAoA)
  anova(modelsess, modelSpanishAoA) ### Spanish AoA does not improve model fit
  
  #modelAge <- glmer(error ~ session*scale(Age) +
  #                    (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  #summary(modelAge)
  #anova(modelsess, modelAge) ### Age does NOT improve model fit
  
  #modelGender <- glmer(error ~ session*Gender +
  #                       (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  #summary(modelGender)
  #anova(modelsess, modelGender) ### Age improves model fit
  
  modelDoor <- glmer(error ~ session*DoorsScore +
                       (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelDoor)
  anova(modelsess, modelDoor) ### Memory capacity does NOT improve model fit
  
  modelT2success <- glmer(error ~ session*scale(T2perf) +
                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelT2success)
  anova(modelsess, modelT2success) ### T2 performance does improve model fit
  
  ### full model with all significant predictors in it 
  modelfull <- glmer(error ~ session*scale(Immersion) +
                       session*scale(T2_T3_Spanish) +
                       session*scale(T2_T3_German) +
                       session*scale(SRP_Spanish_T2_T3) +
                       session*scale(Mot_T2_T3_avg) +
                       session*scale(T2perf) +
                      # session*scale(T2_T3_Spanish)*scale(T2perf) +
                       #session*scale(T2_T3_Spanish)*scale(Mot_T2_T3_avg) +
                       (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
  summary(modelfull)
  
  
  # plotting the significant interactions from the final model only 
  
  # Spanish frequency of use
  interact_plot(modelfull, pred = T2_T3_Spanish, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
  e <- effect("session*scale(T2_T3_Spanish)",modelfull)
  plot(e)
  # Spanish proficiency self-rated
  interact_plot(modelfull, pred = SRP_Spanish_T2_T3, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
  e <- effect("session*scale(SRP_Spanish_T2_T3)",modelfull)
  plot(e)
  # T2 performance 
  interact_plot(modelfull, pred = T2perf, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
  e <- effect("session*scale(T2perf)",modelfull)
  plot(e)
}

#logit2prob <- function(logit){
#  odds <- exp(logit)
#  prob <- odds / (1 + odds)
#  return(prob)
#}
#df$predlm <- logit2prob(predict(modelSpanishFreq))*100

### Plotting individual interactions with session
library(effects)
library(interactions)
# Spanish frequency of use
interact_plot(modelSpanishFreq, pred = T2_T3_Spanish, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(T2_T3_Spanish)",modelSpanishFreq)
plot(e)
# Ratio of German to English use 
interact_plot(modelGermanEngl, pred = GermanEngRatio_T2_T3, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(GermanEngRatio_T2_T3)",modelGermanEngl)
plot(e)
# English frequency of use 
interact_plot(modelEnglFreq, pred = T2_T3_English, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(T2_T3_English)",modelEnglFreq)
plot(e)
# German frequency of use 
interact_plot(modelGerFreq, pred = T2_T3_German, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(T2_T3_German)",modelGerFreq)
plot(e)
# Motivation Spanish
interact_plot(modelMotivation, pred = Mot_T2_T3_avg, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(Mot_T2_T3_avg)",modelMotivation)
plot(e)
# Spanish proficiency self-rated
interact_plot(modelSRpSpanish, pred = SRP_Spanish_T2_T3, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(SRP_Spanish_T2_T3)",modelSRpSpanish)
plot(e)
# Continued immersion in Spanish culture after return to Germany
interact_plot(modelImmersion, pred = Immersion, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(Immersion)",modelImmersion)
plot(e)
# Spanish AoA
interact_plot(modelSpanishAoA, pred = SpanishAoA, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(SpanishAoA)",modelSpanishAoA)
plot(e)
# Age
interact_plot(modelAge, pred = Age, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(Age)",modelAge)
plot(e)
# gender
cat_plot(modelGender, pred = Gender, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*Gender",modelGender)
plot(e)
# Doors performance == memory capacity
interact_plot(modelDoor, pred = DoorsScore, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
e <- effect("session*scale(DoorsScore)",modelDoor)
plot(e)

# plotting aggregated data 
aggdf <- ddply(df, .(session, ppn, GermanEngRatio_T2_T3, T2_T3_Spanish), 
                           plyr::summarise,
                           mean = mean(Ratio),
                           sem = sd(Ratio)/sqrt(length(Ratio)))

ggplot(aes(x=scale(T2_T3_Spanish), y=(mean/(1-mean)), color= session), data = aggdf) +
  geom_point() +
 # geom_text(data = aggdf, aes(label = ppn)) +
 geom_smooth(method = "lm")

# plotting with raw data
ggplot(df, aes(x = T2_T3_Spanish, y= Ratio/100, color = session)) + 
  geom_point() +
  geom_smooth(method = "glm",method.args=list(family = "binomial"))


#### Item level predictors ####

#### Regression hypothesis  ####
# check for whether the words learned between T1 and T2, are also the words that were forgotten more often

# first load T1 data
T1 <- read_excel("T1_PicNaming_DataCoded.xlsx")
T1$img <- gsub(".png", "", T1$imgFilename)
T1$ppn <- as.factor(T1$ppn)
T1[T1=='NA'] <- NA
# subsetting to codede T1 data
T1[is.na(T1$error) == 0,]-> T1sub
df$img <- gsub(".png", "", df$imgFilename)
df_sub <- df[df$ppn %in% T1sub$ppn,]
df_sub$ppn <- droplevels(df_sub$ppn)
T1sub <- T1sub[T1sub$ppn %in% df_sub$ppn,]
T1sub$ppn <- droplevels(T1sub$ppn)
A <- unique(df_sub$ppn)

#df_sub$error_t1 <- NA
#for (i in 1:length(A)){
#  pNumber <- A[i]
#  for (k in 1:nrow(df_sub[df_sub$ppn == pNumber,])){
#    num <- which(T1sub[T1sub$ppn == pNumber,]$img == df_sub[df_sub$ppn == pNumber,]$img[k])
#    if (length(num)==0){
#      df_sub[df_sub$ppn == pNumber,]$error_t1[k] <- NA
#    }
#    else if (length(num)==1) {
#      df_sub[df_sub$ppn == pNumber,]$error_t1[k] <- T1sub[T1sub$ppn == pNumber,]$error[num]}
#  }
#  print(pNumber)
#}

# alternative: merge
df_sub2 <- merge(df_sub, T1sub, by = c("ppn", "img"))

# change all 999 errors to full errors
for (i in 1:nrow(df_sub2)){
  if (is.na(df_sub2$error.y[i])==0 & df_sub2$error.y[i] == 999){
    df_sub2$error.y[i] <- 1
  }
}

# change error coding to binary
df_sub2[df_sub2$error.x==100,]$error.x <- 1

## add a column that indexes whether the item was learned at T2
df_sub2$learned_T1_T2 <- "nochange"
for (i in 1:nrow(df_sub2)){
  if (is.na(df_sub2$error.y[i])==0 & df_sub2$error.y[i] == 1 & df_sub2$error.x[i] == 0){
    df_sub2$learned_T1_T2[i] <- "learned"
  } else if (is.na(df_sub2$error.y[i])==0 & df_sub2$error.y[i] == 0 & df_sub2$error.x[i] == 1) {
    df_sub2$learned_T1_T2[i] <- "forgotten"
  #} else if (is.na(df_sub2$error.y[i])==0 & df_sub2$error.y[i] == 0 & df_sub2$error.x[i] == 0) {
  #  df_sub2$learned_T1_T2[i] <- "remainedknown"
  #} else if (is.na(df_sub2$error.y[i])==0 & df_sub2$error.y[i] == 1 & df_sub2$error.x[i] == 1) {
  #  df_sub2$learned_T1_T2[i] <- "remainedunknown"
  #}
}}

# transfer all T2 values to their respective T3 values
df_sub3 <- df_sub2[order(df_sub2$img, df_sub2$session),]
df_sub3[df_sub3$session==3,]$learned_T1_T2 <- df_sub3[df_sub3$session==2,]$learned_T1_T2
df_sub2 <- df_sub3 

df_sub2$learned_T1_T2 <- as.factor(df_sub2$learned_T1_T2)
library(MASS)
df_sub2$learned_T1_T2 <- relevel(df_sub2$learned_T1_T2, ref="learned")
#c<-contr.treatment(4)
#my.coding<-matrix(rep(1/4, 12), ncol=3)
#my.simple<-c-my.coding
#contrasts(df_sub2$learned_T1_T2) <-my.simple
c<-contr.treatment(3)
my.coding<-matrix(rep(1/3, 6), ncol=2)
my.simple<-c-my.coding
contrasts(df_sub2$learned_T1_T2) <-my.simple

# modelling
df_sub2$error_t1N <- as.numeric(df_sub2$error.y)
df_sub2$error_t1 <- as.factor(df_sub2$error.y)
contrasts(df_sub2$error_t1) <- c(-0.5, 0.5)

# for binary error coding
modelerror <- glmer(error.x ~ session*error_t1N + 
        (1|ppn) + (1|img), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df_sub2)
summary(modelerror)

interact_plot(modelerror, pred = error_t1N, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df_sub2) 
e <- effect("session*error_t1",modelerror)
plot(e)
## words that were not know at T1, are forgotten less than words that were already known at T1

modelerror <- glmer(error.x ~ session*learned_T1_T2 + 
                      (1|ppn) + (1|img), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df_sub2)
summary(modelerror)
e <- effect("session*learned_T1_T2",modelerror)
plot(e)#ggplot(Mcombined_sub, aes(x = error_t1N, y= error, color = session)) + 
#  geom_point() +
#  geom_smooth(method = "glm",method.args=list(family = "binomial"))

# for fine-grained coding 
modelerror <- glmer(cbind(Corr, Incorr) ~ session*error_t1 + 
                      (1|ppn) + (1|img), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df_sub)
summary(modelerror)
interact_plot(modelerror, pred = error_t1N, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df_sub) 
e <- effect("session*error_t1",modelerror)
plot(e)

### Cognate vs. non-cognates  
# compare forgetting rates for cognates vs non-cognates via glm with cognate as fixed factor 
# plot the difference
setwd("//ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
lenwords <- read.delim("FullListWords_SpanishNaming.txt")
lenwords$img <- gsub(".png", "", lenwords$Picture)

dfcog <- merge(df_sub, lenwords)
dfcog[dfcog$error==100,]$error <- 1

dfcog$Cognate.[dfcog$Cognate.==2] <- 1
dfcog$Cognate. <- as.factor(dfcog$Cognate.)
#contrasts(dfcog$Cognate.) <- c(-0.5, 0.5)
dfcog$CogN <- (as.numeric(as.character(dfcog$Cognate.))-0.5)  # cognates are 0.5, non-cognates are -0.5

modelcog <- glmer(error ~ session*CogN + 
                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = dfcog)
summary(modelcog)

interact_plot(modelcog, pred = CogN, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = dfcog) 
# non-cognates tend to be forgotten more than cognates 

e <- allEffects(modelcog)
plot(e)
#x <- as.data.frame(as.data.frame(e))
#ggplot(x, aes(session.CogN.CogN, session.CogN.fit, color=session.CogN.session)) + 
#  geom_point() + 
#  geom_line() +
#  geom_errorbar(aes(ymin=session.CogN.fit-session.CogN.se , ymax= session.CogN.fit+ session.CogN.se ), width=0.4) + 
#  theme_bw(base_size=12)

ddply(dfcog, .(Cognate., session, ppn), 
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

#aggregatedCog_mean$Cognate. <- as.numeric(aggregatedCog_mean$Cognate.)
#aggregatedCog_mean$session <- as.numeric(aggregatedCog_mean$session)
#ggplot(aggregatedCog_mean, aes(x=session, y=condition_mean, color= Cognate.Lab)) +
#  geom_line(aes(x=session, y=condition_mean, color = Cognate.Lab)) +
#  geom_point(aes(x=session, y=condition_mean, color= Cognate.Lab)) +
#  geom_errorbar(aes(ymin=condition_mean-condition_sem,
#                    ymax=condition_mean+condition_sem,
#                    color = Cognate.Lab),
#                width = 0.5) +
#  xlab("Cognate status") +
#  #scale_x_discrete(labels=c("Non-cognate", "Cognate"), breaks = 1:2, expand = c(0.1,0.1)) +
#  ylab("% correct") +
#  ylim(c(0,100)) + 
#  theme_bw()

## Frequency (taking German frequency as base)
# add frequency information 
setwd("//ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
frequencies <- read.delim("Germanfrequencies.txt")
colnames(frequencies) <- c("German", "German2", "img", "Spanish", "SubLog", "SubMln")

dfcog <- merge(Mcombined, frequencies, by = "img")
dfcog[dfcog$error==100,]$error <- 1

modelfreq <- glmer(error ~ session*SubLog + 
                    (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = dfcog)
summary(modelfreq)

interact_plot(modelfreq, pred = SubLog, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = dfcog) 
e <- allEffects(modelfreq)
plot(e)

dfcog$Freq <- NA
dfcog[dfcog$SubLog<2.3475,]$Freq <- "Low"
dfcog[dfcog$SubLog>2.3475,]$Freq <- "High"

ddply(dfcog, .(Freq, session, ppn), 
      summarise, N=length(error), 
      mean   = mean(error, na.rm = TRUE), 
      sem = sd(error, na.rm = TRUE)/sqrt(N)) -> aggregatedCog


aggregatedCog_mean <- ddply(aggregatedCog, .(Freq, session), 
                                 summarise,
                                condition_mean = mean(mean,na.rm = T),
                                 condition_sem = sd(mean,na.rm = T)/sqrt(length(mean[!is.na(mean)])))
aggregatedCog_mean$condition_mean <- aggregatedCog_mean$condition_mean*100
aggregatedCog_mean$condition_sem <- aggregatedCog_mean$condition_sem*100

ggplot(aggregatedCog_mean, aes(x=Freq, y=condition_mean, fill= session)) +
  geom_bar(stat="identity", position =position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  xlab("Frequency") +
  scale_fill_manual(values=c("grey70", "grey25")) +
  ylab("% incorrect") +
  theme_bw()

## add individual difference stuff to the dataframe 
individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

## add predictors to the full dataframe 
colnames(individualdiff)[1] <- "ppn"
dfcog2 <- merge(dfcog, individualdiff, by = "ppn")
dfcog2$GermanEngRatio_T2_T3 <- dfcog2$T2_T3_English/df$T2_T3_German

modelfreqint <- glmer(error ~ session*scale(SubLog)*scale(T2perf) + 
                     (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = dfcog2)
summary(modelfreqint)
