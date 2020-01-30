#### Plot learning progress from T1 to T2
library(readxl)
library(ggplot2)

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T1 <- read_excel("T1_PicNaming_DataCoded.xlsx")
T2 <- read_excel("T2_PicNaming_DataCoded.xlsx")

# subset to coded people 
T1[T1=='NA'] <- NA
T2[T2=='NA'] <- NA
T1[is.na(T1$error) == 0,]-> T1sub
T2[is.na(T2$error) == 0,]-> T2sub

# set numeric 
T1sub$error <- as.numeric(T1sub$error)
T1sub$phoncorr <- as.numeric(T1sub$phoncorr)
T1sub$phonincorr <- as.numeric(T1sub$phonincorr)
for (i in 1:nrow(T1sub)){
  if (is.na(T1sub$error[i]) == 0 && T1sub$error[i] == 999){
    T1sub$error[i] <- T1sub$phonincorr[i]/(T1sub$phonincorr[i]+T1sub$phoncorr[i])
  }
}

# calculate performance (error in %) at both time points and plot a historgram
hist(tapply(T1sub$error, list(T1sub$ppn), mean, na.rm = T))

# set numeric 
T2sub$error <- as.numeric(T2sub$error)
T2sub$phoncorr <- as.numeric(T2sub$phoncorr)
T2sub$phonincorr <- as.numeric(T2sub$phonincorr)
for (i in 1:nrow(T2sub)){
  if (is.na(T2sub$error[i]) == 0 && T2sub$error[i] == 999){
    T2sub$error[i] <- T2sub$phonincorr[i]/(T2sub$phonincorr[i]+T2sub$phoncorr[i])
  }
}

# calculate performance (error in %) at both time points and plot a historgram
hist(tapply(T2sub$error, list(T2sub$ppn), mean, na.rm = T))


### subset T2 to those that have been coded for T1
T1sub$ppn <- as.factor(T1sub$ppn)
which(table(T1sub$ppn) < 144)
T2sub$ppn <- as.factor(T2sub$ppn)
nums <- which(table(T2sub$ppn) < 144)
nums <- names(nums)
T2sub <- T2sub[-which(T2sub$ppn == nums),]

T2sub2 <- T2sub[T2sub$ppn %in% T1sub$ppn,]
T1sub2 <- T1sub[T1sub$ppn %in% T2sub2$ppn,]
T2sub2$ppn <- droplevels(T2sub2$ppn)
T1sub2$ppn <- droplevels(T1sub2$ppn)
T1mean <- as.data.frame(tapply(T1sub2$error, list(T1sub2$ppn), mean, na.rm = T))
T2mean <- as.data.frame(tapply(T2sub2$error, list(T2sub2$ppn), mean, na.rm = T))
colnames(T1mean) <- c("mean")
colnames(T2mean) <- c("mean")

combined <- rbind(T1mean, T2mean) 
combined[,2] <- rep(c(1,2), each = nrow(T1mean))
combined[,3] <- rep(row.names(T1mean), times = 2)
colnames(combined) <- c("mean", "session", "pp")
combined$mean <- combined$mean*100

lineplot <- ggplot(combined, aes(y = mean, x = session, group = pp))
lineplot + geom_point(color = "darkgrey") +
  geom_line(color = "darkgrey") +
  geom_text(aes(label=pp)) +
  coord_cartesian(ylim=c(0,100)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_continuous(labels=c("T1", "T2"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Error rates") +
  #scale_color_manual(guide=F, "Language Condition", values=c("dodgerblue4","firebrick"),labels=c("Dutch","English")) +
  theme_bw()

## difference T1 - T2 
T1mean$diff <- T1mean$mean - T2mean$mean


#### CLEARNER: out of the words they did not know at T1, how many did they learn? 

T2sub2$errort1 <- NA
ppn <- unique(T2sub2$ppn)
for (i in 1:length(ppn)) {
  ppnn <- ppn[i]
  for (k in 1:length(T1sub2[T1sub2$ppn == ppnn,]$imgFilename)){
    num <- which(T2sub2[T2sub2$ppn == ppnn,]$imgFilename == T1sub2[T1sub2$ppn == ppnn,]$imgFilename[k])
    if (length(num) == 1){
      T2sub2[T2sub2$ppn == ppnn,]$errort1[num] <- T1sub2[T1sub2$ppn == ppnn,]$error[k]
    }
  }
}

T1sub3 <- T1sub2[T1sub2$error == 1,]
T2sub3 <- T2sub2[T2sub2$errort1 == 1,]

hist(tapply(T2sub3$error, list(T2sub3$ppn), mean, na.rm = T))
T1meanclean <- as.data.frame(tapply(T1sub3$error, list(T1sub3$ppn), mean, na.rm = T))
colnames(T1meanclean) <- c("mean")
T2meanclean <- as.data.frame(tapply(T2sub3$error, list(T2sub3$ppn), mean, na.rm = T))
colnames(T2meanclean) <- c("mean")

combined2 <- rbind(T1meanclean, T2meanclean) 
combined2[,2] <- rep(c(1,2), each = nrow(T1meanclean))
combined2[,3] <- rep(row.names(T1meanclean), times = 2)
colnames(combined2) <- c("mean", "session", "pp")
combined2$mean <- combined2$mean*100
as.factor(combined2$session) -> combined2$session

lineplot <- ggplot(combined2, aes(y = mean, x = session, group = pp))
lineplot + geom_point(color = "darkgrey") +
  geom_line(color = "darkgrey") +
  #geom_text(data = combined2[combined2$session == 2,], aes(label=pp)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  scale_x_discrete(labels=c("T1", "T2"), breaks = c(2,3), expand = c(0.1,0.1)) +
  ylab("Error rates (only including words that were unknown at T1)") +
  #scale_color_manual(guide=F, "Language Condition", values=c("dodgerblue4","firebrick"),labels=c("Dutch","English")) +
  theme_bw()
