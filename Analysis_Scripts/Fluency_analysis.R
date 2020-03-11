### Fluency task performance T2 an T3 ####

# load packages
library(readxl)
library(ggplot2)
library(plyr)

# load data 
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T1data <- read_excel("Fluency_all_coded.xlsx", guess_max = 1048576, sheet = "T1_fluency_all")
T2data <- read_excel("Fluency_all_coded.xlsx", guess_max = 1048576, sheet = "T2_fluency_all")
T3data <- read_excel("Fluency_all_coded.xlsx", guess_max = 1048576, sheet = "T3_fluency_all")
data <- rbind(T2data, T3data)
data_all <- rbind(T1data, T2data, T3data)

ppn <- read.delim("PPN_final.txt", header = F)
ppnT1 <- read.delim("PPN_final_T1.txt", header = F)

# subset data
datasub <- data[which(data$PP %in% ppn$V1),]
datasuball <- data_all[which(data_all$PP %in% ppnT1$V1),]

# calculate sum of productions per participant per category 
sums <- as.data.frame(tapply(datasub$Word, list(datasub$PP, datasub$`Category/Letter`), sum))
sumsall <- as.data.frame(tapply(datasuball$Word, list(datasuball$PP, datasuball$`Category/Letter`), sum))

# find any NA values in the sums dataframe and figure out why data is missing 
apply(is.na(sums), 2, which)
apply(is.na(sumsall), 2, which)

# is there a minimum amount of words participants must have produced? 
apply(sums,2,min, na.rm = T)
apply(sums,2,max, na.rm = T)
apply(sums,2,mean, na.rm = T)

# plot stuff
#datasub = datasuball
means <- ddply(datasub, .(PP, TN, Language, `Category/Letter`), 
                        plyr::summarise,
                        sum = sum(Word))
meansagg <- ddply(means, .(TN, Language, `Category/Letter`),
                  plyr::summarise,
                  mean = mean(sum),
                  sem = sd(sum)/sqrt(length(sum)))

ggplot(meansagg, aes(y = mean, x = `Category/Letter`, fill = Language)) +
  geom_bar(aes(y = mean, fill = Language), stat = "identity") +
  geom_errorbar(aes(ymin=mean-sem,
                    ymax=mean+sem),
                width = 0.5) +
  facet_wrap(~TN, scales = "free_x") +
  ylab("Mean words produced") +
  scale_fill_manual("Language", values = c("grey30", "grey80"), labels=c("German","English")) +
  theme_bw()

ggplot(means, aes(y = sum, x = `Category/Letter`, fill = Language)) +
  geom_violin(aes(y = sum, fill = Language)) +
  facet_wrap(~TN, scales = "free_x") +
  ylab("Mean words produced") +
  stat_summary(fun.y=mean, geom="point", size=2, color = "white") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  #geom_text(aes(label=PP)) +
  #stat_summary(fun.data=mean_sdl, 
  #             geom="pointrange", color="white") +
  scale_fill_manual("Language", values = c("grey30", "grey80"), labels=c("German","English")) +
  theme_bw()


# leave fluency 5 out for all and calculate difference score per language
#datasubclean <- datasub[-which(datasub$Trial==5),]
scores <- as.data.frame(tapply(datasub$Word, list(datasub$PP, datasub$`Category/Letter`), sum))
scores$Ppn <- rownames(scores)

scores$Diff_T2_T3_letter_German <- scores$B-scores$M
scores$Diff_T2_T3_letter_English <- scores$D-scores$A
scores$Diff_T2_T3_category_German_avg <- (scores$Büroartikel+scores$Sport)/2 - (scores$Badartikel+scores$Fortbewegungsmittel)/2
scores$Diff_T2_T3_category_German_4 <- scores$Sport - scores$Badartikel
scores$Diff_T2_T3_category_English_avg <- (scores$`Body parts`+scores$`electronic devices`)/2 - (scores$Kleidung+scores$Obst)/2
scores$Diff_T2_T3_category_English_1 <- scores$`Body parts` - scores$Kleidung
scores$Diff_T2_T3_category_English_2 <- scores$`electronic devices` - scores$Obst 

# save this to file
write.table(scores[,c(13:20)], "Fluency_scores_T2_T3.txt", sep = "\t", quote = F, row.names = F)

## correlations between T2 and T3 fluency data 
library(ggcorrplot)
corrmat <- scores[, -c(13)]
corrdetail <- round(cor(corrmat, use = "complete.obs"), 1)
ggcorrplot(corrdetail, method = "circle") +
  theme(axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10))
