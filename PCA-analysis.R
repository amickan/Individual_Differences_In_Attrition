##### Motivation questionnaire script 

# load in lime survey questionnaire T1 
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T1lime <- read.csv("T1_results.csv", stringsAsFactors = F)
T2lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T2_results_new.csv", stringsAsFactors = F)
T3lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T3_results_new.csv", stringsAsFactors = F)

# participants
A <- unique(T2lime[T2lime$Letzte.Seite==6,]$ZugangsschlÃ.ssel)
T3lime <- T3lime[T3lime$Letzte.Seite...>4,]


# seperate out the motivation questionnaire data only 
moti_T1 <- T1lime[,c(5,85:121)]
moti_T2 <- T2lime[,c(5,92:128)]
moti_T3 <- T3lime[,c(5,37:73)]

colnames(moti_T1) <- c("token", 
                    "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12",
                    "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22", "M23", 
                    "M24", "M25", "M26", "M27", "M28", "M29", "M30", "M31", "M32", "M33", "M34",
                    "M35", "M36", "M37")

colnames(moti_T2) <- c("token", 
                       "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12",
                       "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22", "M23", 
                       "M24", "M25", "M26", "M27", "M28", "M29", "M30", "M31", "M32", "M33", "M34",
                       "M35", "M36", "M37")

colnames(moti_T3) <- c("token", 
                       "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12",
                       "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22", "M23", 
                       "M24", "M25", "M26", "M27", "M28", "M29", "M30", "M31", "M32", "M33", "M34",
                       "M35", "M36", "M37")

# load in the motivation questionnaire coding 
coding <- read.delim("MotivationTestCategoryCoding.txt")

# # replace with numbers 
# for (i in 1:nrow(moti_T1)){
#   for (k in 1:ncol(moti_T1)){
#     if (moti_T1[i,k] == "absolut einverstanden"){
#       moti_T1[i,k] <- "7"} 
#     else if (moti_T1[i,k] == "..."){
#       moti_T1[i,k] <- "6" }
#     else if (moti_T1[i,k] == "eher einverstanden"){
#       moti_T1[i,k] <- "5" }
#     else if (moti_T1[i,k] == ".."){
#       moti_T1[i,k] <- "4" }
#     else if (moti_T1[i,k] == "eher nicht einverstanden"){
#       moti_T1[i,k] <- "3" }
#     else if (moti_T1[i,k] == "."){
#       moti_T1[i,k] <- "2" }
#     else if (moti_T1[i,k] == "Ã¼berhaupt nicht einverstanden"){
#       moti_T1[i,k] <- "1" }
#   }
# }        

moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = "absolut einverstanden", replacement = "7", fixed = TRUE))
moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = "...", replacement = "6", fixed = TRUE))
moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = "eher einverstanden", replacement = "5", fixed = TRUE))
moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = "..", replacement = "4", fixed = TRUE))
moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = "eher nicht einverstanden", replacement = "3", fixed = TRUE))
moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = ".", replacement = "2", fixed = TRUE))
moti_T1 <- as.data.frame(lapply(moti_T1, gsub, pattern = "Ã¼berhaupt nicht einverstanden", replacement = "1", fixed = TRUE))

moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = "absolut einverstanden", replacement = "7", fixed = TRUE))
moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = "...", replacement = "6", fixed = TRUE))
moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = "eher einverstanden", replacement = "5", fixed = TRUE))
moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = "..", replacement = "4", fixed = TRUE))
moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = "eher nicht einverstanden", replacement = "3", fixed = TRUE))
moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = ".", replacement = "2", fixed = TRUE))
moti_T2 <- as.data.frame(lapply(moti_T2, gsub, pattern = "Ã¼berhaupt nicht einverstanden", replacement = "1", fixed = TRUE))

moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = "absolut einverstanden", replacement = "7", fixed = TRUE))
moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = "...", replacement = "6", fixed = TRUE))
moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = "eher einverstanden", replacement = "5", fixed = TRUE))
moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = "..", replacement = "4", fixed = TRUE))
moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = "eher nicht einverstanden", replacement = "3", fixed = TRUE))
moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = ".", replacement = "2", fixed = TRUE))
moti_T3 <- as.data.frame(lapply(moti_T3, gsub, pattern = "Ã¼berhaupt nicht einverstanden", replacement = "1", fixed = TRUE))

 
# turning some of these around accroding to the coding scheme 
row.names(moti_T1) <- moti_T1[,1] 
row.names(moti_T2) <- moti_T2[,1] 
row.names(moti_T3) <- moti_T3[,1] 

moti_T1_2 <- moti_T1[,-c(1,34:38)] 
moti_T2_2 <- moti_T2[,-c(1,34:38)] 
moti_T3_2 <- moti_T3[,-c(1,34:38)] 

instanceconvert <- colnames(moti_T1_2)
for (i in instanceconvert){
  moti_T1_2[[i]] <- as.numeric(as.character(moti_T1_2[[i]]))
} 

instanceconvert <- colnames(moti_T2_2)
for (i in instanceconvert){
  moti_T2_2[[i]] <- as.numeric(as.character(moti_T2_2[[i]]))
} 

instanceconvert <- colnames(moti_T3_2)
for (i in instanceconvert){
  moti_T3_2[[i]] <- as.numeric(as.character(moti_T3_2[[i]]))
} 


row.names(moti_T1_2) <- row.names(moti_T1)
row.names(moti_T2_2) <- row.names(moti_T2)
row.names(moti_T3_2) <- row.names(moti_T3)

# leaving out NAs
#moti_T1_2 <- moti_T1_2[-c(51,131,153),]
#moti_T2_2 <- moti_T2_2[-c(51,131,153),]
#moti_T3_2 <- moti_T3_2[-c(51,131,153),]
coding <- coding[c(1:32),]
nums <- c(7,6,5,4,3,2,1)
for (i in 1:nrow(coding)){
  if (coding$Inverted.[i] == 1){
    for (k in 1:nrow(moti_T1_2)){
      moti_T1_2[k,i] <- nums[moti_T1_2[k,i]]
    }
    }
}

for (i in 1:nrow(coding)){
  if (coding$Inverted.[i] == 1){
    for (k in 1:nrow(moti_T2_2)){
      moti_T2_2[k,i] <- nums[moti_T2_2[k,i]]
    }
  }
}

for (i in 1:nrow(coding)){
  if (coding$Inverted.[i] == 1){
    for (k in 1:nrow(moti_T3_2)){
      moti_T3_2[k,i] <- nums[moti_T3_2[k,i]]
    }
  }
}

# do a PCA on the motivation questionnaire data 
#pca <- prcomp(t(moti3[, 1:32]), center = TRUE, scale = TRUE)
#summary(pca)

#library(factoextra)

#get_pca(pca, element = c("var", "ind"))
#var <- get_pca_var(pca)
#head(var$contrib)
#ind <- get_pca_ind(pca)
#comps <- ind$contrib[,1:8] # only the first 8 components, which toether explain 80% of the variance 
#comp1 <- round(sort(comps[,1], decreasing = T),2)
#comp2 <- round(sort(comps[,2], decreasing = T),2)
#comp3 <- round(sort(comps[,3], decreasing = T),2)
#comp4 <- round(sort(comps[,4], decreasing = T),2)
#comp5 <- round(sort(comps[,5], decreasing = T),2)
#comp6 <- round(sort(comps[,6], decreasing = T),2)
#comp7 <- round(sort(comps[,7], decreasing = T),2)
#comp8 <- round(sort(comps[,8], decreasing = T),2)
## put all rownmaes of the comps into one dataframe for easier comparison
#pcaloadings <- matrix(NA, 32,8)
#pcaloadings[,1] <- names(comp1)
#pcaloadings[,2] <- names(comp2)
#pcaloadings[,3] <- names(comp3)
#pcaloadings[,4] <- names(comp4)
#pcaloadings[,5] <- names(comp5)
#pcaloadings[,6] <- names(comp6)
#pcaloadings[,7] <- names(comp7)
#pcaloadings[,8] <- names(comp8)

# # set all fields with an association below 2 to NA 
# pcaloadings[-which(round(sort(comps[,1], decreasing = T),2) >= 2),1] <- NA
# pcaloadings[-which(round(sort(comps[,2], decreasing = T),2) >= 2),2] <- NA
# pcaloadings[-which(round(sort(comps[,3], decreasing = T),2) >= 2),3] <- NA
# pcaloadings[-which(round(sort(comps[,4], decreasing = T),2) >= 2),4] <- NA
# pcaloadings[-which(round(sort(comps[,5], decreasing = T),2) >= 2),5] <- NA
# pcaloadings[-which(round(sort(comps[,6], decreasing = T),2) >= 2),6] <- NA
# pcaloadings[-which(round(sort(comps[,7], decreasing = T),2) >= 2),7] <- NA
# pcaloadings[-which(round(sort(comps[,8], decreasing = T),2) >= 2),8] <- NA
# 
# # load in the original questions and put them in the pcaloadings dataframe 
# questions <- read.delim("motivationquestions.txt")
# 
# for (i in 1:nrow(pcaloadings)){
#   for (k in 1:ncol(pcaloadings)){
#     if (is.na(pcaloadings[i,k]) == 0){
#     num <- as.numeric(gsub("M([0-9]+)", "\\1", pcaloadings[i,k])) 
#     pcaloadings[i,k] <- as.character(questions$Question[num])}
#   }
# }
# pcaloadingsdf <- as.data.frame(pcaloadings)
# 
# setwd("U:/PhD/EXPERIMENT 4")
# write.table(pcaloadingsdf, "pcaloadingsT1_text.txt", quote = F, row.names = F, sep = "\t")
# 
# # correlate mean performance from subjects in component 1 with mean performance if we were to take all answers into account, 
# # and seperate for the predefined categories 
# subjmeans <- rowMeans(moti3[1:32])
# c1red <- comp1[1:15]
# comp1means <- rowMeans(moti3[,which(colnames(moti3) %in% names(c1red))])
# library(psych)
# out <- corr.test(subjmeans, comp1means)
# # correlation of r = .6

# calculate scores for the 5 subparts of the motivation test and see how these scores correlate with the overall score
setwd("U:/PhD/EXPERIMENT 4")
questions <- read.delim("motivationquestions.txt")
interest <- which(questions$Type == "interest in FL")
attitude <- which(questions$Type == "attitude towards Spanish")
instrumental <- which(questions$Type == "instrumental")
integrative <- which(questions$Type == "integrative")
anxiety <- which(questions$Type == "anxiety")

minterestT1 <- rowMeans(moti_T1_2[,interest])
mattitudeT1 <- rowMeans(moti_T1_2[,attitude])
minstrumentalT1 <- rowMeans(moti_T1_2[,instrumental])
mintegrativeT1 <- rowMeans(moti_T1_2[,integrative])
manxietyT1 <- rowMeans(moti_T1_2[,anxiety])

minterestT2 <- rowMeans(moti_T2_2[,interest])
mattitudeT2 <- rowMeans(moti_T2_2[,attitude])
minstrumentalT2 <- rowMeans(moti_T2_2[,instrumental])
mintegrativeT2 <- rowMeans(moti_T2_2[,integrative])
manxietyT2 <- rowMeans(moti_T2_2[,anxiety])

minterestT3 <- rowMeans(moti_T3_2[,interest])
mattitudeT3 <- rowMeans(moti_T3_2[,attitude])
minstrumentalT3 <- rowMeans(moti_T3_2[,instrumental])
mintegrativeT3 <- rowMeans(moti_T3_2[,integrative])
manxietyT3 <- rowMeans(moti_T3_2[,anxiety])

# add all of those to the individual differences dataframe 
setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

for (i in 1:nrow(individualdiff)){
  num <- which(as.numeric(row.names(moti_T1_2)) == individualdiff$ppn[i])
  num2 <- which(as.numeric(row.names(moti_T2_2)) == individualdiff$ppn[i])
  num3 <- which(as.numeric(row.names(moti_T3_2)) == individualdiff$ppn[i])
  
  if (length(num) == 1) {
    individualdiff$Mot_T1_interest[i] <- minterestT1[num]
    individualdiff$Mot_T1_attitude[i] <- mattitudeT1[num]
    individualdiff$Mot_T1_instrumental[i] <- minstrumentalT1[num]
    individualdiff$Mot_T1_integrative[i] <- mintegrativeT1[num]
    individualdiff$Mot_T1_anxiety[i] <- manxietyT1[num]
    individualdiff$Mot_T1_avg[i] <- mean(c(minterestT1[num], mattitudeT1[num], minstrumentalT1[num], mintegrativeT1[num],manxietyT1[num]))
  } else {
    individualdiff$Mot_T1_interest[i] <- NA
    individualdiff$Mot_T1_attitude[i] <- NA
    individualdiff$Mot_T1_instrumental[i] <- NA
    individualdiff$Mot_T1_integrative[i] <- NA
    individualdiff$Mot_T1_anxiety[i] <- NA
    individualdiff$Mot_T1_avg[i] <- NA
  }
  
  if (length(num2) == 1) {
    individualdiff$Mot_T2_interest[i] <- minterestT2[num2]
    individualdiff$Mot_T2_attitude[i] <- mattitudeT2[num2]
    individualdiff$Mot_T2_instrumental[i] <- minstrumentalT2[num2]
    individualdiff$Mot_T2_integrative[i] <- mintegrativeT2[num2]
    individualdiff$Mot_T2_anxiety[i] <- manxietyT2[num2]
    individualdiff$Mot_T2_avg[i] <- mean(c(minterestT2[num2], mattitudeT2[num2], minstrumentalT2[num2], mintegrativeT2[num2],manxietyT2[num2]))
  } else {
    individualdiff$Mot_T2_interest[i] <- NA
    individualdiff$Mot_T2_attitude[i] <- NA
    individualdiff$Mot_T2_instrumental[i] <- NA
    individualdiff$Mot_T2_integrative[i] <- NA
    individualdiff$Mot_T2_anxiety[i] <- NA
    individualdiff$Mot_T2_avg[i] <- NA
  }
  
  if (length(num3) == 1) {
    individualdiff$Mot_T3_interest[i] <- minterestT3[num3]
    individualdiff$Mot_T3_attitude[i] <- mattitudeT3[num3]
    individualdiff$Mot_T3_instrumental[i] <- minstrumentalT3[num3]
    individualdiff$Mot_T3_integrative[i] <- mintegrativeT3[num3]
    individualdiff$Mot_T3_anxiety[i] <- manxietyT3[num3]
    individualdiff$Mot_T3_avg[i] <- mean(c(minterestT3[num3], mattitudeT3[num3], minstrumentalT3[num3], mintegrativeT3[num3],manxietyT3[num3]))
  } else {
    individualdiff$Mot_T3_interest[i] <- NA
    individualdiff$Mot_T3_attitude[i] <- NA
    individualdiff$Mot_T3_instrumental[i] <- NA
    individualdiff$Mot_T3_integrative[i] <- NA
    individualdiff$Mot_T3_anxiety[i] <- NA
    individualdiff$Mot_T3_avg[i] <- NA
  }
  
}

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
write.table(individualdiff, "T1_T2_T3_lime_clean.txt", sep = "\t", quote = F, row.names = F)

dat <- as.data.frame(c(individualdiff$Mot_T1_attitude, individualdiff$Mot_T2_attitude, individualdiff$Mot_T3_attitude))
dat[,2] <- rep(individualdiff$ppn, 3)
dat[,3] <- rep(c(1:3), each = 146)
colnames(dat) <- c("mean", "ppn", "session")
as.factor(dat$ppn) -> dat$ppn
ggplot(dat, aes(x=session, y=mean, group = ppn)) +
  geom_line(aes(color = ppn)) +
  geom_point(aes(color = ppn))

cor(individualdiff$Mot_T1_avg, individualdiff$Mot_T3_avg, use = "complete.obs")
rcorr(individualdiff$Mot_T1_avg, individualdiff$Mot_T3_avg)

#corr.test(subjmeans, manxiety)
# interest: .65
# attitude: .72
# instrumental: .59
# integrative: .61
# anxiety: .18

### some more diagnostic plots from the pca analysis 
# fviz_eig(pca)
# fviz_pca_ind(pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# screeplot(pca, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
# abline(h = 1, col="red", lty=5)
# legend("topright", legend=c("Eigen»value = 1"),
#        col=c("red"), lty=5, cex=0.6)
# 
# cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
# plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
# #abline(v = 7, col="blue", lty=5)
# #abline(h = 0.56575, col="blue", lty=5)
# #legend("topleft", legend=c("Cut-off @ PC7"),
# #       col=c("blue"), lty=5, cex=0.6)