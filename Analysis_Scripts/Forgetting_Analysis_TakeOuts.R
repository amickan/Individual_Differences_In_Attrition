### Analysis with dichotomous error coding 
#Mcombined$errorfine <- Mcombined$error
##T2sub$errorfine <- T2sub$error
##T3sub$errorfine <- T3sub$error
#Mcombined$error <- Mcombined$errorbin
##T2sub$error <- T2sub$errorbin
##T3sub$error <- T3sub$errorbin
#
# df[df$error==100,]$error <- 1
# 
# # does average Spanish frequency of use after study abroad predict forgetting / learning rates between T2 and T3 
# 
# # test each predictor seperately and assess whether its inclusion improves model fit 
# modelsess <- glmer(error ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelsess)
# modelSpanishFreq <- glmer(error ~ session*scale(T2_T3_Spanish) +
#                             (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelSpanishFreq)
# anova(modelsess, modelSpanishFreq) ### Spanish frequency of use improves model fit  :-)
# 
# modelGermanEngl <- glmer(error ~ session*scale(GermanEngRatio_T2_T3) +
#                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelGermanEngl)
# anova(modelsess, modelGermanEngl) ### Ratio of German to English frequency of use idoes not improve model fit.
# 
# modelEnglFreq <- glmer(error ~ session*scale(T2_T3_English) +
#                          (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelEnglFreq)
# anova(modelsess, modelEnglFreq) ### English frequency of use does NOT improve model fit.
# 
# modelGerFreq <- glmer(error ~ session*scale(T2_T3_German) +
#                         (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelGerFreq)
# anova(modelsess, modelGerFreq) ### German frequency of use improves model fit.
# 
# modelMotivation <- glmer(error ~ session*scale(Mot_T2_T3_avg) +
#                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelMotivation)
# anova(modelsess, modelMotivation) ### Motivation improves model fit :-)
# 
# modelSRpSpanish <- glmer(error ~ session*scale(SRP_Spanish_T2_T3) +
#                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelSRpSpanish)
# anova(modelsess, modelSRpSpanish) ### Spansh SRP improves model fit :-)
# 
# modelImmersion <- glmer(error ~ session*scale(Immersion) +
#                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelImmersion)
# anova(modelsess, modelImmersion) ### Immersion improves model fit :-)
# 
# modelSpanishAoA <- glmer(error ~ session*scale(SpanishAoA) +
#                            (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelSpanishAoA)
# anova(modelsess, modelSpanishAoA) ### Spanish AoA does not improve model fit
# 
# #modelAge <- glmer(error ~ session*scale(Age) +
# #                    (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# #summary(modelAge)
# #anova(modelsess, modelAge) ### Age does NOT improve model fit
# 
# #modelGender <- glmer(error ~ session*Gender +
# #                       (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# #summary(modelGender)
# #anova(modelsess, modelGender) ### Age improves model fit
# 
# modelDoor <- glmer(error ~ session*DoorsScore +
#                      (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelDoor)
# anova(modelsess, modelDoor) ### Memory capacity does NOT improve model fit
# 
# modelT2success <- glmer(error ~ session*scale(T2perf) +
#                           (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelT2success)
# anova(modelsess, modelT2success) ### T2 performance does improve model fit
# 
# ### full model with all significant predictors in it 
# modelfull <- glmer(error ~ session*scale(Immersion) +
#                      session*scale(T2_T3_Spanish) +
#                      session*scale(T2_T3_German) +
#                      session*scale(SRP_Spanish_T2_T3) +
#                      session*scale(Mot_T2_T3_avg) +
#                      session*scale(T2perf) +
#                      # session*scale(T2_T3_Spanish)*scale(T2perf) +
#                      #session*scale(T2_T3_Spanish)*scale(Mot_T2_T3_avg) +
#                      (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df)
# summary(modelfull)
# 
# 
# # plotting the significant interactions from the final model only 
# 
# # Spanish frequency of use
# interact_plot(modelfull, pred = T2_T3_Spanish, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
# e <- effect("session*scale(T2_T3_Spanish)",modelfull)
# plot(e)
# # Spanish proficiency self-rated
# interact_plot(modelfull, pred = SRP_Spanish_T2_T3, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
# e <- effect("session*scale(SRP_Spanish_T2_T3)",modelfull)
# plot(e)
# # T2 performance 
# interact_plot(modelfull, pred = T2perf, modx = session, interval = TRUE, robust = TRUE,  int.width = 0.8, data = df) 
# e <- effect("session*scale(T2perf)",modelfull)
# plot(e)


#require(ggiraph)
#require(ggiraphExtra)
#ggPredict(modelSpanishFreq,se=TRUE,interactive=TRUE)

#### Use forgetting score ####
#aggdiff <- as.data.frame(aggdf[aggdf$session=="3",]$mean - aggdf[aggdf$session=="2",]$mean)
#aggdiff[,2] <- aggdf[aggdf$session=="3",]$ppn
#aggdiff[,3] <- aggdf[aggdf$session=="3",]$GermanEngRatio_T2_T3
#colnames(aggdiff) <- c("mean", "ppn", "GermanEngRatio_T2_T3")
#ggplot(aes(x=GermanEngRatio_T2_T3, y=mean), data = aggdf) +
#  geom_point() +
#  # geom_text(data = aggdf, aes(label = ppn)) +
#  geom_smooth(method = "lm")

#histogram of forgetting score - difference between T2 and T3 
#dat <- tapply(Mcombined$error, list(Mcombined$ppn, Mcombined$session), mean, na.rm = T)
#dat$diff <- dat[,2]-dat[,1]
#hist(dat$diff)

#### Model with only the words known at T2 #####
## 100 % at T2 is participant specific
## this isolates forgetting from T2 to T3 

# first adjust the phoneme correct and incorrect counts again 
# adjust existing phoneme correct and incorrect counts 
# Mcombined2$OrigLen <- NA
# Mcombined2$img <- gsub(".png", "", Mcombined2$imgFilename)
# 
# for (j in 1:nrow(Mcombined2)) {
#   pos <- which(tolower(as.character(lenwords$English)) == tolower(as.character(Mcombined2$img[j])))
#   Mcombined2$OrigLen[j] <- lenwords$TotalPhon[pos]
#   
#   if (is.na(lenwords$AltPhon[pos])==1) {}
#   else if (is.na(lenwords$AltPhon[pos])==0 && is.na(Mcombined2$response[j])==0) {
#     if (grepl("man", Mcombined2$response[j])) {
#       # cacahuete synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "mani"
#     } else if (grepl("pil", Mcombined2$response[j])) {
#       # batteria synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "pila"
#     } else if (grepl("are", Mcombined2$response[j])) {
#       # pendientes synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "aretes"
#     } else if (grepl("aro", Mcombined2$response[j])) {
#       # pendientes synonym
#       Mcombined2$OrigLen[j] <- lenwords$X[pos]
#       Mcombined2$imgFilename[j] <- "aro"
#     } else if (grepl("co", Mcombined2$response[j])) {
#       # almohada synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "cojin"
#     } else if (grepl("ana", Mcombined2$response[j])) {
#       # pinya synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "ananas"
#     } else if (grepl("cob", Mcombined2$response[j])) {
#       # manta synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "cobija"
#     } else if (grepl("cit", Mcombined2$response[j])) {
#       # limon synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "citron"
#     } else if (grepl("ori", Mcombined2$response[j])) {
#       # salchicha synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "chorizo"
#     } else if (grepl("bomb", Mcombined2$response[j])) {
#       # paja synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "bombilla"
#     } else if (grepl("can", Mcombined2$response[j])) {
#       # vela synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "candela"
#     } else if (grepl("cer", Mcombined2$response[j])) {
#       # cadena synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "cerradura"
#     } else if (grepl("stam", Mcombined2$response[j])) {
#       # sello synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "estampilla"
#     } else if (grepl("zap", Mcombined2$response[j])) {
#       # calabaza synonym
#       Mcombined2$OrigLen[j] <- lenwords$AltPhon[pos]
#       Mcombined2$imgFilename[j] <- "zapallo"
#     }
#   } 
#   
#   rm(pos)
# }
# 
# for (j in 1:nrow(Mcombined2)) {
#   if (Mcombined2$error[j]==0){
#     Mcombined2$Corr[j] <- Mcombined2$OrigLen[j]
#     Mcombined2$Incorr[j] <- 0
#     Mcombined2$phoncorr[j]  <- Mcombined2$OrigLen[j]
#     Mcombined2$phonincorr[j]  <- 0
#   } else if (Mcombined2$error[j]==100){
#     Mcombined2$Corr[j] <- 0
#     Mcombined2$Incorr[j] <- Mcombined2$OrigLen[j]
#     Mcombined2$phoncorr[j]  <- 0
#     Mcombined2$phonincorr[j]  <- Mcombined2$OrigLen[j]
#   }
# }
# 
# Mcombined2$Total <- Mcombined2$phoncorr + Mcombined2$phonincorr
# 
# Mcombined2$CorrPer <- round(Mcombined2$phoncorr/Mcombined2$Total,2)
# Mcombined2$Corr <- round(Mcombined2$CorrPer*Mcombined2$OrigLen,0)
# Mcombined2$Incorr <- Mcombined2$OrigLen-Mcombined2$Corr
# Mcombined2$Ratio <- (Mcombined2$Corr/Mcombined2$OrigLen)*100
# Mcombined2$RatioIncorr <- (Mcombined2$Incorr/Mcombined2$OrigLen)*100
# 
# Mcombined2$session <- as.factor(Mcombined2$session)
# contrasts(Mcombined2$session) <- c(-0.5, 0.5)
# 
# ## full model with session as a factor in the model 
# modelfull2 <- glmer(cbind(Corr, Incorr) ~ session + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = Mcombined2)
# summary(modelfull2)
# # model comparisons 
# modelnull2 <- glmer(cbind(Corr, Incorr) ~ 1 + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = Mcombined2)
# anova(modelnull2, modelfull2)
# 
# ## what predicts changes in error rate from T2 to T3?
# individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")
# 
# ## add predictors to the full dataframe 
# colnames(individualdiff)[1] <- "ppn"
# df <- merge(Mcombined2, individualdiff, by = "ppn")
# 
# modelfull2 <- glmer(cbind(Corr, Incorr) ~ scale(T2_T3_Spanish) + (1|ppn) + (1|imgFilename), family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = df[df$session==3,])
# summary(modelfull2)
# 
# # plot interaction
# ggplot(aes(x=Mot_T3_avg, y=RatioIncorr), data = df[df$session==3,]) +
#   geom_smooth(method="lm")
