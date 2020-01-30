### Correlations between learning rate & ultimate L2 attainment abroad and frequency of use ratings 

# load packages
require(gdata)
require(tidyr)
require(here)
require(ggplot2)
library("readxl")
library(ggplot2)

# initiate empty dataframe 
res <- matrix(NA, 1,6)
res <- as.data.frame(res)
colnames(res) <- c("Token", "Date", "Spanish", "English", "German", "Token_Time")

# determine participants 
reg <- read.delim(here("ADMINISTRATION","REGISTRATION.txt"), header = T, stringsAsFactors = F)
# subset to good participants only
reg[is.na(reg$T2done)==0,]->reg2
reg2[reg2$T2done==1,]->reg
A <- unique(reg$token)

# read in T1 
setwd("U:/PhD/EXPERIMENT 4/DATA/T1")
T1 <- read.csv("results-survey422243.csv", stringsAsFactors = F)

for (i in 1:length(A)){
  num <- which(T1$ZugangsschlÃ.ssel == A[i])
  res[1+nrow(res),] <- NA
  res$Token[nrow(res)] <- A[i]
  res$Date[nrow(res)] <-  as.character(as.Date(T1$Datum.Abgeschickt[num]))
  res$Spanish[nrow(res)] <- mean(c(T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]))
  res$English[nrow(res)] <- mean(c(T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]))
  res$German[nrow(res)] <- mean(c(T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]))
  res$Token_Time[nrow(res)] <- "T1"
}

# read in T2
setwd("U:/PhD/EXPERIMENT 4/DATA/T2")
T2 <- read.csv("results-survey678123_full.csv", stringsAsFactors = F)

for (i in 1:length(A)){
  num <- which(T2$ZugangsschlÃ.ssel== A[i])
  if (length(num)!= 0){
    res[1+nrow(res),] <- NA
    res$Token[nrow(res)] <- A[i]
    res$Date[nrow(res)] <-  as.character(as.Date(T2$Datum.Abgeschickt[num]))
    res$Spanish[nrow(res)] <- mean(c(T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]))
    res$English[nrow(res)] <- mean(c(T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]))
    res$German[nrow(res)] <- mean(c(T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]))
    res$Token_Time[nrow(res)] <- "T2"
  } else {
    print(A[i])
  }
}

res <- res[-1,]


### Learning performance measures

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
T1dat <- read_excel("T1_PicNaming_DataCoded.xlsx")
T2dat <- read_excel("T2_PicNaming_DataCoded.xlsx")

# subset to coded people 
T1dat[T1dat =='NA'] <- NA
T2dat[T2dat=='NA'] <- NA
T1dat[is.na(T1dat$error) == 0,]-> T1sub
T2dat[is.na(T2dat$error) == 0,]-> T2sub

# set numeric 
T1sub$error <- as.numeric(T1sub$error)
T1sub$phoncorr <- as.numeric(T1sub$phoncorr)
T1sub$phonincorr <- as.numeric(T1sub$phonincorr)
for (i in 1:nrow(T1sub)){
  if (is.na(T1sub$error[i]) == 0 && T1sub$error[i] == 999){
    T1sub$error[i] <- T1sub$phonincorr[i]/(T1sub$phonincorr[i]+T1sub$phoncorr[i])
  }
}

# set numeric 
T2sub$error <- as.numeric(T2sub$error)
T2sub$phoncorr <- as.numeric(T2sub$phoncorr)
T2sub$phonincorr <- as.numeric(T2sub$phonincorr)
for (i in 1:nrow(T2sub)){
  if (is.na(T2sub$error[i]) == 0 && T2sub$error[i] == 999){
    T2sub$error[i] <- T2sub$phonincorr[i]/(T2sub$phonincorr[i]+T2sub$phoncorr[i])
  }
}

### subset T2 to those that have been coded for T1
T1sub$ppn <- as.factor(T1sub$ppn)
T2sub$ppn <- as.factor(T2sub$ppn)

T2sub2 <- T2sub[T2sub$ppn %in% T1sub$ppn,]
T2sub2$ppn <- droplevels(T2sub2$ppn)
# calculate mean error rates at T1 and T2 
T1mean <- as.data.frame(1-tapply(T1sub$error, list(T1sub$ppn), mean, na.rm = T))
T2mean <- as.data.frame(1-tapply(T2sub2$error, list(T2sub2$ppn), mean, na.rm = T))
colnames(T1mean) <- c("mean")
colnames(T2mean) <- c("mean")
## difference T1 - T2 
T1mean$diff <- T2mean$mean - T1mean$mean
T2mean$diff <- T2mean$mean - T1mean$mean

### correlations 
# create one dataframe with all relevant values in it 
combined <- T1mean
colnames(combined) <- c("T1mean", "T1T2diff")
combined$T2mean <- T2mean$mean

for (i in 1:nrow(combined)){
  ppnr <- rownames(combined)[i]
  num <- which(ppnr == res$Token)
  combined$T1Spanish[i] <- res$Spanish[num[1]]
  combined$T1English[i] <- res$English[num[1]]
  combined$T1German[i] <- res$German[num[1]]
  combined$T2Spanish[i] <- res$Spanish[num[2]]
  combined$T2English[i] <- res$English[num[2]]
  combined$T2German[i] <- res$German[num[2]]
  combined$SpanishDiff[i] <- combined$T2Spanish[i] - combined$T1Spanish[i]
  combined$EnglishDiff[i] <- combined$T2English[i] - combined$T1English[i]
  combined$GermanDiff[i] <- combined$T2German[i] - combined$T1German[i]
}

library("Hmisc")
res2 <- rcorr(as.matrix(combined))

for (i in 1:nrow(res2$r)) {
  for (j in 1:ncol(res2$r)) {
    if (is.na(res2$P[i,j])==0 && res2$P[i,j]<0.05){
    } else {
      res2$P[i,j] = NA
      res2$r[i,j] = NA
    }
  }}

