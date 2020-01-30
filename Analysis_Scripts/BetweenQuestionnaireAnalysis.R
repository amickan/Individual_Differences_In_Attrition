### Plot when people filled in their in between Questionnaires ####

# load packages
require(gdata)
require(tidyr)
require(here)
require(ggplot2)

setwd("U:/PhD/EXPERIMENT 4/DATA/InBetweenQuestionnaire")
res <- read.csv("results-survey775398_4.csv", stringsAsFactors = F)

# convert to date format and rename
res$Date <- as.Date(res$Datum.Abgeschickt)

# simplify tokens and rename the column
res$Token <- res$ZugangsschlÃ.ssel
for (i in 1: nrow(res)){
    res$Token[i] <- gsub("(\\d.*)_\\d.*","\\1", tolower(as.character(res$Token[i])))
}

# plot a line plot where one line is a participant and dots represent questionnaire answers over time 
ggplot(data = res, aes(x = Date, y = Token)) +
  geom_point() +
  geom_line() +
  labs(x = "Time", y = "Participants")


### Check percentages of use for Spanish, English and German 

# rename column names 
colnames(res) <- c("ID", "DateSent", "LastPage", "Lang", "Token_Time", "StartTime", "EndTime", "SpanishSpeaking", "EnglishSpeaking", "GermanSpeaking", "OtherSpeaking", "SpanishWriting", "EnglishWriting", "GermanWriting", "OtherWriting", "SpanishListening", "EnglishListening", "GermanListening", "OtherListening", "SpanishReading", "EnglishReading", "GermanReading", "OtherReading", "WhichOtherLang", "TotalTime", "GroupTime", "TimeFreq", "Comment", "Date", "Token")

# NAs to 0 
res[, 8:23][is.na(res[, 8:23])] <- 0

# calculate Average per Language 
res$Spanish <- NA
for (i in 1:nrow(res)){
  res$Spanish[i] <- mean(c(res$SpanishListening[i], res$SpanishReading[i], res$SpanishSpeaking[i], res$SpanishWriting[i]))}

res$English <- NA
for (i in 1:nrow(res)){
  res$English[i] <- mean(c(res$EnglishListening[i], res$EnglishReading[i], res$EnglishSpeaking[i], res$EnglishWriting[i]))}

res$German <- NA
for (i in 1:nrow(res)){
  res$German[i] <- mean(c(res$GermanListening[i], res$GermanReading[i], res$GermanSpeaking[i], res$GermanWriting[i]))}


# plot only Spanish use for each subject over the time points they provided 
ggplot() +
  geom_point(data = res, aes(x = Date, y = Spanish), color = "blue") +
  geom_line(data = res, aes(x = Date, y = Spanish), color = "blue") +
  geom_point(data = res, aes(x = Date, y = English), color = "red") +
  geom_line(data = res, aes(x = Date, y = English), color = "red") +
  geom_point(data = res, aes(x = Date, y = German), color = "black") +
  geom_line(data = res, aes(x = Date, y = German), color = "black") +
  facet_wrap(~Token) + 
  labs(title = "Spanish", x = "Time", y = "Frequency of Use (0 - 100 %)")


# in this plot also mark each persons start date, leaving date, T1, T2 and T3 date
# create new dataframe with that information taken from the T1, T2, T3 and REGISTRATION forms
# read in REGISTRATION FROM 
reg <- read.delim("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION/REGISTRATION.txt", header = T, stringsAsFactors = F)
# subset to good participants only
reg[is.na(reg$T2done)==0,]->reg2
reg2[reg2$T2done==1,]->reg
A <- unique(reg$token)

for (i in 1:length(A)){
  num <- which(reg$token == A[i])
  if (length(num) == 1){
  res[1+nrow(res),] <- NA
  res$Token[nrow(res)] <- A[i]
  res$StartDate[nrow(res)] <-  reg$Startdatum[num]
  res$EndDate[nrow(res)] <- reg$LeaveDateActual[num]} 
}
res$StartDate <- as.Date(res$StartDate)
res$EndDate <- as.Date(res$EndDate)

k = 1
keep <- NA
for (i in 1:nrow(res)){
  if (res$Token[i] %in% A){
    keep[k] <- i
    k <- k+1
  } 
}
res <- res[keep,]

# read in T1 
setwd("U:/PhD/EXPERIMENT 4/DATA/T1")
T1 <- read.csv("results-survey422243.csv", stringsAsFactors = F)
#for (i in 1:nrow(res)){
#  num <- which(T1$ZugangsschlÃ.ssel == res$Token[i])
#  res$T1Date[i] <-  as.character(as.Date(T1$Datum.Abgeschickt[num]))
#  res$SpanishT1[i] <- mean(c(T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]))
#  res$EnglishT1[i] <- mean(c(T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]))
#  res$GermanT1[i] <- mean(c(T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num], T1$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]))
#}
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
T2 <- read.csv("results-survey678123_full_3.csv", stringsAsFactors = F)
#T2[T2$Letzte.Seite==6,]->T2
##for (i in 1:nrow(res)){
#  num <- which(T2$ZugangsschlÃ.ssel== res$Token[i])
#  if (length(num)!= 0){
#    res$T2Date[i] <-  as.character(as.Date(T2$Datum.Abgeschickt[num]))
#    res$SpanishT2[i] <- mean(c(T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]))
#    res$EnglishT2[i] <- mean(c(T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]))
#    res$GermanT2[i] <- mean(c(T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num], T2$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]))
#  } else {
#    res$T2Date[i] <- NA
#    res$SpanishT2[i] <- NA
#    res$EnglishT2[i] <- NA
#    res$GermanT2[i] <- NA
#  }
#}
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

# read in times they went to Germany over Christmas
#filescsv <- file.info("U:/PhD/EXPERIMENT 4/SCRIPTS/SensitiveData/Christmas/")
christmas <- read.csv("U:/PhD/EXPERIMENT 4/SCRIPTS/SensitiveData/Christmas/results-survey454351 (24).csv", stringsAsFactors = F)
colnames(christmas)[6] <- "Gone"
colnames(christmas)[7] <- "Leave"
colnames(christmas)[8] <- "Back"

chrisvacay <- matrix(NA,nrow(christmas[christmas$Gone=="Ja",]),3)
k <- 1
for (i in 1:nrow(christmas)){
  if (christmas$Gone[i] == "Ja"){
    chrisvacay[k,1] <- christmas$ZugangsschlÃ.ssel[i]
    chrisvacay[k,2] <- as.character(as.Date(christmas$Leave[i]))
    chrisvacay[k,3] <- as.character(as.Date(christmas$Back[i]))
    k <- k +1
  }
}
colnames(chrisvacay) <- c("Token", "Leave", "Back")
chrisvacay <- as.data.frame(chrisvacay)
chrisvacay$Leave <- as.Date(chrisvacay$Leave)
chrisvacay$Back <- as.Date(chrisvacay$Back)

# filter out participants that are no longer in the dataset
k = 1
keep <- NA
for (i in 1:nrow(chrisvacay)){
  if (chrisvacay$Token[i] %in% A){
    keep[k] <- i
    k <- k+1
  } 
}
chrisvacay <- chrisvacay[keep,]

# read in T2
setwd("U:/PhD/EXPERIMENT 4/DATA/T3")
T3 <- read.csv("results-survey827149-2.csv", stringsAsFactors = F)
T3 <- T3[which(T3$Letzte.Seite==6),]
for (i in 1:length(A)){
  num <- which(T3$ZugangsschlÃ.ssel== A[i])
  if (length(num)!= 0){
    res[1+nrow(res),] <- NA
    res$Token[nrow(res)] <- A[i]
    res$Date[nrow(res)] <-  as.character(as.Date(T3$Datum.Abgeschickt[num]))
    res$Spanish[nrow(res)] <- mean(c(T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂSprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]))
    res$English[nrow(res)] <- mean(c(T3$T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂSprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]))
    res$German[nrow(res)] <- mean(c(T3$T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂSprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num], T3$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ.die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]))
    res$Token_Time[nrow(res)] <- "T3"
    print(A[i])
  } else {
    res$Token[nrow(res)] <- NA
    res$Date[nrow(res)] <-  NA
    res$Spanish[nrow(res)] <- NA
    res$English[nrow(res)] <- NA
    res$German[nrow(res)] <- NA
    res$Token_Time[nrow(res)] <- NA
    print(A[i])
  }
}

# subset only to the datapoints for T1 and T2 
res[is.na(res$StartDate)==0,]->dates
res[is.na(res$StartDate)==1,]->res
dates$StartTime <- 0 
dates$EndTime <- 25

# put T1, T2, T3 dates in one dataframe 
datesT1 <- res[which(res$Token_Time == "T1"),]
datesT2 <- res[which(res$Token_Time == "T2"),]
datesT3 <- res[which(res$Token_Time == "T3"),]

ggplot() +
  geom_point(data = res, aes(x = Date, y = Spanish), color = "blue") +
  geom_line(data = res, aes(x = Date, y = Spanish), color = "blue") +
  geom_point(data = res, aes(x = Date, y = English), color = "red") +
  geom_line(data = res, aes(x = Date, y = English), color = "red") +
  geom_point(data = res, aes(x = Date, y = German), color = "black") +
  geom_line(data = res, aes(x = Date, y = German), color = "black") +
  geom_segment(data = dates, aes(x = StartDate, xend = StartDate, y = 0, yend = 20)) +
  geom_point(data = dates, aes(x=StartDate ,y=20), size=1, shape = 18) +
  geom_text(data = dates, aes(x=StartDate, y=20, label="Start"), hjust=-0.1, vjust=0.1, size=2, angle = 90) +
  geom_segment(data = dates, aes(x = EndDate, xend = EndDate, y = 0, yend = 20)) +
  geom_point(data = dates, aes(x=EndDate ,y=20), size=1, shape = 18) +
  geom_text(data = dates, aes(x=EndDate, y=20, label="End"), hjust=-0.1, vjust=0.1, size=2, angle = 90) +
  geom_point(data = datesT1, aes(x=Date ,y=20), size=1, shape = 18) +
  geom_text(data = datesT1, aes(x=Date, y=20, label="T1"), hjust=-0.1, vjust=0.1, size=2, angle = 90) +
  geom_point(data = datesT2, aes(x=Date ,y=20), size=1, shape = 18) +
  geom_text(data = datesT2, aes(x=Date, y=20, label="T2"), hjust=-0.1, vjust=0.1, size=2, angle = 90) +
  geom_point(data = datesT3, aes(x=Date ,y=20), size=1, shape = 18) +
  geom_text(data = datesT3, aes(x=Date, y=20, label="T3"), hjust=-0.1, vjust=0.1, size=2, angle = 90) +
  geom_rect(data = chrisvacay, aes(xmin = Leave, xmax = Back, ymin = -15, ymax = 0)) +
  facet_wrap(~Token) + 
  labs(title = "Frequency of Use in %", x = "Time", y = "Frequency of Use (0 - 100 %)")

# on average?
# first leave only those values in per person after T2 (and after actual leaving date?)
# average over 1st month after T2, 2nd, 3rd, 4th, 5th, T3 # here we will sometimes have missing values, so be it 
# plot the 6 months between T2 and T3 (the measurements points correspond to different days for each person, but what is common is the time distance from T2 and T3)

# correlations with learning rates from T1 and T2 (quick and dirty)
corr.test(T2time$English[which(T2time$Token %in% row.names(T2mean))], T2mean$mean)

#### calculate mean frequency of use between T1 and T2 and between T2 and T3 per person
frequencystats <- matrix(NA, nrow(reg), 7)
for (i in 1:nrow(reg)){
  frequencystats[i,1] <- reg$token[i]
  subsetpp <- res[res$Token==reg$token[i],]
  ind <- colSums(is.na(subsetpp)) == nrow(subsetpp)
  if (isFALSE(ind[30]) == F){
    frequencystats[i,2] <- NA
    frequencystats[i,3] <- NA
    frequencystats[i,4] <- NA
    frequencystats[i,5] <- NA
    frequencystats[i,6] <- NA
    frequencystats[i,7] <- NA
  } else {
  leavedate <- reg$LeaveDateActual[i]
  frequencystats[i,2] <- mean(subsetpp[which(subsetpp$Token_Time == "T2" | subsetpp$StartTime < leavedate),]$Spanish)
  frequencystats[i,3] <- mean(subsetpp[which(subsetpp$Token_Time == "T2" | subsetpp$StartTime < leavedate),]$English)
  frequencystats[i,4] <- mean(subsetpp[which(subsetpp$Token_Time == "T2" | subsetpp$StartTime < leavedate),]$German)
  t2date <- reg$completedLimeT2[i]
  frequencystats[i,5] <- mean(subsetpp[which(subsetpp$Token_Time == "T3" | subsetpp$StartTime > leavedate),]$Spanish)
  frequencystats[i,6] <- mean(subsetpp[which(subsetpp$Token_Time == "T3" | subsetpp$StartTime > leavedate),]$English)
  frequencystats[i,7] <- mean(subsetpp[which(subsetpp$Token_Time == "T3" | subsetpp$StartTime > leavedate),]$German)
  }
}
colnames(frequencystats) <- c("PP", "T1_T2_Spanish", "T1_T2_English", "T1_T2_German", "T2_T3_Spanish", "T2_T3_English", "T2_T3_German")
frequencystats <- as.data.frame(frequencystats)

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
individualdiff <- read.delim("T1_T2_T3_lime_clean.txt", sep = "\t")

individualdiff$T1_T2_Spanish <- NA
individualdiff$T1_T2_English <- NA
individualdiff$T1_T2_German <- NA
individualdiff$T2_T3_Spanish <- NA
individualdiff$T2_T3_English <- NA
individualdiff$T2_T3_German <- NA

for (i in 1:nrow(individualdiff)){
  num <- which(frequencystats$PP == individualdiff$Ppn[i])
  if (length(num)==1){
    individualdiff$T1_T2_Spanish[i] <- frequencystats$T1_T2_Spanish[num]
    individualdiff$T1_T2_English[i] <- frequencystats$T1_T2_English[num]
    individualdiff$T1_T2_German[i] <- frequencystats$T1_T2_German[num]
    individualdiff$T2_T3_Spanish[i] <- frequencystats$T2_T3_Spanish[num]
    individualdiff$T2_T3_English[i] <- frequencystats$T2_T3_English[num]
    individualdiff$T2_T3_German[i] <- frequencystats$T2_T3_German[num]
  } else {
    individualdiff$T1_T2_Spanish[i] <- NA
    individualdiff$T1_T2_English[i] <- NA
    individualdiff$T1_T2_German[i] <- NA
    individualdiff$T2_T3_Spanish[i] <- NA
    individualdiff$T2_T3_English[i] <- NA
    individualdiff$T2_T3_German[i] <- NA
    print(individualdiff$Ppn[i])
  }
}
write.table(individualdiff, "T1_T2_T3_lime_clean.txt", sep = "\t", quote = F, row.names = F)
