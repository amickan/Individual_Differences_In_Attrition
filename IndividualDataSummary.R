### Individual data from limesurvey 
library(ggcorrplot)

# T1 limesurvey 
T1lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T1_results.csv", stringsAsFactors = F)

# T2 limesurvey 
T2lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T2_results_new.csv", stringsAsFactors = F)

# T3 limesurvey
T3lime <- read.csv("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T3_results_new.csv", stringsAsFactors = F)

# participants
A <- unique(T2lime[T2lime$Letzte.Seite==6,]$ZugangsschlÃ.ssel)
T3lime <- T3lime[T3lime$Letzte.Seite...>4,]

## extract potentially relevant information into one dataframe 
data <- data.frame(Ppn=integer(),
                      SpanishAoA=integer(),
                      Age=integer(),
                      Gender=character(),
                      StudyYear=character(),
                      StudyYearOther=character(),
                      PlaceInSpain=character(),
                      PlaceInSpainOther=character(),
                      EnglishAoA=integer(),
                      T1_SRP_English_Listening=character(),
                      T1_SRP_English_Reading=character(),
                      T1_SRP_English_Speaking=character(),
                      T1_SRP_English_Writing=character(),
                      T1_SRP_Spanish_Listening=character(),
                      T1_SRP_Spanish_Reading=character(),
                      T1_SRP_Spanish_Speaking=character(),
                      T1_SRP_Spanish_Writing=character(),
                      T1_SpanishCERF=character(),
                      T1_SpanishCERF2=character(),
                      T1_French=character(),
                      T1_Italian=character(),
                      T1_Portugese=character(),
                      T1_Russian=character(),
                      T1_Latin=character(),
                      T1_Catalan=character(),
                      T1SpanishFreq=integer(),
                      T1EnglishFreq=integer(),
                      T1GermanFreq=integer(),
                      T1OtherFreq=integer(),
                      T1SpanishFreqWriting=integer(),
                      T1SpanishFreqListening=integer(),
                      T1SpanishFreqReading=integer(),
                      T1SpanishFreqSpeaking=integer(),
                      T1EnglishFreqWriting=integer(),
                      T1EnglishFreqListening=integer(),
                      T1EnglishFreqReading=integer(),
                      T1EnglishFreqSpeaking=integer(),
                      T1GermanFreqWriting=integer(),
                      T1GermanFreqListening=integer(),
                      T1GermanFreqReading=integer(),
                      T1GermanFreqSpeaking=integer(),
                      T2SpanishFreq=integer(),
                      T2EnglishFreq=integer(),
                      T2GermanFreq=integer(),
                      T2OtherFreq=integer(),
                      T2SpanishFreqWriting=integer(),
                      T2SpanishFreqListening=integer(),
                      T2SpanishFreqReading=integer(),
                      T2SpanishFreqSpeaking=integer(),
                      T2EnglishFreqWriting=integer(),
                      T2EnglishFreqListening=integer(),
                      T2EnglishFreqReading=integer(),
                      T2EnglishFreqSpeaking=integer(),
                      T2GermanFreqWriting=integer(),
                      T2GermanFreqListening=integer(),
                      T2GermanFreqReading=integer(),
                      T2GermanFreqSpeaking=integer(),
                      T2JudgmentStudyAbroad_Overall=character(),
                      T2JudgmentStudyAbroad_Lang=character(),
                      T2JudgmentStudyAbroad_Social=character(),
                      T2JudgmentStudyAbroad_Study=character(),
                      T2integrated_lang=integer(),
                      T2integrated_social=integer(),
                      T2_SRP_Spanish_Reading=integer(),
                      T2_SRP_Spanish_Listening=integer(),
                      T2_SRP_Spanish_Writing=integer(),
                      T2_SRP_Spanish_Speaking=integer(),
                      T2_SRP_English_Reading=integer(),
                      T2_SRP_English_Listening=integer(),
                      T2_SRP_English_Writing=integer(),
                      T2_SRP_English_Speaking=integer(),
                      T2_EnglishInput_Native=integer(),
                      T2_SpanishInput_Native=integer(),
                      T2_EnglishInput_NonNative=integer(),
                      T2_SpanishInput_NonNative=integer(),
                      T2_SpanishInput_NotGerman=integer(),
                      T3SpanishFreqWriting=integer(),
                      T3SpanishFreqListening=integer(),
                      T3SpanishFreqReading=integer(),
                      T3SpanishFreqSpeaking=integer(),
                      T3EnglishFreqWriting=integer(),
                      T3EnglishFreqListening=integer(),
                      T3EnglishFreqReading=integer(),
                      T3EnglishFreqSpeaking=integer(),
                      T3GermanFreqWriting=integer(),
                      T3GermanFreqListening=integer(),
                      T3GermanFreqReading=integer(),
                      T3GermanFreqSpeaking=integer(),
                      T3SpanishFreq=integer(),
                      T3EnglishFreq=integer(),
                      T3GermanFreq=integer(),
                      T3OtherFreq=integer(),
                      T3_SpanishLessonGermany=character(),
                      T3_LivingSitGermany_Spanish=character(),
                      T3_KeepUpSpanishActively=character(),
                      T3_MaintainedSpanishContact=character(),
                      T3_WatchSpanishMovies=character(),
                      T3_ReadSpanishBooks=character(),
                      T3_SRP_Spanish_Reading=integer(),
                      T3_SRP_Spanish_Listening=integer(),
                      T3_SRP_Spanish_Writing=integer(),
                      T3_SRP_Spanish_Speaking=integer(),
                      T3_SRP_English_Reading=integer(),
                      T3_SRP_English_Listening=integer(),
                      T3_SRP_English_Writing=integer(),
                      T3_SRP_English_Speaking=integer(),
                      T3_EnglishInput_Native=integer(),
                      T3_SpanishInput_Native=integer(),
                      T3_EnglishInput_NonNative=integer(),
                      T3_SpanishInput_NonNative=integer(),
                      T3_SpanishInput_NotGerman=integer(),
                      stringsAsFactors = F
)
data[length(A),]<- NA

for (i in 1:length(A)){
  pnum <- A[i]
  
  data$Ppn[i] <- pnum
  
  # extractions from T1 limesurvey
  num <- which(T1lime$ZugangsschlÃ.ssel == pnum)
  data$SpanishAoA[i] <- as.numeric(T1lime$Wie.alt.warst.du..als.angefangen.hast.Spanisch.zu.lernen.[num])
  data$Age[i] <- as.numeric(T1lime$Wie.alt.bist.du.[num])
  data$Gender[i] <- as.character(T1lime$Geschlecht[num])
  data$StudyYear[i] <- as.character(T1lime$In.welchem.Studienjahr.befindest.du.dich.[num])
  data$StudyYearOther[i] <- as.character(T1lime$In.welchem.Studienjahr.befindest.du.dich...Sonstiges.[num])
  data$PlaceInSpain[i] <- as.character(T1lime$An.welcher.spanischen.Uni.wirst.du.studieren.[num])
  data$PlaceInSpainOther[i] <- as.character(T1lime$An.welcher.spanischen.Uni.wirst.du.studieren...Sonstiges.[num])
  # maybe also whether they deliberately decided to go to Spain or not 
  # English 
  data$EnglishAoA[i] <- as.numeric(T1lime$Wie.alt.warst.du..als.du.angefangen.hast.Englisch.zu.lernen.[num])
  data$T1_SRP_English_Listening[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...HÃ.ren.[num])
  data$T1_SRP_English_Reading[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Lesen.[num])
  data$T1_SRP_English_Speaking[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Sprechen.[num])
  data$T1_SRP_English_Writing[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Schreiben.[num])
  # Spanish 
  data$SpanishhAoA[i] <- as.numeric(T1lime$Wie.alt.warst.du..als.angefangen.hast.Spanisch.zu.lernen.[num])
  data$T1_SRP_Spanish_Listening[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...HÃ.ren.[num])
  data$T1_SRP_Spanish_Reading[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Lesen.[num])
  data$T1_SRP_Spanish_Speaking[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Sprechen.[num])
  data$T1_SRP_Spanish_Writing[i] <- as.character(T1lime$Wie.schÃ.tzt.du.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Schreiben.[num])
  data$T1_SpanishCERF[i] <- as.character(T1lime$Zu.welchem.CERF.Niveau.hat.dein.letzter.Spanischkurs.in.Deutschland.gefÃ.hrt.Â.[num])
  data$T1_SpanishCERF2[i] <- as.character(T1lime$Zu.welchem.CERF.Niveau.hat.dein.letzter.Spanischkurs.in.Deutschland.gefÃ.hrt.Â...Sonstiges.[num])
  data$T1_French[i] <- as.character(T1lime$Wie.schÃ.tz.du.deine.Sprachkenntnisse.in.den.folgenden.Sprachen.ein...FranzÃ.sisch.[num])
  data$T1_Italian[i] <- as.character(T1lime$Wie.schÃ.tz.du.deine.Sprachkenntnisse.in.den.folgenden.Sprachen.ein...Italienisch.[num])
  data$T1_Portugese[i] <- as.character(T1lime$Wie.schÃ.tz.du.deine.Sprachkenntnisse.in.den.folgenden.Sprachen.ein...Portugiesisch.[num])
  data$T1_Catalan[i] <- as.character(T1lime$Wie.schÃ.tz.du.deine.Sprachkenntnisse.in.den.folgenden.Sprachen.ein...Katalanisch.[num])
  data$T1_Russian[i] <- as.character(T1lime$Wie.schÃ.tz.du.deine.Sprachkenntnisse.in.den.folgenden.Sprachen.ein...Russisch.[num])
  data$T1_Latin[i] <- as.character(T1lime$Wie.schÃ.tz.du.deine.Sprachkenntnisse.in.den.folgenden.Sprachen.ein...Latein.[num])
  
  data$T1SpanishFreqWriting[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num])
  data$T1SpanishFreqListening[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num])
  data$T1SpanishFreqReading[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num])
  data$T1SpanishFreqSpeaking[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num])
  
  data$T1EnglishFreqWriting[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num])
  data$T1EnglishFreqListening[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num])
  data$T1EnglishFreqReading[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num])
  data$T1EnglishFreqSpeaking[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num])
  
  data$T1GermanFreqWriting[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num])
  data$T1GermanFreqListening[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num])
  data$T1GermanFreqReading[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num])
  data$T1GermanFreqSpeaking[i] <- as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num])
  
  
  # T1 Frequency of Use means
  data$T1SpanishFreq[i] <- mean(c(as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num]), 
                                  as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num]),
                                  as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]),
                                  as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num])))
  data$T1EnglishFreq[i] <- mean(c(as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num]), 
                                as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num]),
                                as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]),
                                as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num])))
  data$T1GermanFreq[i] <- mean(c(as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num]), 
                                 as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num]),
                                 as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]),
                                 as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num])))
  data$T1OtherFreq[i] <- mean(c(as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Andere.Sprachen.[num]), 
                                as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Andere.Sprachen.[num]),
                                as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Andere.Sprachen.[num]),
                                as.numeric(T1lime$Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.entweder.noch.in.Deutschland.oder.in.den.ersten.Wochen.im.Ausland..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Andere.Sprachen.[num])))
  
  num <- which(T2lime$ZugangsschlÃ.ssel == pnum)
  # T2 frequency of use single measures 
  data$T2SpanishFreqWriting[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num])
  data$T2SpanishFreqListening[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num])
  data$T2SpanishFreqReading[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num])
  data$T2SpanishFreqSpeaking[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num])
  
  data$T2EnglishFreqWriting[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num])
  data$T2EnglishFreqListening[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num])
  data$T2EnglishFreqReading[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num])
  data$T2EnglishFreqSpeaking[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num])
  
  data$T2GermanFreqWriting[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num])
  data$T2GermanFreqListening[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num])
  data$T2GermanFreqReading[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num])
  data$T2GermanFreqSpeaking[i] <- as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num])
  
  # T2 Frequency of Use means
  data$T2SpanishFreq[i] <- mean(c(as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Spanisch.[num]), 
                                  as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Spanisch.[num]),
                                  as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Spanisch.[num]),
                                  as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Spanisch.[num])))
  data$T2EnglishFreq[i] <- mean(c(as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Englisch.[num]), 
                                  as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Englisch.[num]),
                                  as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Englisch.[num]),
                                  as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Englisch.[num])))
  data$T2GermanFreq[i] <- mean(c(as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Deutsch.[num]), 
                                 as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Deutsch.[num]),
                                 as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Deutsch.[num]),
                                 as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Deutsch.[num])))
  data$T2OtherFreq[i] <- mean(c(as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SCHREIBEN..z.B...E.Mails..Briefe..Formulare..PrÃ.fungen..im.Internet.......Andere.Sprachen.[num]), 
                                as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....HÃ.REN..z.B...an.der.Uni..im.Alltag..im.Radio..Fernsehen..mit.Freunden.und.oder.Familie.......Andere.Sprachen.[num]),
                                as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....LESEN..z.B...im.Internet..BÃ.cher.in.der.Freizeit.und.an.der.Uni..E.Mails.......Andere.Sprachen.[num]),
                                as.numeric(T2lime$Sprachgebrauch.in.Prozent...Gib.an..zu.wie.viel.Prozent.du.aktuellÂ..also.in.den.letzten.Wochen.im.Ausland..bzw..seit.der.letzten.Erhebung.dieser.Frage..die.folgenden.Sprachen.in.den.angegebenen.Kontexten.verwendest..siehe.Beispielsituationen.in.Klammern.....Jede.Reihe.sollte.insgesamt.100..ergeben..siehe.Berechnung.unter.der.Frage.als.Hilfe....SPRECHEN..z.B...mit.Freunden..Familie..an.der.Uni..im.Pub.Club..im.Restaurant..beim.Einkaufen.......Andere.Sprachen.[num])))
  
  data$T2JudgmentStudyAbroad_Overall[i] <- T2lime$Wie.wÃ.rdest.du.deinen.Auslandsaufenthalt.in.den.folgenden.Aspekten.bewerten...Insgesamt.[num]
  data$T2JudgmentStudyAbroad_Lang[i] <- T2lime$Wie.wÃ.rdest.du.deinen.Auslandsaufenthalt.in.den.folgenden.Aspekten.bewerten...aus.sprachlicher.Sicht..Hast.du.sprachlich.Fortschritte.gemacht..Kann.sich.auf.Spanisch.aber.auch.auf.Englisch.beziehen...[num]
  data$T2JudgmentStudyAbroad_Social[i] <- T2lime$Wie.wÃ.rdest.du.deinen.Auslandsaufenthalt.in.den.folgenden.Aspekten.bewerten...aus.sozialer.Sicht..Warst.du.zufrieden.mit.deinem.Sozialleben..oder.hÃ.ttest.du.lieber.mehr.weniger.Kontakte.geknÃ.pft...[num]
  data$T2JudgmentStudyAbroad_Study[i] <- T2lime$Wie.wÃ.rdest.du.deinen.Auslandsaufenthalt.in.den.folgenden.Aspekten.bewerten...aus.fachlicher.Sicht..Hast.du.viel.gelernt..War.es.fÃ.r.dein.Fachstudium.bereichernd...[num]
  
  data$T2integrated_lang[i] <- T2lime$Wie.integriertÂ.wÃ.rdest.du.sagen.warst.bist.du.in.die.spanisch.lateinamerikanische.Gemeinschaft...Kultur.und.Sprache..Â...Sprache.[num]
  data$T2integrated_social[i] <- T2lime$Wie.integriertÂ.wÃ.rdest.du.sagen.warst.bist.du.in.die.spanisch.lateinamerikanische.Gemeinschaft...Kultur.und.Sprache..Â...Kultur...Gesellschaft.[num]
  
  data$T2_SRP_Spanish_Reading[i] <- T2lime$Wie.schÃ.tzt.duÂ.aktuellÂ.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Lesen.[num]
  data$T2_SRP_Spanish_Listening[i] <- T2lime$Wie.schÃ.tzt.duÂ.aktuellÂ.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...HÃ.ren.[num]
  data$T2_SRP_Spanish_Writing[i] <- T2lime$Wie.schÃ.tzt.duÂ.aktuellÂ.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Schreiben.[num]
  data$T2_SRP_Spanish_Speaking[i] <- T2lime$Wie.schÃ.tzt.duÂ.aktuellÂ.deine.spanischen.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Sprechen.[num]
  
  data$T2_SRP_English_Reading[i] <- T2lime$Wie.schÃ.tzt.du.aktuellÂ.deineÂ.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Lesen.[num]
  data$T2_SRP_English_Listening[i] <- T2lime$Wie.schÃ.tzt.du.aktuellÂ.deineÂ.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...HÃ.ren.[num]
  data$T2_SRP_English_Writing[i] <- T2lime$Wie.schÃ.tzt.du.aktuellÂ.deineÂ.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Schreiben.[num]
  data$T2_SRP_English_Speaking[i] <- T2lime$Wie.schÃ.tzt.du.aktuellÂ.deineÂ.englischenÂ.Sprachkenntnisse.auf.den.folgenden.Gebieten.ein...Sprechen.[num]
  
  data$T2_EnglishInput_Native[i] <- T2lime$Zu.wie.viel.Prozent.hast.du.im.Ausland.mit.den.aufgefÃ.hrten.PersonengruppenÂ.Englisch.gesprochen..Nimm.die.vergangene.Woche..oder.die.letzte.Woche.im.Ausland..als.Referenzpunkt....Die.Summe.sollte.100..ergeben..100..reflektiert.die.Gesamtheit.deiner.Zeit..die.du.Englisch.gesprochen.hast..also.auch.wenn.das.stundenmÃ.ÃŸig.wenig.war..geht.es.uns.darum..wie.viel.von.dieser.Zeit.du.mit.wem.gesprochen.hast...englische.Muttersprachler...[num] 
  data$T2_SpanishInput_Native[i] <- T2lime$Zu.wie.viel.Prozent.hast.du.im.Ausland.mit.den.aufgefÃ.hrten.Personengruppen.SpanischÂ.gesprochen..Nimm.die.vergangene.Woche..oder.die.letzte.Woche.im.Ausland..als.Referenzpunkt....Die.Summe.sollte.100..ergeben..100..reflektiert.die.Gesamtheit.deiner.Zeit..die.du.Spanisch.gesprochen.hast..also.auch.wenn.das.stundenmÃ.ÃŸig.wenig.war..geht.es.uns.darum..wie.viel.von.dieser.Zeit.du.mit.wem.gesprochen.hast...spanische.Muttersprachler...[num] 
  data$T2_SpanishInput_NotGerman[i] <- T2lime$Sprachgebrauch.nach.Personengruppen...Welche.Sprachen.hast.du.mit.den.aufgelisteten.Personengruppen.im.Ausland.gesprochen....Gib.pro.Gruppe.an.zu.wie.viel.Prozent.du.mit.dieser.Gruppe.die.entsprechenden.Sprachen.gesprochen.hast....Pro.Gruppe.sollte.die.Summe.100...betragen..siehe.Spalte..Gesamt.....Die.Summe.pro.Sprachspalte.kann.mehr.als.100..sein..Wenn.du.beispielsweise.mit.Spaniern.immer.Spanisch.gesprochen.hast...und.mit.internationalen.Studenten.20..der.Zeit.Spanisch.gesprochen.hast..wÃ.re.die.Summe.der.Spanischspalte.120....das.ist.vÃ.llig.ok..sobald.die.Summe.pro.Reihe..also.pro.Gruppe.immer.100..ergibt.Â...andere..nicht.Deutsche..internationale.Studenten..Spanisch.[num]
  data$T2_EnglishInput_NonNative[i] <- T2lime$Zu.wie.viel.Prozent.hast.du.im.Ausland.mit.den.aufgefÃ.hrten.PersonengruppenÂ.Englisch.gesprochen..Nimm.die.vergangene.Woche..oder.die.letzte.Woche.im.Ausland..als.Referenzpunkt....Die.Summe.sollte.100..ergeben..100..reflektiert.die.Gesamtheit.deiner.Zeit..die.du.Englisch.gesprochen.hast..also.auch.wenn.das.stundenmÃ.ÃŸig.wenig.war..geht.es.uns.darum..wie.viel.von.dieser.Zeit.du.mit.wem.gesprochen.hast...Nichtmuttersprachler...[num] 
  data$T2_SpanishInput_NonNative[i] <- T2lime$Zu.wie.viel.Prozent.hast.du.im.Ausland.mit.den.aufgefÃ.hrten.Personengruppen.SpanischÂ.gesprochen..Nimm.die.vergangene.Woche..oder.die.letzte.Woche.im.Ausland..als.Referenzpunkt....Die.Summe.sollte.100..ergeben..100..reflektiert.die.Gesamtheit.deiner.Zeit..die.du.Spanisch.gesprochen.hast..also.auch.wenn.das.stundenmÃ.ÃŸig.wenig.war..geht.es.uns.darum..wie.viel.von.dieser.Zeit.du.mit.wem.gesprochen.hast...Nichtmuttersprachler...[num] 
  
  # Lime T3 extractions 
  num <- which(T3lime$ZugangsschlÃ.ss... == pnum)
  if (length(num)==0){
    data$T3SpanishFreqWriting[i] <- NA
    data$T3SpanishFreqListening[i] <- NA
    data$T3SpanishFreqReading[i] <- NA
    data$T3SpanishFreqSpeaking[i] <- NA
    
    data$T3EnglishFreqWriting[i] <- NA
    data$T3EnglishFreqListening[i] <- NA
    data$T3EnglishFreqReading[i] <- NA
    data$T3EnglishFreqSpeaking[i] <- NA
    
    data$T3GermanFreqWriting[i] <- NA
    data$T3GermanFreqListening[i] <- NA
    data$T3GermanFreqReading[i] <- NA
    data$T3GermanFreqSpeaking[i] <- NA
    
    data$T3SpanishFreq[i] <- NA
    data$T3EnglishFreq[i] <- NA
    data$T3GermanFreq[i] <- NA
    data$T3OtherFreq[i] <- NA
    data$T3_SpanishLessonGermany[i] <- NA
    data$T3_LivingSitGermany_Spanish[i] <- NA
    data$T3_KeepUpSpanishActively[i]<- NA
    data$T3_MaintainedSpanishContact[i]<- NA
    data$T3_WatchSpanishMovies[i]<- NA
    data$T3_ReadSpanishBooks[i]<- NA
    
    data$T3_SRP_Spanish_Reading[i]<- NA
    data$T3_SRP_Spanish_Listening[i]<- NA
    data$T3_SRP_Spanish_Writing[i]<- NA
    data$T3_SRP_Spanish_Speaking[i]<- NA
    data$T3_SRP_English_Reading[i]<- NA
    data$T3_SRP_English_Listening[i]<- NA
    data$T3_SRP_English_Writing[i]<- NA
    data$T3_SRP_English_Speaking[i]<- NA
    
    data$T3_EnglishInput_Native[i]<- NA
    data$T3_SpanishInput_Native[i]<- NA
    data$T3_EnglishInput_NonNative[i]<- NA
    data$T3_SpanishInput_NonNative[i]<- NA
    data$T3_SpanishInput_NotGerman[i]<- NA
    
  } else {
    # T2 frequency of use single measures 
    data$T3SpanishFreqWriting[i] <- as.numeric(T3lime$Sprachgebrauch.....write_a.[num])
    data$T3SpanishFreqListening[i] <- as.numeric(T3lime$Sprachgebrauch.....list_a.[num])
    data$T3SpanishFreqReading[i] <- as.numeric(T3lime$Sprachgebrauch.....read_a.[num])
    data$T3SpanishFreqSpeaking[i] <- as.numeric(T3lime$Sprachgebrauch.....speak_a.[num])
    
    data$T3EnglishFreqWriting[i] <- as.numeric(T3lime$Sprachgebrauch.....write_b.[num])
    data$T3EnglishFreqListening[i] <- as.numeric(T3lime$Sprachgebrauch.....list_b.[num])
    data$T3EnglishFreqReading[i] <- as.numeric(T3lime$Sprachgebrauch.....read_b.[num])
    data$T3EnglishFreqSpeaking[i] <- as.numeric(T3lime$Sprachgebrauch.....speak_b.[num])
    
    data$T3GermanFreqWriting[i] <- as.numeric(T3lime$Sprachgebrauch.....write_c.[num])
    data$T3GermanFreqListening[i] <- as.numeric(T3lime$Sprachgebrauch.....list_c.[num])
    data$T3GermanFreqReading[i] <- as.numeric(T3lime$Sprachgebrauch.....read_c.[num])
    data$T3GermanFreqSpeaking[i] <- as.numeric(T3lime$Sprachgebrauch.....speak_c.[num])
  
    # T2 Frequency of Use means
    data$T3SpanishFreq[i] <- mean(c(as.numeric(T3lime$Sprachgebrauch.....write_a.[num]), 
                                    as.numeric(T3lime$Sprachgebrauch.....list_a.[num]),
                                    as.numeric(T3lime$Sprachgebrauch.....read_a.[num]),
                                    as.numeric(T3lime$Sprachgebrauch.....speak_a.[num])))
    data$T3EnglishFreq[i] <- mean(c(as.numeric(T3lime$Sprachgebrauch.....write_b.[num]), 
                                    as.numeric(T3lime$Sprachgebrauch.....list_b.[num]),
                                    as.numeric(T3lime$Sprachgebrauch.....read_b.[num]),
                                    as.numeric(T3lime$Sprachgebrauch.....speak_b.[num])))
    data$T3GermanFreq[i] <- mean(c(as.numeric(T3lime$Sprachgebrauch.....write_c.[num]), 
                                   as.numeric(T3lime$Sprachgebrauch.....list_c.[num]),
                                   as.numeric(T3lime$Sprachgebrauch.....read_c.[num]),
                                   as.numeric(T3lime$Sprachgebrauch.....speak_c.[num])))
    data$T3OtherFreq[i] <- mean(c(as.numeric(T3lime$Sprachgebrauch.....write_d.[num]), 
                                  as.numeric(T3lime$Sprachgebrauch.....list_d.[num]),
                                  as.numeric(T3lime$Sprachgebrauch.....read_d.[num]),
                                  as.numeric(T3lime$Sprachgebrauch.....speak_d.[num]))) 
  
    data$T3_SpanishLessonGermany[i] <- T3lime$Hast.du.seit.de...[num]
    data$T3_LivingSitGermany_Spanish[i] <- T3lime$Was.ist.deine.a....SQ003.[num]
    data$T3_KeepUpSpanishActively[i]<- T3lime$Inwiefern.stimm....SQ001.[num]
    data$T3_MaintainedSpanishContact[i]<- T3lime$Inwiefern.stimm....SQ002.[num]
    data$T3_WatchSpanishMovies[i]<- T3lime$Inwiefern.stimm....SQ003.[num]
    data$T3_ReadSpanishBooks[i]<- T3lime$Inwiefern.stimm....SQ004.[num]
    
    data$T3_SRP_Spanish_Reading[i]<- T3lime$Wie.schÃ.tzt.du....SQ001.[num]
    data$T3_SRP_Spanish_Listening[i]<- T3lime$Wie.schÃ.tzt.du....SQ002.[num]
    data$T3_SRP_Spanish_Writing[i]<- T3lime$Wie.schÃ.tzt.du....SQ003.[num]
    data$T3_SRP_Spanish_Speaking[i]<- T3lime$Wie.schÃ.tzt.du....SQ004.[num]
    data$T3_SRP_English_Reading[i]<- T3lime$Wie.schÃ.tzt.du....SQ001..1[num]
    data$T3_SRP_English_Listening[i]<- T3lime$Wie.schÃ.tzt.du....SQ002..1[num]
    data$T3_SRP_English_Writing[i]<- T3lime$Wie.schÃ.tzt.du....SQ003..1[num]
    data$T3_SRP_English_Speaking[i]<- T3lime$Wie.schÃ.tzt.du....SQ004..1[num]
    
    data$T3_EnglishInput_Native[i]<- T3lime$Zu.wie.viel.Pro....SQ001_SQ001..1[num]
    data$T3_SpanishInput_Native[i]<- T3lime$Zu.wie.viel.Pro....SQ001_SQ001.[num]
    data$T3_EnglishInput_NonNative[i]<- T3lime$Zu.wie.viel.Pro....SQ002_SQ001..1[num]
    data$T3_SpanishInput_NonNative[i]<-  T3lime$Zu.wie.viel.Pro....SQ002_SQ001.[num]
    data$T3_SpanishInput_NotGerman[i]<- T3lime$Sprachgebrauch.....SQ003_spanish.[num]}
  }

for (i in 1: nrow(data)){
  if (data$StudyYearOther[i]==""){
  } else {
    data$StudyYear[i] <- as.character(data$StudyYearOther[i])
  }
}
data$StudyYearOther <- NULL

for (i in 1: nrow(data)){
  if (data$PlaceInSpainOther[i]==""){
  } else {
    data$PlaceInSpain[i] <- as.character(data$PlaceInSpainOther[i])
  }
}
data$PlaceInSpainOther <- NULL

for (i in 1: nrow(data)){
  if (data$T1_SpanishCERF2[i]==""){
  } else {
    data$T1_SpanishCERF[i] <- as.character(data$T1_SpanishCERF2[i])
  }
}
data$T1_SpanishCERF2 <- NULL

data <- as.data.frame(lapply(data, gsub, pattern = " - sehr gut, auf Muttersprachlerniveau", replacement = "", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = " - sehr schlecht", replacement = "", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "mÃ¤nnlich", replacement = "m", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "weiblich", replacement = "f", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = " (sehr schlecht)", replacement = "", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Spreche ich nicht", replacement = "NA", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Ich habe bisher keinen Spanischkurs besucht", replacement = "NA", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Neutral", replacement = "4", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Sehr gut / vollkommen zufrieden", replacement = "7", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "1 - stimme Ã¼berhaupt nicht zu", replacement = "1", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Sehr schlecht / total unzufrieden", replacement = "1", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Sehr gut / vollkommen zufrieden", replacement = "7", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Ã¼berhaupt nicht integriert", replacement = "1", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "4 - neutral", replacement = "4", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "vollstÃ¤ndig integriert", replacement = "7", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Nein", replacement = "0", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "Ja", replacement = "1", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "neutral", replacement = "4", fixed = TRUE))
data <- as.data.frame(lapply(data, gsub, pattern = "7 - stimme absolut zu", replacement = "7", fixed = TRUE))

cols.num <- c(2,3,7:15,17:109)
data[cols.num] <- sapply(data[cols.num],as.character)
data[cols.num] <- sapply(data[cols.num],as.numeric)
data$GermanEngRatio <- data$T1EnglishFreq/data$T1GermanFreq

# fill in missing values for the language use with natives question
for (i in 1:nrow(data)){
  if (is.na(data$T2_EnglishInput_Native[i])==1 && is.na(data$T2_EnglishInput_NonNative[i]) ==0 && data$T2_EnglishInput_NonNative[i] == 0){
    data$T2_EnglishInput_Native[i] <- 100 
  } else if (is.na(data$T2_EnglishInput_Native[i])==1 && is.na(data$T2_EnglishInput_NonNative[i]) ==0 &&  data$T2_EnglishInput_NonNative[i] == 100){
    data$T2_EnglishInput_Native[i] <- 0 
  } else if (is.na(data$T2_EnglishInput_Native[i])==1 && is.na(data$T2_EnglishInput_NonNative[i]) ==1){
    #data$T2_EnglishInput_Native[i] <- 0 
    #data$T2_EnglishInput_NonNative[i] <- 0 
  }
}

for (i in 1:nrow(data)){
  if (is.na(data$T2_SpanishInput_Native[i])==1 && is.na(data$T2_SpanishInput_NonNative[i]) ==0 && data$T2_SpanishInput_NonNative[i] == 0){
    data$T2_SpanishInput_Native[i] <- 100 
  } else if (is.na(data$T2_SpanishInput_Native[i])==1 && is.na(data$T2_SpanishInput_NonNative[i]) ==0 &&  data$T2_SpanishInput_NonNative[i] == 100){
    data$T2_SpanishInput_Native[i] <- 0 
  } else if (is.na(data$T2_SpanishInput_Native[i])==1 && is.na(data$T2_SpanishInput_NonNative[i]) ==1){
    #data$T2_SpanishInput_Native[i] <- 0 
    #data$T2_SpanishInput_NonNative[i] <- 0 
  }
}

for (i in 1:nrow(data)){
  if (is.na(data$T3_EnglishInput_Native[i])==1 && is.na(data$T3_EnglishInput_NonNative[i]) ==0 && data$T3_EnglishInput_NonNative[i] == 0){
    data$T3_EnglishInput_Native[i] <- 100 
  } else if (is.na(data$T3_EnglishInput_Native[i])==1 && is.na(data$T3_EnglishInput_NonNative[i]) ==0 &&  data$T3_EnglishInput_NonNative[i] == 100){
    data$T3_EnglishInput_Native[i] <- 0 
  } else if (is.na(data$T3_EnglishInput_Native[i])==1 && is.na(data$T3_EnglishInput_NonNative[i]) ==1){
    #data$T3_EnglishInput_Native[i] <- 0 
    #data$T3_EnglishInput_NonNative[i] <- 0 
  }
}

for (i in 1:nrow(data)){
  if (is.na(data$T3_SpanishInput_Native[i])==1 && is.na(data$T3_SpanishInput_NonNative[i]) ==0 && data$T3_SpanishInput_NonNative[i] == 0){
    data$T3_SpanishInput_Native[i] <- 100 
  } else if (is.na(data$T3_SpanishInput_Native[i])==1 && is.na(data$T3_SpanishInput_NonNative[i]) ==0 &&  data$T3_SpanishInput_NonNative[i] == 100){
    data$T3_SpanishInput_Native[i] <- 0 
  } else if (is.na(data$T3_SpanishInput_Native[i])==1 && is.na(data$T3_SpanishInput_NonNative[i]) ==1){
    #data$T3_SpanishInput_Native[i] <- 0 
    #data$T3_SpanishInput_NonNative[i] <- 0 
  }
}

# calculate averages for self-rated proficiency measures
for (i in 1: nrow(data)){
  data$T1_SRP_English_avg[i] <- mean(c(data$T1_SRP_English_Listening[i], data$T1_SRP_English_Reading[i], data$T1_SRP_English_Speaking[i], data$T1_SRP_English_Writing[i]))
  data$T1_SRP_Spanish_avg[i] <- mean(c(data$T1_SRP_Spanish_Listening[i], data$T1_SRP_Spanish_Reading[i], data$T1_SRP_Spanish_Speaking[i], data$T1_SRP_Spanish_Writing[i]))
  data$T2_SRP_English_avg[i] <- mean(c(data$T2_SRP_English_Listening[i], data$T2_SRP_English_Reading[i], data$T2_SRP_English_Speaking[i], data$T2_SRP_English_Writing[i]))
  data$T2_SRP_Spanish_avg[i] <- mean(c(data$T2_SRP_Spanish_Listening[i], data$T2_SRP_Spanish_Reading[i], data$T2_SRP_Spanish_Speaking[i], data$T2_SRP_Spanish_Writing[i]))
  data$T3_SRP_English_avg[i] <- mean(c(data$T3_SRP_English_Listening[i], data$T3_SRP_English_Reading[i], data$T3_SRP_English_Speaking[i], data$T3_SRP_English_Writing[i]))
  data$T3_SRP_Spanish_avg[i] <- mean(c(data$T3_SRP_Spanish_Listening[i], data$T3_SRP_Spanish_Reading[i], data$T3_SRP_Spanish_Speaking[i], data$T3_SRP_Spanish_Writing[i]))
  
  }

setwd("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding")
write.table(data, "T1_T2_T3_lime_clean.txt", sep = "\t", quote = F, row.names = F)

### subset to numerical variables only and correlate them 
#corrmatdetail <- data[,c(2,3,7:15,27:38,43:71,73)]
corrmatdetail <- data[,c(2,3,7:15,27:103, 109:116)]
corrmatdetail <- corrmatdetail[-which(is.na(corrmatdetail$T2GermanFreq)==1),]
corrdetail <- round(cor(corrmatdetail, use = "complete.obs"), 1)
ggcorrplot(corrdetail) +
  theme(axis.text.x = element_text(size= 5),
        axis.text.y = element_text(size= 5))

corrmat <- data[,c(2,3,7,23:26,39:42,55,59,60,70,86:95,104,105,109:123,129,135,141)]
corrmat <- corrmat[-which(is.na(corrmat$T2GermanFreq)==1),]
corr <- round(cor(corrmat, use = "complete.obs"), 1)
ggcorrplot(corr) +
  theme(axis.text.x = element_text(size= 10, angle = 90),
        axis.text.y = element_text(size= 10, angle = 0))
  
