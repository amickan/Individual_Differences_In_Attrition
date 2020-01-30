### Script to send emails to participants ### 

### Load required packages
require(mailR)
require(gdata)
require(tidyr)

##########################################################################
####################### DOWNLOADING THE DATABASES ########################
##########################################################################

### Read in the local participant database
setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
reg <- read.delim("REGISTRATION.txt", header = T, stringsAsFactors = F)

### Download and read in the latest sign-up sheet
# download and convert to txt (xls loading does not work for some reason)
setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION/SignUp_download")
files <- file.info(list.files(getwd(),pattern = "txt"))
signup <- read.delim(rownames(files)[order(files$mtime)][nrow(files)], stringsAsFactors = F)

### Download the latest Limesurvey token list and read it in 
setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
filescsv <- file.info(list.files(getwd(),pattern = "csv"))
limelist <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)

### Download the latest Limesurvey token list from inbetween questionnaire
setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION/Limesurvey_download_2")
filescsv <- file.info(list.files(getwd(),pattern = "csv"))
limelist2 <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)

##########################################################################
######################### UPDATING THE DATABASES #########################
##########################################################################
signup$Startdatum <- as.character(as.Date(as.character(signup$Startdatum),format = "%d-%m-%Y")) #format = "%d-%m-%Y
signup$Enddatum <- as.character(as.Date(as.character(signup$Enddatum),format = "%d-%m-%Y"))

### Add new participants to the database (if there are new entries)
if (nrow(subset(signup, !(signup$Id %in% reg$tid))) != 0) {
  #if (length(signup[,1]) != length(reg[,1])) {
  new <- subset(signup, !(signup$Id %in% reg$tid))
  k <- length(reg[,1])
  reg[(k+1):(k+length(new[,1])),] <- NA
  for (i in 1: length(new[,1])){
    k <- k+1
    reg$tid[k] <- new$Id[i]
    reg$firstname[k] <- new$Vorname[i]
    reg$lastname[k] <- new$Nachname[i]
    reg$Geburtsdatum[k] <- new$Geburtsdatum[i]
    reg$Geschlecht[k] <- new$Geschlecht[i]
    reg$Dauer[k] <- new$Dauer[i]
    reg$email[k] <- new$Email[i]
    reg$Startdatum[k] <- as.character(new$Startdatum[i])
    reg$Enddatum[k] <- as.character(new$Enddatum[i])
  }
}

### Create tokens for new people
for (i in 1:length(reg[,1])){
  # first sort dataframe by token, as to make sure the highest token is last 
  reg <- reg[order(reg$token),]
  if (is.na(reg$token[i])==1 && is.na(reg$token2[i])==1){
    #reg$token[i] <- as.numeric(as.numeric(reg$token[i-1])+1)
    #reg$token2[i] <- paste(reg$token[i],"_2", sep="")
    #reg$token3[i] <- paste(reg$token[i],"_3", sep="")
    #reg$token4[i] <- paste(reg$token[i],"_4", sep="")
    #reg$token5[i] <- paste(reg$token[i],"_5", sep="")
    #reg$token6[i] <- paste(reg$token[i],"_6", sep="")
    #reg$token7[i] <- paste(reg$token[i],"_7", sep="")
    reg$token8[i] <- paste(reg$token[i],"_8", sep="")
    reg$token9[i] <- paste(reg$token[i],"_9", sep="")
    reg$token10[i] <- paste(reg$token[i],"_10", sep="")
    reg$token11[i] <- paste(reg$token[i],"_11", sep="")
    reg$token12[i] <- paste(reg$token[i],"_12", sep="")
  } else if (is.na(reg$token8[i])==1){
    #reg$token2[i] <- paste(reg$token[i],"_2", sep="")
    #reg$token3[i] <- paste(reg$token[i],"_3", sep="")
    #reg$token4[i] <- paste(reg$token[i],"_4", sep="")
    #reg$token5[i] <- paste(reg$token[i],"_5", sep="")
    #reg$token6[i] <- paste(reg$token[i],"_6", sep="")
    #reg$token7[i] <- paste(reg$token[i],"_7", sep="")
    reg$token8[i] <- paste(reg$token[i],"_8", sep="")
    reg$token9[i] <- paste(reg$token[i],"_9", sep="")
    reg$token10[i] <- paste(reg$token[i],"_10", sep="")
    reg$token11[i] <- paste(reg$token[i],"_11", sep="")
    reg$token12[i] <- paste(reg$token[i],"_12", sep="")
  }
}

### Fill in the missing values for the new people 
# number of between session questionaires
reg$Startdatum <- as.Date(as.character(reg$Startdatum)) #format = "%d-%m-%Y
reg$Enddatum <- as.Date(as.character(reg$Enddatum))
  
reg$BetweenQs <- floor(difftime(reg$Enddatum,reg$Startdatum , units = c("weeks"))/4.5) # this is conservative and ruonds down in all possible instances
# time for T2 
reg$T2Datum <- as.Date(reg$Enddatum)-10
# time for T3
reg$T3Datum <- as.Date(reg$Enddatum)+180

# fill reminder count for new items with 0 
for (i in 1:length(reg[,1])){
  if (is.na(reg$ReminderCount[i])==1){
    reg$ReminderCount[i] <- 0 
  }}

# create links for new participants 
for (i in 1:length(reg[,1])){
  if (is.na(reg$LimeLink8[i])==1) {#| is.na(reg$LimeLink2[i])==1 | is.na(reg$LimeLink3[i])==1 | is.na(reg$LimeLink4[i])==1 | is.na(reg$LimeLink5[i])==1 | is.na(reg$LimeLink6[i])==1 | is.na(reg$LimeLink7[i])==1 | is.na(reg$PicLink1[i])==1 | is.na(reg$FluencyLink1[i])==1){
    #reg$LimeLink1[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/422243/token/",reg$token[i] ,"/lang/de", sep = "") 
    #reg$LimeLink2[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token2[i], "/lang/de", sep="")
    #reg$LimeLink3[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token3[i], "/lang/de", sep="")
    #reg$LimeLink4[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token4[i], "/lang/de", sep="")
    #reg$LimeLink5[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token5[i], "/lang/de", sep="")
    #reg$LimeLink6[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token6[i], "/lang/de", sep="")
    #reg$LimeLink7[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token7[i], "/lang/de", sep="")
    reg$LimeLink8[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token8[i], "/lang/de", sep="")
    reg$LimeLink9[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token9[i], "/lang/de", sep="")
    reg$LimeLink10[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token10[i], "/lang/de", sep="")
    reg$LimeLink11[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token11[i], "/lang/de", sep="")
    reg$LimeLink12[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token12[i], "/lang/de", sep="")
    #reg$PicLink1[i] <- paste("https://exp.socsci.ru.nl/spanish/naming.html?ppn=", reg$token[i], sep="")
    #reg$FluencyLink1[i] <- paste("https://exp.socsci.ru.nl/spanish/fluency2.html?ppn=", reg$token[i], sep="")
  }}

### Update database with info from limesurvey and vice versa
if (nrow(subset(reg, !(reg$token %in% limelist$token))) != 0) {
  new <- subset(reg, !(reg$token %in% limelist$token))
  k <- length(limelist[,1])
  limelist[(k+1):(k+length(new[,1])),] <- NA
  for (i in 1:length(new[,1])){
    k <- k+1
    limelist$token[k] <- new$token[i]
    limelist$firstname[k] <- new$firstname[i]
    limelist$lastname[k] <- new$lastname[i]
    limelist$email[k] <- new$email[i]
    limelist$ï..tid[k] <- new$tid[i]
    limelist$completed[k] <- "N"
    limelist$usesleft[k] <- 1
  }
}

for (j in 1:length(limelist[,1])){
  if (limelist$completed[j]!="N"){
  l <- which(tolower(as.character(reg$token)) == tolower(as.character(limelist$token[j])))
  reg$completedLime1[l] <- 1}
}

## Check whether tasks have been completed
setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION/Logfiles")
filescsv <- file.info(list.files(getwd(),pattern = "csv"))
comptasks <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)
pic <- comptasks[grep("naming",comptasks$fileName), ]
ppspic <- unique(pic[pic$imgIndex == 143,]$ppn)
ppspic<-ppspic[!is.na(ppspic)]
flu <- comptasks[grep("fluency",comptasks$fileName), ]
ppsflu <- unique(flu[flu$trial_index == 20,]$ppn)
ppsflu<-ppsflu[!is.na(ppsflu)]

for (i in 1:length(ppsflu)){
  num <- which(tolower(as.character(reg$token)) == tolower(as.character(ppsflu[i])))
  reg$completedFluency1[num] <- 1
}

for (i in 1:length(ppspic)){
  num <- which(tolower(as.character(reg$token)) == tolower(as.character(ppspic[i])))
  reg$completedPic1[num] <- 1
}

#files <- list.files("U:/PhD/EXPERIMENT 4/ADMINISTRATION/Logfiles")
#completednums <- matrix(NA,length(files),2)
#for (i in 1:length(files)){
#  tem <- files[i]
#  completednums[i,1] <- gsub("\\d.*_\\d.*_(\\d.*?)_.*", "\\1", tem[1])
#  completednums[i,2] <- gsub("\\d.*_\\d.*_\\d.*?_(\\w.*?)_.*", "\\1", tem[1])
#}
#completednums <- as.data.frame(completednums)
#colnames(completednums) <- c("PP","Task")

#for (i in 1:nrow(completednums)){
#  num <- which(tolower(as.character(reg$token)) == tolower(as.character(completednums$PP[i])))
#  if (completednums$Task[i]=="fluency"){
#    reg$completedFluency1[num] <- 1
#  } else if (completednums$Task[i]=="pic-naming"){
#    reg$completedPic1[num] <- 1
#  }
#}

## Check whether T1 has been completed
for (i in 1:length(reg[,1])){
  if (is.na(reg$completedLime1[i])==0 && is.na(reg$completedPic1[i])==0 && is.na(reg$completedFluency1[i])==0){
    reg$T1done[i] <- 1
  }}

## Check whether in between questionnaires have been filled in 
for (i in 1:nrow(limelist2)){
  if (limelist2$completed[i] != "N"){
    m <- which(tolower(as.character(reg$token)) == gsub("(\\d.*)_\\d","\\1", tolower(as.character(limelist2$token[i]))))
    if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 1){
      reg$completedLime2[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 2){
      reg$completedLime3[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 3){
      reg$completedLime4[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 4){
      reg$completedLime5[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 5){
      reg$completedLime6[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 6){
      reg$completedLime7[m] <- 1}
  }
}


####################################################################
####################### Send emails ################################
####################################################################

########
## T1 ##
########

##### T1 - First invitation (i.e. email with all links for T1) #####
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if (is.na(reg$Invited[i]) == 1){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp) != 0) {
for (i in 1:length(temp[,1])){
  html_body <- paste("<html> Liebe(r)",
              as.character(temp$firstname[i]),
              ", <br /><p> Vielen Dank, dass du an unserer Studie teilnimmst! Wir w?nschen dir viel Spa? und Erfolg beim Erledigen der Aufgaben. Bei Fragen oder technischen Problemen, melde dich einfach bei mir: anne.mickan@mpi.nl </p> 
              <p>Unten findest du die Links zu den Aufgaben f?r den ersten Teil der Studie. Bitte arbeite dich von <b>oben nach unten, also der Reihenfolge nach, durch die Links</b>. Das ist f?r uns sehr wichtig, wir k?nnen deine Daten sonst nicht verwenden.</p>
              <p>Insgesamt hast du nun <b> bis Freitag (05.10.2018) Zeit daf?r</b>. Wir m?chten dich jedoch bitten, <b>die Aufgaben m?glichst bald und auf einmal</b>, also in einer Sitzung, zu erledigen. Erkl?rungen zu den jeweiligen Aufgaben folgen ?ber die Links. Wichtig ist au?erdem, dass du alle Aufgaben vollst?ndig erledigst. <u> Wir k?nnen dich n?mlich leider nicht bezahlen, wenn dies nicht der Fall ist.</u></p>  
              <p>Bevor du anf?ngst, stelle bitte sicher, dass:</p> 
              <ul> 
              <li> du in einem ruhigen Raum sitzt, wo du ungest?rt und allein arbeiten kannst und es keine starken Hintergrundger?usche gibt.</li>
              <li> du <b>die Aufgaben auf einem Computer erledigst.</b> Auf dem Handy/Tablet ist nicht garantiert, dass die Aufgaben funktionieren.</li>
              <li> du alle Aufgaben im <b>gleichen Browser</b> erledigst (<b>FIREFOX oder CHROME</b>). Wenn es ein technisches Problem gibt, kannst du nach Aktualisierung der Webseite bei manchen Aufgaben dort weitermachen, wo du aufgeh?rt hast. Das geht nicht, wenn du zwischendurch den Browser wechselst. <u><b>ACHTUNG: Safari und Internet Exlporer funktionieren nicht.</b></u></li>
              <li> du eine stabile Internetverbindung hast. Wenn deine Internetverbindung bei den Aufgaben abbricht, kann es (je nach Aufgabe) sein, dass deine Daten verloren gehen bzw. unvollst?ndig gespeichert werden.</li>
              <li> <b>dein Computer ein integriertes Mikrofon hat</b> (oder du ein Mikrofon angeschlossen hast). Die letzten beiden Aufgaben erfordern gesprochene Antworten.</li>
              <li> Um dein Mikrofon vorab auf Lautst?rke und Funktionalit?t zu testen, schau hier vorbei: https://www.speakpipe.com/voice-recorder Trouble-shooting Tips findest du notfalls hier: https://www.onlinemictest.com/ </i></li>              
              </ul><b>Hier die Links (<b>unbedingt die Reihenfolge einhalten!</b>):</b> <br / ><p>1. <u>Einverst?ndniserkl?rung</u> (ACHTUNG: nach dem Ausf?llen wird dir ein Umleitungsfehler angezeigt. Das ist normal, deine Daten werden trotzdem gespeichert!):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
              <p>2. <u>Fragebogen (ca. 15 min):</u><br />",
              temp$LimeLink1[i], 
              "</p><p>3. <u>Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
              temp$PicLink1[i],
              "</p><p>4. <u>Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
              temp$FluencyLink1[i],
              "</p><p>Viel Spa? und beste Gr??e,<br />
              Anne Mickan</p><br /><br />
              (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
              </html>")
  mail <- as.character(temp$email[i])
  send.mail(
          from = "anne.mickan@mpi.nl"  ,    
          to = c(mail, "anne.mickan@mpi.nl"),
          #bcc = "anne.mickan@mpi.nl",
          subject = "[Max Planck Studie] Links f?r Teil 1",
          html = TRUE,
          body = html_body,
          smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "*****", tls = TRUE),
          authenticate = TRUE,
          send = TRUE,
          debug = F)
  print(paste(i,". email sent to: ",as.character(temp$email[i]), sep = ""))
  # mark that invitation email has been sent
  num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
  reg$Invited[num] <- 1
  reg$InvitedDate[num] <- as.character(as.Date(Sys.Date()))
}
} else if (length(temp) == 0) {
  print("Keine neuen Einladungen versandt. Alle Leute in der Datenbank haben bereits eine Einladung erhalten.")
}

###### Reminder for T1 session #####
## this is only checked 4, 7 and 10 days after initial invitation
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if (is.na(reg$Invited[i]) != 1 && is.na(reg$T1done[i]) == 1 &&
      (is.na(reg$completedLime1[i]) == 1 | is.na(reg$completedPic1[i]) == 1 | is.na(reg$completedFluency1[i]) == 1) &&
      ((reg$ReminderCount[i] == 0 && (reg$InvitedDate[i] == (Sys.Date()-4) | reg$InvitedDate[i] == (Sys.Date()-6) | reg$InvitedDate[i] == (Sys.Date()-5) )) || 
      (reg$ReminderCount[i] == 1 && (reg$InvitedDate[i] == (Sys.Date()-8) | reg$InvitedDate[i] == (Sys.Date()-9) | reg$InvitedDate[i] == (Sys.Date()-10)))) #||
      #(reg$ReminderCount[i] == 2 && (reg$InvitedDate[i] == (Sys.Date()-12) | reg$InvitedDate[i] == (Sys.Date()-13) | reg$InvitedDate[i] == (Sys.Date()-14)))
      ){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp) != 0) {
for (i in 1:length(temp[,1])){
  if (is.na(temp$completedLime1[i]) == T && is.na(temp$completedPic1[i]) == T && is.na(temp$completedFluency1[i]) == T){
    html_body <- paste("<html> Liebe(r)",
              as.character(temp$firstname[i]),
              ", <br /><p>Dies ist eine kurze Erinnerung, am ersten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
              <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein K?stchen, mit oder ohne H?kchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, wird das durch ein H?kchen neben der entsprechenden Aufgabe angezeigt. Dadurch wei?t du also welche Aufgaben du noch erledigen (oder eventuell vervollst?ndigen) musst.</u></p>  
              <p>Solltest du der Meinung sein, eine Aufgabe wohl vollst?ndig erledigt zu haben, diese aber kein H?kchen hat, melde dich bitte bei mir.</p> 
              <p>Hier au?erdem auch noch einmal die Erinnerung an Folgendes:</p> 
              <ul> 
              <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
              <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>, wie unten aufgef?hrt!</li>
              <li> F?r die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
              </b> <p><u>1. Einverst?ndniserkl?rung</u> (Hierauf haben wir nur bedingt Zugriff, nur du kannst also wissen, ob du diese bereits ausgef?llt hast. Im Zweifelsfall einfach noch mal ausf?llen.):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
              <p>&#9744; <u> 2. Fragebogen (ca. 15 min):</u><br />",
              temp$LimeLink1[i], 
              "</p><p>&#9744; <u> 3. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
              temp$PicLink1[i],
              "</p><p>&#9744; <u> 4. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
             temp$FluencyLink1[i],
              "</p><p>Viel Spa? und beste Gr??e,<br />
              Anne Mickan</p>
             <br /><br />
              (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
             </html>")}
  else if (is.na(temp$completedLime1[i]) == F && is.na(temp$completedPic1[i]) == T && is.na(temp$completedFluency1[i]) == T){
    html_body <- paste("<html> Liebe(r)",
              as.character(temp$firstname[i]),
              ", <br /><p>Dies ist eine kurze Erinnerung, am ersten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
              <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein K?stchen, mit oder ohne H?kchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, wird das durch ein H?kchen neben der entsprechenden Aufgabe angezeigt. Dadurch wei?t du also welche Aufgaben du noch erledigen (oder eventuell vervollst?ndigen) musst.</u></p>  
              <p>Solltest du der Meinung sein, eine Aufgabe wohl vollst?ndig erledigt zu haben, diese aber kein H?kchen hat, melde dich bitte bei mir.</p> 
              <p>Hier au?erdem auch noch einmal die Erinnerung an Folgendes:</p> 
              <ul> 
              <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
              <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>, wie unten aufgef?hrt!</li>
              <li> F?r die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
              </b> <p><u>1. Einverst?ndniserkl?rung</u> (Hierauf haben wir nur bedingt Zugriff, nur du kannst also wissen, ob du diese bereits ausgef?llt hast. Im Zweifelsfall einfach noch mal ausf?llen.):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
              <p>&#9745; <u> 2. Fragebogen (ca. 15 min):</u><br />",
                       temp$LimeLink1[i], 
                       "</p><p>&#9744; <u> 3. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                       temp$PicLink1[i],
                       "</p><p>&#9744; <u> 4. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                       temp$FluencyLink1[i],
                       "</p><p>Viel Spa? und beste Gr??e,<br />
              Anne Mickan</p>
              <br /><br />
              (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
              </html>")}
  else if (is.na(temp$completedLime1[i]) == F && is.na(temp$completedPic1[i]) == F && is.na(temp$completedFluency1[i]) == T){
    html_body <- paste("<html> Liebe(r)",
              as.character(temp$firstname[i]),
              ", <br /><p>Dies ist eine kurze Erinnerung, am ersten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
              <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein K?stchen, mit oder ohne H?kchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, wird das durch ein H?kchen neben der entsprechenden Aufgabe angezeigt. Dadurch wei?t du also welche Aufgaben du noch erledigen (oder eventuell vervollst?ndigen) musst.</u></p>  
              <p>Solltest du der Meinung sein, eine Aufgabe wohl vollst?ndig erledigt zu haben, diese aber kein H?kchen hat, melde dich bitte bei mir.</p> 
              <p>Hier au?erdem auch noch einmal die Erinnerung an Folgendes:</p> 
              <ul> 
              <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
              <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>, wie unten aufgef?hrt!</li>
              <li> F?r die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
              </b> <p><u>1. Einverst?ndniserkl?rung</u> (Hierauf haben wir nur bedingt Zugriff, nur du kannst also wissen, ob du diese bereits ausgef?llt hast. Im Zweifelsfall einfach noch mal ausf?llen.):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
              <p>&#9745; <u> 2. Fragebogen (ca. 15 min):</u><br />",
              temp$LimeLink1[i], 
              "</p><p>&#9745; <u> 3. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
              temp$PicLink1[i],
              "</p><p>&#9744; <u> 4. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
              temp$FluencyLink1[i],
              "</p><p>Viel Spa? und beste Gr??e,<br />
              Anne Mickan</p>
              <br /><br />
              (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
              </html>")
  } else if (is.na(temp$completedLime1[i]) == F && is.na(temp$completedPic1[i]) == T && is.na(temp$completedFluency1[i]) == F){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Dies ist eine kurze Erinnerung, am ersten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
              <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein K?stchen, mit oder ohne H?kchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, wird das durch ein H?kchen neben der entsprechenden Aufgabe angezeigt. Dadurch wei?t du also welche Aufgaben du noch erledigen (oder eventuell vervollst?ndigen) musst.</u></p>  
              <p>Solltest du der Meinung sein, eine Aufgabe wohl vollst?ndig erledigt zu haben, diese aber kein H?kchen hat, melde dich bitte bei mir.</p> 
              <p>Hier au?erdem auch noch einmal die Erinnerung an Folgendes:</p> 
              <ul> 
              <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
              <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>, wie unten aufgef?hrt!</li>
              <li> F?r die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
              </b> <p><u>1. Einverst?ndniserkl?rung</u> (Hierauf haben wir nur bedingt Zugriff, nur du kannst also wissen, ob du diese bereits ausgef?llt hast. Im Zweifelsfall einfach noch mal ausf?llen.):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
              <p>&#9745; <u> 2. Fragebogen (ca. 15 min):</u><br />",
                       temp$LimeLink1[i], 
                       "</p><p>&#9744; <u> 3. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                       temp$PicLink1[i],
                       "</p><p>&#9745; <u> 4. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                       temp$FluencyLink1[i],
                       "</p><p>Viel Spa? und beste Gr??e,<br />
              Anne Mickan</p>
              <br /><br />
              (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
              </html>")
  } else if (is.na(temp$completedLime1[i]) == T && is.na(temp$completedPic1[i]) == F && is.na(temp$completedFluency1[i]) == F){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Dies ist eine kurze Erinnerung, am ersten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
              <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein K?stchen, mit oder ohne H?kchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, wird das durch ein H?kchen neben der entsprechenden Aufgabe angezeigt. Dadurch wei?t du also welche Aufgaben du noch erledigen (oder eventuell vervollst?ndigen) musst.</u></p>  
              <p>Solltest du der Meinung sein, eine Aufgabe wohl vollst?ndig erledigt zu haben, diese aber kein H?kchen hat, melde dich bitte bei mir.</p> 
              <p>Hier au?erdem auch noch einmal die Erinnerung an Folgendes:</p> 
              <ul> 
              <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
              <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>, wie unten aufgef?hrt!</li>
              <li> F?r die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
              </b> <p><u>1. Einverst?ndniserkl?rung</u> (Hierauf haben wir nur bedingt Zugriff, nur du kannst also wissen, ob du diese bereits ausgef?llt hast. Im Zweifelsfall einfach noch mal ausf?llen.):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
              <p>&#9744; <u> 2. Fragebogen (ca. 15 min):</u><br />",
                       temp$LimeLink1[i], 
                       "</p><p>&#9745; <u> 3. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                       temp$PicLink1[i],
                       "</p><p>&#9745; <u> 4. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                       temp$FluencyLink1[i],
                       "</p><p>Viel Spa? und beste Gr??e,<br />
              Anne Mickan</p>
              <br /><br />
              (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
              </html>")
  } else if (is.na(temp$completedLime1[i]) == T && is.na(temp$completedPic1[i]) == T && is.na(temp$completedFluency1[i]) == F){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Dies ist eine kurze Erinnerung, am ersten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                       <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein K?stchen, mit oder ohne H?kchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, wird das durch ein H?kchen neben der entsprechenden Aufgabe angezeigt. Dadurch wei?t du also welche Aufgaben du noch erledigen (oder eventuell vervollst?ndigen) musst.</u></p>  
                       <p>Solltest du der Meinung sein, eine Aufgabe wohl vollst?ndig erledigt zu haben, diese aber kein H?kchen hat, melde dich bitte bei mir.</p> 
                       <p>Hier au?erdem auch noch einmal die Erinnerung an Folgendes:</p> 
                       <ul> 
                       <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                       <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>, wie unten aufgef?hrt!</li>
                       <li> F?r die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                       </b> <p><u>1. Einverst?ndniserkl?rung</u> (Hierauf haben wir nur bedingt Zugriff, nur du kannst also wissen, ob du diese bereits ausgef?llt hast. Im Zweifelsfall einfach noch mal ausf?llen.):<br /> https://survey.socsci.ru.nl/index.php/671975/lang-de <br /></p> 
                       <p>&#9744; <u> 2. Fragebogen (ca. 15 min):</u><br />",
                       temp$LimeLink1[i], 
                       "</p><p>&#9744; <u> 3. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                       temp$PicLink1[i],
                       "</p><p>&#9745; <u> 4. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                       temp$FluencyLink1[i],
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
  } else if (length(temp) == 0) {
    print("Keine neuen Erinnerungen versandt. Alle Leute in der Datenbank sind entweder erinnert worden oder haben die Aufgaben erledigt.")
  }
  mail <- as.character(temp$email[i])
  out <- send.mail(
    from = "anne.mickan@mpi.nl"  ,    
    to = c(mail, "anne.mickan@mpi.nl"),
    #bcc = "anne.mickan@mpi.nl",
    subject = "[Max Planck Studie] Erinnerung zur Teilnahme an Teil 1",
    html = TRUE,
    body = html_body,
    smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "******", tls = TRUE),
    authenticate = TRUE,
    send = TRUE,
    debug = F)
  print(paste(i,". reminder sent to: ",as.character(temp$email[i]), sep = ""))
  # mark that invitation email has been sent
  num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
  reg$ReminderCount[num] <- reg$ReminderCount[num] + 1
}
}

#######################
## BETWEEN T1 and T2 ##
#######################

#### Invitation for in between emails ####
# mail 1 refers to the date of the first in between email invitation
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if (reg$T1done[i] == 1 &&
       ((is.na(reg$Mail1Sent[i]) == 1) &&
            ((reg$compLime1[i] == (Sys.Date()-31) | 
            reg$compLime1[i] == (Sys.Date()-29) | 
            reg$compLime1[i] == (Sys.Date()-30)))) ||
       ((is.na(reg$Mail2Sent[i]) == 1) &&
            ((reg$compLime1[i] == (Sys.Date()-59) | 
            reg$compLime1[i] == (Sys.Date()-61) | 
            reg$compLime1[i] == (Sys.Date()-60)))) || 
      ((is.na(reg$Mail3Sent[i]) == 1) &&
            ((reg$compLime1[i] == (Sys.Date()-91) | 
            reg$compLime1[i] == (Sys.Date()-89) | 
            reg$compLime1[i] == (Sys.Date()-90)))) ||
      ((is.na(reg$Mail4Sent[i]) == 1) &&
            ((reg$compLime1[i] == (Sys.Date()-121) | 
            reg$compLime1[i] == (Sys.Date()-119) | 
            reg$compLime1[i] == (Sys.Date()-120)))) ||
      ((is.na(reg$Mail5Sent[i]) == 1) &&
            ((reg$compLime1[i] == (Sys.Date()-151) | 
            reg$compLime1[i] == (Sys.Date()-149) | 
            reg$compLime1[i] == (Sys.Date()-150)))) ||
      ((is.na(reg$Mail6Sent[i]) == 1) &&
            ((reg$compLime1[i] == (Sys.Date()-181) |
            reg$compLime1[i] == (Sys.Date()-179) |
            reg$compLime1[i] == (Sys.Date()-180))))){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

for (i in 1:length(temp[,1])){
  num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
  if (is.na(temp$Mail1Sent[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Es ist nun ca. einen Monat her, dass du den ersten Teil der Studie ausgef?llt hast.</p> 
                       <p>Wir m?chten dich nun bitten, eine kurze Frage zu deinem <b>aktuellen</b> Sprachgebrauch zu beantworten (die erste von insgesamt",
                       as.character(temp$BetweenQs[i]), " solcher Emails ?ber die n?chsten Monate.</p>
                       <p>Es sollte dich nicht mehr als 2 Minuten kosten, und sollte m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink2[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail1Sent[num] <- 1}
  else if (is.na(temp$Mail2Sent[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es gut.</p> 
                       <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem <b>aktuellen</b> Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink3[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail2Sent[num] <- 1}
  else if (is.na(temp$Mail3Sent[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Und wieder ist ein Monat vergangen. Wir hoffen, dir geht es gut.</p> 
                       <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink4[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail3Sent[num] <- 1}
  else if (is.na(temp$Mail4Sent[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es gut.</p> 
                       <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink5[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail4Sent[num] <- 1}
  else if (is.na(temp$Mail5Sent[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                       <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink6[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail5Sent[num] <- 1}
  else if (is.na(temp$Mail6Sent[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                       <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink7[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p><br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail6Sent[num] <- 1}
  mail <- as.character(temp$email[i])
  out <- send.mail(
    from = "anne.mickan@mpi.nl"  ,    
    to = c(mail, "anne.mickan@mpi.nl"),
    #bcc = "anne.mickan@mpi.nl",
    subject = "[Max Planck Studie] Kurze Frage zu deinem aktuellen Sprachgebrauch (dauert nur 2 Minuten!)",
    html = TRUE,
    body = html_body,
    smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "*****", tls = TRUE),
    authenticate = TRUE,
    send = TRUE,
    debug = TRUE)
  print(paste(i,". email sent to: ",as.character(temp$email[i]), sep = ""))
}

## Reminders to fill in the inbetween questionnaire 
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if ((is.na(reg$Mail1Sent[i] != 1) &&
       is.na(reg$completedLime2) == 1 &&
       is.na(reg$Mail1Rem) == 1 &&
      (reg$Mail1Sent[i] == (Sys.Date()-3) | 
         reg$Mail1Sent[i] == (Sys.Date()-4) | 
         reg$Mail1Sent[i] == (Sys.Date()-5))) ||
      (is.na(reg$Mail2Sent[i] != 1) &&
       is.na(reg$completedLime3) == 1 &&
       is.na(reg$Mail2Rem) == 1 &&
      (reg$Mail2Sent[i] == (Sys.Date()-3) | 
         reg$Mail2Sent[i] == (Sys.Date()-4) | 
         reg$Mail2Sent[i] == (Sys.Date()-5))) || 
      (is.na(reg$Mail3Sent[i] != 1) &&
       is.na(reg$completedLime4) == 1 &&
       is.na(reg$Mail3Rem) == 1 &&
      (reg$Mail3Sent[i] == (Sys.Date()-3) | 
         reg$Mail3Sent[i] == (Sys.Date()-4) | 
         reg$Mail3Sent[i] == (Sys.Date()-5))) ||
      (is.na(reg$Mail4Sent[i] != 1) &&
       is.na(reg$completedLime5) == 1 &&
       is.na(reg$Mail4Rem) == 1 &&
       (reg$Mail4Sent[i] == (Sys.Date()-3) | 
         reg$Mail4Sent[i] == (Sys.Date()-4) | 
         reg$Mail4Sent[i] == (Sys.Date()-5))) ||
      (is.na(reg$Mail5Sent[i] != 1) &&
       is.na(reg$completedLime6) == 1 &&
       is.na(reg$Mail5Rem) == 1 &&
       (reg$Mail5Sent[i] == (Sys.Date()-3) | 
         reg$Mail5Sent[i] == (Sys.Date()-4) | 
         reg$Mail5Sent[i] == (Sys.Date()-5))) ||
      (is.na(reg$Mail6Sent[i] != 1) &&
       is.na(reg$completedLime7) == 1 &&
       is.na(reg$Mail6Rem) == 1 &&
       (reg$Mail6Sent[i] == (Sys.Date()-3) |
         reg$Mail6Sent[i] == (Sys.Date()-4) |
         reg$Mail6Sent[i] == (Sys.Date()-5)))){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

for (i in 1:length(temp[,1])){
  num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
  if (is.na(temp$Mail1Rem[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Es ist nun ca. einen Monat her, dass du den ersten Teil der Studie ausgef?llt hast.</p> 
                       <p>Wir m?chten dich eben erinnern, eine kurze Frage zu deinem aktuellen Sprachgebrauch zu beantworten (die erste von insgesamt",
                       as.character(temp$BetweenQs[i]), " solcher Emails ?ber die n?chsten Monate.</p>
                       <p>Es sollte dich nicht mehr als 2 Minuten kosten, und sollte m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink2[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail1Rem[num] <- 1}
  else if (is.na(temp$Mail2Rem[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es gut.</p> 
                       <p>Wir m?chten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink3[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail2Rem[num] <- 1}
  else if (is.na(temp$Mail3Rem[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                       <p>Wir m?chten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink4[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail3Rem[num] <- 1}
  else if (is.na(temp$Mail4Rem[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                       <p>Wir m?chten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink5[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail4Rem[num] <- 1}
  else if (is.na(temp$Mail5Rem[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                       <p>Wir m?chten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink6[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p>
                       <br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail5Rem[num] <- 1}
  else if (is.na(temp$Mail6Rem[i]) == T){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                       <p>Wir m?chten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                       <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und m?glichst zeitnah beantwortet werden, am besten sofort, oder <b>sp?testens in den n?chsten 3 Tagen</b>.</p> 
                       Hier der Link:</b> <br / >",
                       temp$LimeLink7[i], 
                       "</p><p>Viel Spa? und beste Gr??e,<br />
                       Anne Mickan</p><br /><br />
                      (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    reg$Mail6Rem[num] <- 1}
  mail <- as.character(temp$email[i])
  out <- send.mail(
    from = "anne.mickan@mpi.nl"  ,    
    to = c(mail, "anne.mickan@mpi.nl"),
    #bcc = "anne.mickan@mpi.nl",
    subject = "[Max Planck Studie] Kurze Frage zu deinem aktuellen Sprachgebrauch (dauert nur 2 Minuten!)",
    html = TRUE,
    body = html_body,
    smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "******", tls = TRUE),
    authenticate = TRUE,
    send = TRUE,
    debug = TRUE)
  print(paste(i,". email sent to: ",as.character(temp$email[i]), sep = ""))
}

#####################################################################################
################ Create a new dataframe for the second Limesurvey Q #################
#####################################################################################

between <- reg
#between <- between[,c(1:9,39:44)]
between <- between[,c(1:9,39:44,70:74)]
data_long <- gather(between, session, token, token2:token12, factor_key=TRUE)


######### Get an overview over sign-up ############
for (i in 1) {
  print(paste("Total number of people signed up:             ", length(reg[,1])))
  print(paste("Total number of people invited:               ", length(reg[reg$Invited==1,1])))
  print(paste("People who have completed T1 questionnaire:   ", length(which(!is.na(reg$completedLime1==1)))))
  print(paste("People who have completed T1 pic naming:      ", length(which(!is.na(reg$completedPic1==1)))))
  print(paste("People who have completed T1 fluency test:    ", length(which(!is.na(reg$completedFluency1==1)))))
  print(paste("People who have completed T1 entirely:        ", length(which(!is.na(reg$T1done==1)))))
  print(paste("People who have been reminded once:           ", length(reg[reg$ReminderCount==1,1])))
  print(paste("People who have been reminded twice:          ", length(reg[reg$ReminderCount==2,1])))
  print(paste("People who have been reminded three times:    ", length(reg[reg$ReminderCount==3,1])))
  print(paste("Reminded three times but still not finished:  ", length(which(reg$ReminderCount==3 & is.na(reg$T1done)==1))))}

#####################################################################################
#### Save the updated participant database both as txt and as csv for limesurvey ####
#####################################################################################

setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
write.table(reg, "REGISTRATION.txt", quote = F, row.names = F, col.names = T, sep = "\t")

setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION/Limesurvey_upload")
write.csv(reg, "LimesurveyTokensT1.csv", quote=F)

setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION/Limesurvey_upload_2")
write.csv(data_long, "LimesurveyTokens_BetweenT1T2.csv", quote=F)
#write.csv(data_long, "LimesurveyTokens_BetweenT1T3.csv", quote=F)

################################################
################ Payment Check #################
################################################

### Read in the payment database
setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
pay <- read.delim("PaymentLogbook.txt", header = T, stringsAsFactors = F)

sub <- subset(reg, reg$T1done==1)
new2 <- subset(sub, !(sub$token %in% pay$token))
k <- length(pay[,1])
pay[(k+1):(k+length(new2[,1])),] <- NA

for (i in 1:length(new2[,1])){
    k <- k+1
    pay$token[k] <- new2$token[i]
    pay$firstname[k] <- new2$firstname[i]
    pay$lastname[k] <- new2$lastname[i]
    pay$email[k] <- new2$email[i]
    pay$tid[k] <- new2$tid[i]
    pay$T1done[k] <- new2$T1done[i]
    pay$PaidT1[k] <- 0
  }

setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
write.table(pay, "PaymentLogbook.txt", quote = F, row.names = F, col.names = T, sep = "\t")

# change the links for the inbetween questionnaires
for (i in 1:length(reg[,1])){
    reg$LimeLink2[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token2[i], "/lang/de", sep="")
    reg$LimeLink3[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token3[i], "/lang/de", sep="")
    reg$LimeLink4[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token4[i], "/lang/de", sep="")
    reg$LimeLink5[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token5[i], "/lang/de", sep="")
    reg$LimeLink6[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token6[i], "/lang/de", sep="")
    reg$LimeLink7[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/775398/token/", reg$token7[i], "/lang/de", sep="")
  }

# add the completed date for the first lime survey questionnaire
for (j in 1:length(reg[,1])){
  if (is.na(reg$completedLime1[j])!=1){
    l <- which(tolower(as.character(limelist$token)) == tolower(as.character(reg$token[j])))
    if (length(num)==1){
    reg$compLime1[j] <- limelist$completed[l]}else{
      reg$compLime1[j] <- NA
    }}
}
