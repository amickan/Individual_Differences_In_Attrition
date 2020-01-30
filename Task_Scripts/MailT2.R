### Invites for T2 ###

# load packages
require(mailR)
require(gdata)
require(tidyr)
require(here)

### Read in the local participant database
reg <- read.delim(here("ADMINISTRATION","REGISTRATION.txt"), header = T, stringsAsFactors = F)

### Download the latest Limesurvey token list from T2 questionnaire
filescsv <- file.info(Sys.glob(here("ADMINISTRATION","Limesurvey_download_T2","*.csv")))
limelist2 <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)

## Add links for T2 tasks for everyone
#reg$PicLink2_reset <- NA
#for (i in 1:length(reg[,1])){
 # if (is.na(reg$PicLink2[i])==1 | is.na(reg$FluencyLink2[i])==1 | is.na(reg$PicLink2_reset[i])==1){
#    reg$LimeLinkT2[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/678123/token/", reg$token[i], "/lang/de", sep="")
#    reg$PicLink2_reset[i] <- paste("https://exp.socsci.ru.nl/spanish/naming3.html?ppn=", reg$token[i], "&reset=1", sep="")
#    #reg$PicLink2[i] <- paste("https://exp.socsci.ru.nl/spanish/naming3.html?ppn=", reg$token[i], sep="")
#    reg$FluencyLink2[i] <- paste("https://exp.socsci.ru.nl/spanish/fluency3.html?ppn=", reg$token[i], sep="")
#  #}
#    }

## Check whether T2 questionnaire has been filled in 
for (i in 1:nrow(limelist2)){
  if (limelist2$completed[i] != "N"){
    m <- which(tolower(as.character(reg$token)) == tolower(as.character(limelist2$token[i])))
    reg$completedLimeT2[m] <- as.character(as.Date(limelist2$completed[i]))
  }
}

### check whether Pic naming and fluency for T2 have been completed
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION/LogfilesT2")
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
  reg$completedFluency2[num] <- 1
}

for (i in 1:length(ppspic)){
  num <- which(tolower(as.character(reg$token)) == tolower(as.character(ppspic[i])))
  reg$completedPic2[num] <- 1
}

## Check whether T2 has been completed
for (i in 1:length(reg[,1])){
  if (is.na(reg$T2done[i]) == 1){
    if (is.na(reg$completedLimeT2[i])==0 && is.na(reg$completedPic2[i])==0 && is.na(reg$completedFluency2[i])==0){
      reg$T2done[i] <- 1
    }}
}


####################### Send emails ################################

##### T2 - First invitation (i.e. email with all links for T2) #####
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if ((is.na(reg$InvitedT2[i]) == 1) && (reg$T1done[i] !="FA") && 
      (((reg$T2Datum[i] == (Sys.Date()))) | (reg$T2Datum[i] == (Sys.Date()-1)) | (reg$T2Datum[i] == (Sys.Date()-2)))){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp) != 0) {
  for (i in 1:length(temp[,1])){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p> Es ist Zeit für den zweiten Teil der Studie. Wir freuen uns, dass du noch dabei bist und hoffen du hattest (bisher) eine tolle Zeit in Spanien/Lateinamerika!</p> 
                       <p>Unten findest du wieder die Links zu den Aufgaben. Es gelten die gleichen Regeln, wie beim ersten Mal. Bitte arbeite dich von <b>oben nach unten, also der Reihenfolge nach, durch die Links</b>.</p>
                       <p>Insgesamt hast du nun <b> ca. 7 Tage Zeit dafür</b>. Erledige die Aufgaben jedoch am besten bald und auf einmal, also in einer Sitzung.</p>  
                       <p>Bevor du anfängst, stelle bitte noch einmal sicher, dass:</p> 
                       <ul> 
                       <li> du in einem ruhigen Raum sitzt, wo du ungestört und allein arbeiten kannst und es <u>keine starken Hintergrundgeräusche gibt</u>.</li>
                       <li> du <b>die Aufgaben an einem Computer erledigst.</b> Am besten dem gleichen Computer, wie vor ein paar Monaten schon (hat damals ja scheinbar funktioniert).</li>
                       <li> du alle Aufgaben im <b>gleichen Browser</b> erledigst (<b>FIREFOX oder CHROME</b>)</li>
                       <li><u><b>ACHTUNG: Aufgaben funktionieren NICHT IN SAFARI und Internet Exlporer.</b></u></li>
                       <li> du eine stabile Internetverbindung hast.</li>
                       <li> <b>dein Computer ein integriertes Mikrofon hat</b> (oder du ein Mikrofon angeschlossen hast). Um dein Mikrofon vorab zu testen, schau hier vorbei: https://www.speakpipe.com/voice-recorder Trouble-shooting Tips findest du notfalls hier: https://www.onlinemictest.com/</li>              
                       </ul>
                       <p>Wenn ihr euch unsicher seid, ob euer Mikrofon funktioniert, bzw. verständliche Aufnahmen macht, meldet euch lieber. Ich antworte in der Regel schnell :-).</p><br />
                       <b>Hier die Links (<b>unbedingt die Reihenfolge einhalten!</b>):</b> <br / >
                       <p>1. <u>Fragebogen (ca. 20 min):</u><br />",
                       temp$LimeLinkT2[i], 
                       "</p><p>2. <u>Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                       temp$PicLink2_reset[i],
                       "</p><p>3. <u>Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                       temp$FluencyLink2[i],
                       "</p><p>Viel Spaß und beste Grüße,<br />
                       Anne Mickan</p><br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    mail <- as.character(temp$email[i])
    send.mail(
      from = "anne.mickan@mpi.nl"  ,    
      to = c(mail, "anne.mickan@mpi.nl"),
      subject = "[Max Planck Studie] Links für Teil 2 (funktionieren NICHT in SAFARI!!)",
      html = TRUE,
      body = html_body,
      smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "******", tls = TRUE),
      authenticate = TRUE,
      send = TRUE,
      debug = F)
    print(paste(i,". email sent to: ",as.character(temp$email[i]), sep = ""))
    # mark that invitation email has been sent
    num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
    reg$InvitedT2[num] <- 1
    reg$InvitedDateT2[num] <- as.character(as.Date(Sys.Date()))
  }
} else if (length(temp) == 0) {
  print("Keine Einladungen versandt.")
}

###### Reminder for T2 session #####
## this is only checked 4, 7 and 10 days after initial invitation
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if (is.na(reg$InvitedT2[i]) != 1 && is.na(reg$T2done[i]) != "FA" &&
      (is.na(reg$completedLimeT2[i]) == 1 | is.na(reg$completedPic2[i]) == 1 | is.na(reg$completedFluency2[i]) == 1) &&
      ((reg$ReminderCountT2[i] == 0 && (reg$InvitedDateT2[i] == (Sys.Date()-4) | reg$InvitedDateT2[i] == (Sys.Date()-3) | reg$InvitedDateT2[i] == (Sys.Date()-5) )) || 
       (reg$ReminderCountT2[i] == 1 && (reg$InvitedDateT2[i] == (Sys.Date()-7) | reg$InvitedDateT2[i] == (Sys.Date()-9) | reg$InvitedDateT2[i] == (Sys.Date()-8)))) #||
      #(reg$ReminderCount[i] == 2 && (reg$InvitedDate[i] == (Sys.Date()-12) | reg$InvitedDate[i] == (Sys.Date()-13) | reg$InvitedDate[i] == (Sys.Date()-14)))
  ){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp) != 0) {
  for (i in 1:length(temp[,1])){
    if (is.na(temp$completedLimeT2[i]) == T && is.na(temp$completedPic2[i]) == T && is.na(temp$completedFluency2[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am zweiten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         </b><p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT2[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink2_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink2[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")}
    else if (is.na(temp$completedLimeT2[i]) == F && is.na(temp$completedPic2[i]) == T && is.na(temp$completedFluency2[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am zweiten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT2[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink2_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink2[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")}
    else if (is.na(temp$completedLimeT2[i]) == F && is.na(temp$completedPic2[i]) == F && is.na(temp$completedFluency2[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am zweiten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT2[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink2_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink2[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT2[i]) == F && is.na(temp$completedPic2[i]) == T && is.na(temp$completedFluency2[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am zweiten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT2[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink2_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink2[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT2[i]) == T && is.na(temp$completedPic2[i]) == F && is.na(temp$completedFluency2[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am zweiten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kastchen, mit oder ohne Hakchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Hakchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier ausserdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT2[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink2[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink2[i],
                         "</p><p>Viel Spass und beste Grusse,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT2[i]) == T && is.na(temp$completedPic2[i]) == T && is.na(temp$completedFluency2[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am zweiten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                        <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT2[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink2_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink2[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (length(temp) == 0) {
      print("Keine neuen Erinnerungen versandt.")
    }
    mail <- as.character(temp$email[i])
    out <- send.mail(
      from = "anne.mickan@mpi.nl"  ,    
      to = c(mail, "anne.mickan@mpi.nl"),
      #bcc = "anne.mickan@mpi.nl",
      subject = "[Max Planck Studie] Erinnerung zur Teilnahme an Teil 2 (Links funktionieren NICHT in SAFARI!!)",
      html = TRUE,
      body = html_body,
      smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "*****", tls = TRUE),
      authenticate = TRUE,
      send = TRUE,
      debug = F)
    print(paste(i,". reminder sent to: ",as.character(temp$email[i]), sep = ""))
    # mark that invitation email has been sent
    num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
    reg$ReminderCountT2[num] <- reg$ReminderCountT2[num] + 1
    }
    }


############################################################
#### Save the updated participant database both as txt  ####
############################################################
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION")
write.table(reg, "REGISTRATION.txt", quote = F, row.names = F, col.names = T, sep = "\t")

#### Payment admin #####
### Read in the payment database
#setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
pay <- read.delim("PaymentLogbookT2.txt", header = T, stringsAsFactors = F)

sub <- subset(reg, reg$T2done==1)
new2 <- subset(sub, !(sub$token %in% pay$token))
k <- length(pay[,1])
if (nrow(new2)==0){} else {
  pay[(k+1):(k+length(new2[,1])),] <- NA
  for (i in 1:length(new2[,1])){
    k <- k+1
    pay$token[k] <- new2$token[i]
    pay$firstname[k] <- new2$firstname[i]
    pay$lastname[k] <- new2$lastname[i]
    pay$email[k] <- new2$email[i]
    pay$tid[k] <- new2$tid[i]
    pay$T2done[k] <- new2$T2done[i]
    pay$PaidT2[k] <- 0
  }}

#setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
write.table(pay, "PaymentLogbookT2.txt", quote = F, row.names = F, col.names = T, sep = "\t")

