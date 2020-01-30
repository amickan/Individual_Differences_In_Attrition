### Invites for T3 ###

# load packages
require(mailR)
require(gdata)
require(tidyr)
require(here)

### Read in the local participant database
reg <- read.delim(here("ADMINISTRATION","REGISTRATION.txt"), header = T, stringsAsFactors = F)

### Download the latest Limesurvey token list from T2 questionnaire
filescsv <- file.info(Sys.glob(here("ADMINISTRATION","Limesurvey_download_T3","*.csv")))
limelist3 <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)

## Add links for T3 tasks for everyone
#reg$PicLink3_reset <- NA
#reg$FluencyLink3 <- NA
#reg$PicLink3 <- NA
#reg$LimeLinkT3 <- NA
#reg$DoorsLink <- NA
#for (i in 1:length(reg[,1])){
# if (is.na(reg$PicLink3[i])==1 | is.na(reg$FluencyLink3[i])==1 | is.na(reg$PicLink3_reset[i])==1){
#    reg$LimeLinkT3[i] <- paste("https://survey.socsci.ru.nl/index.php/survey/index/sid/827149/token/", reg$token[i], "/lang/de", sep="")
#    reg$PicLink3_reset[i] <- paste("https://exp.socsci.ru.nl/spanish/naming4.html?ppn=", reg$token[i], "&reset=1", sep="")
 #  reg$PicLink3[i] <- paste("https://exp.socsci.ru.nl/spanish/naming4.html?ppn=", reg$token[i], sep="")
#    reg$FluencyLink3[i] <- paste("https://exp.socsci.ru.nl/spanish/fluency4.html?ppn=", reg$token[i], sep="")
#    reg$DoorsLink[i] <- paste("https://exp.socsci.ru.nl/spanish/doors_index.html?ppn=", reg$token[i], sep="")
  #}#}

## Check whether T3 questionnaire has been filled in 
for (i in 1:nrow(limelist3)){
  if (limelist3$completed[i] != "N"){
    m <- which(tolower(as.character(reg$token)) == tolower(as.character(limelist3$token[i])))
    reg$completedLimeT3[m] <- as.character(as.Date(limelist3$completed[i]))
  }
}


### check whether Pic naming and fluency and doors for T3 have been completed
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION/LogfilesT3")
filescsv <- file.info(list.files(getwd(),pattern = "data.*.csv"))
comptasks <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F, sep = ";")
pic <- comptasks[grep("naming_t3",comptasks$fileName), ]
ppspic <- unique(pic[pic$imgIndex == 143,]$ppn)
ppspic<-ppspic[!is.na(ppspic)]
flu <- comptasks[grep("fluency_t3",comptasks$fileName), ]
ppsflu <- unique(flu[flu$trial_index == 20,]$ppn)
ppsflu<-ppsflu[!is.na(ppsflu)]


#filescsv <- file.info(list.files(getwd(),pattern = "doors.*.csv"))
#doors <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)
#pprsdoors <- gsub("_doors.dat", "", unique(doors[which(doors$trial_num == 29),]$fileName))
doors <- comptasks[grep("doors",comptasks$fileName), ]
pprsdoors <- as.numeric(gsub("_doors.dat", "", unique(doors[which(doors$trial_num == 29),]$fileName)))

for (i in 1:length(ppsflu)){
  num <- which(tolower(as.character(reg$token)) == tolower(as.character(ppsflu[i])))
  reg$completedFluency3[num] <- 1
}

for (i in 1:length(ppspic)){
  num <- which(tolower(as.character(reg$token)) == tolower(as.character(ppspic[i])))
  reg$completedPic3[num] <- 1
}

for (i in 1:length(pprsdoors)){
  num <- which(tolower(as.character(reg$token)) == tolower(as.character(pprsdoors[i])))
  reg$completedDoors[num] <- 1
}

## Check whether T3 has been completed
for (i in 1:length(reg[,1])){
  if (is.na(reg$T3done[i]) == 1){
    if (is.na(reg$completedLimeT3[i])==0 && is.na(reg$completedPic3[i])==0 && is.na(reg$completedFluency3[i])==0 && is.na(reg$completedDoors[i])==0){
      reg$T3done[i] <- 1
    }}
}


####################### Send emails ################################

##### T3 - First invitation (i.e. email with all links for T3) #####
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if ((is.na(reg$InvitedT3[i]) == 1) && (reg$T2done[i] == 1) && is.na(reg$T3Datum[i]) == 0 && 
      (((reg$T3Datum[i] == (Sys.Date()))) | (reg$T3Datum[i] == (Sys.Date()-1)) | (reg$T3Datum[i] == (Sys.Date()-2)))){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp) != 0) {
  for (i in 1:length(temp[,1])){
    html_body <- paste("<html> Liebe(r)",
                       as.character(temp$firstname[i]),
                       ", <br /><p> Es ist Zeit für den dritten und letzten Teil der Studie. Wir freuen uns, dass du noch dabei bist und hoffen du hast dich wieder gut in Deutschland eingelebt!</p> 
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
                       temp$LimeLinkT3[i], 
                       "</p><p>2. <u>Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                       temp$PicLink3_reset[i],
                       "</p><p>3. <u>Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                       temp$FluencyLink3[i],
                       "</p><p>4. <u>Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                       temp$DoorsLink[i],
                       "</p><p>Viel Spaß und beste Grüße,<br />
                       Anne Mickan</p><br /><br />
                       (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                       </html>")
    mail <- as.character(temp$email[i])
    send.mail(
      from = "anne.mickan@mpi.nl"  ,    
      to = c(mail, "anne.mickan@mpi.nl"),
      subject = "[Max Planck Studie] Links für Teil 3 (funktionieren NICHT in SAFARI!!)",
      html = TRUE,
      body = html_body,
      smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "*****", tls = TRUE),
      authenticate = TRUE,
      send = TRUE,
      debug = F)
    print(paste(i,". email sent to: ",as.character(temp$email[i]), sep = ""))
    # mark that invitation email has been sent
    num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
    reg$InvitedT3[num] <- 1
    reg$InvitedDateT3[num] <- as.character(as.Date(Sys.Date()))
  }
} else if (length(temp) == 0) {
  print("Keine Einladungen versandt.")
}

###### Reminder for T3 session #####
## this is only checked 4, 7 and 10 days after initial invitation
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if (is.na(reg$InvitedT3[i]) != 1 && is.na(reg$T3done[i]) == 1 &&
      (is.na(reg$completedLimeT3[i]) == 1 | is.na(reg$completedPic3[i]) == 1 | is.na(reg$completedFluency3[i]) == 1) &&
      ((reg$ReminderCountT3[i] == 0 && (reg$InvitedDateT3[i] == (Sys.Date()-4) | reg$InvitedDateT3[i] == (Sys.Date()-3) | reg$InvitedDateT3[i] == (Sys.Date()-5) )) || 
       (reg$ReminderCountT3[i] == 1 && (reg$InvitedDateT3[i] == (Sys.Date()-7) | reg$InvitedDateT3[i] == (Sys.Date()-9) | reg$InvitedDateT3[i] == (Sys.Date()-8))) ||
      (reg$ReminderCount[i] == 2 && (reg$InvitedDateT3[i] == (Sys.Date()-12) | reg$InvitedDateT3[i] == (Sys.Date()-13) | reg$InvitedDateT3[i] == (Sys.Date()-14))))
  ){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp) != 0) {
  for (i in 1:length(temp[,1])){
    if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         </b><p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")}
    else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")}
    else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kastchen, mit oder ohne Hakchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Hakchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier ausserdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in Safari oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spass und beste Grusse,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9745; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9745; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9745; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    }   else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9744; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == T && is.na(temp$completedDoors[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9744; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9745; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == F && is.na(temp$completedPic3[i]) == T && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == F){
                        html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9745; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9744; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9745; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
    } else if (is.na(temp$completedLimeT3[i]) == T && is.na(temp$completedPic3[i]) == F && is.na(temp$completedFluency3[i]) == F && is.na(temp$completedDoors[i]) == F){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Dies ist eine kurze Erinnerung, am dritten Teil der Studie 'Spanisch als Fremdsprache' teilzunehmen.</p> 
                         <p>Unten noch einmal die Links. Neben den Links findest du jeweils ein Kästchen, mit oder ohne Häkchen. Wenn deine Daten erfolgreich auf unserem Server gespeichert wurden, siehst du unten ein Häkchen neben der entsprechenden Aufgabe.</u></p>  
                         <p>Melde dich bitte, wenn du der Meinung bist, das etwas nicht stimmt.</p> 
                         <p>Hier außerdem auch noch einmal die Erinnerung an Folgendes:</p> 
                         <ul> 
                         <li> Die Aufgaben funktionieren nur in <b>Chrome und Firefox</b>, <b>NICHT in SAFARI oder Internet Explorer</b>. Auch auf Handys kann es Probleme geben.</li>
                         <li> <b>Bitte halte dich unbedingt an die richtige Reihenfolge</b>!</li>
                         <li> Fur die letzten beiden Aufgaben brauchst du ein Mikrofon. Teste dein Mikrofon hier: https://www.speakpipe.com/voice-recorder </li></ul>
                         <p>&#9744; <u> 1. Fragebogen (ca. 15 min):</u><br />",
                         temp$LimeLinkT3[i], 
                         "</p><p>&#9745; <u> 2. Spanisch-Vokabeltest (zwischen 30-60 min):</u><br />", 
                         temp$PicLink3_reset[i],
                         "</p><p>&#9745; <u> 3. Deutsch und Englisch Tests (ca. 15 min):</u><br />", 
                         temp$FluencyLink3[i],
                         "</p><p>&#9745; <u> 4. Visueller Gedächtnistest (ca. 5 min):</u><br />", 
                         temp$DoorsLink[i],
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
      subject = "[Max Planck Studie] Erinnerung zur Teilnahme an Teil 3 (Links funktionieren NICHT in SAFARI!!)",
      html = TRUE,
      body = html_body,
      smtp = list(host.name = "email.gwdg.de", port = 587, user.name = "anne.mickan@mpi.nl", passwd = "*****", tls = TRUE),
      authenticate = TRUE,
      send = TRUE,
      debug = F)
    print(paste(i,". reminder sent to: ",as.character(temp$email[i]), sep = ""))
    # mark that invitation email has been sent
    num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
    reg$ReminderCountT3[num] <- reg$ReminderCountT3[num] + 1
    }
    }


############################################################
#### Save the updated participant database both as txt  ####
############################################################
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION")
write.table(reg, "REGISTRATION.txt", quote = F, row.names = F, col.names = T, sep = "\t")

#### Payment admin #####
### Read in the payment database
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION")
pay <- read.delim("PaymentLogbookT3.txt", header = T, stringsAsFactors = F)

sub <- subset(reg, reg$T3done==1)
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
    pay$T3done[k] <- new2$T3done[i]
    pay$PaidT3[k] <- 0
  }}

write.table(pay, "PaymentLogbookT3.txt", quote = F, row.names = F, col.names = T, sep = "\t")

