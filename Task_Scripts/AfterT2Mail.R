### Load required packages
require(mailR)
require(gdata)
require(tidyr)
require(here)

### Read in the local participant database
#setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION")
reg <- read.delim(here("ADMINISTRATION","REGISTRATION.txt"), header = T, stringsAsFactors = F)

### Download the latest Limesurvey token list from inbetween questionnaire
#setwd("U:/PhD/EXPERIMENT 4/ADMINISTRATION/Limesurvey_download_2")
#filescsv <- file.info(list.files(getwd(),pattern = "csv"))
filescsv <- file.info(Sys.glob(here("ADMINISTRATION","Limesurvey_download_2","*.csv")))
#filescsv <- file.info(list.files(here("ADMINISTRATION","Limesurvey_download_2"),pattern = "csv"))
limelist2 <- read.csv(rownames(filescsv)[order(filescsv$mtime)][nrow(filescsv)], stringsAsFactors = F)

## Check whether in between questionnaires have been filled in 
for (i in 1:nrow(limelist2)){
  if (limelist2$completed[i] != "N"){
    m <- which(tolower(as.character(reg$token)) == gsub("(\\d.*)_\\d.*","\\1", tolower(as.character(limelist2$token[i]))))
    if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 2){
      reg$completedLime2[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 3){
      reg$completedLime3[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 4){
      reg$completedLime4[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 5){
      reg$completedLime5[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 6){
      reg$completedLime6[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 7){
      reg$completedLime7[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 8){
      reg$completedLime8[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 9){
      reg$completedLime9[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 10){
      reg$completedLime10[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 11){
      reg$completedLime11[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 12){
      reg$completedLime12[m] <- 1}
    else if (gsub("\\d.*_(\\d)","\\1",limelist2$token[i]) == 13){
      reg$completedLime13[m] <- 1}
  }
}
reg$compLime1 <- as.Date(reg$compLime1)

# only select people who completed T2 
#reg[is.na(reg$T2done)==0,]->reg2
#reg2[reg2$T2done == 1,]-> reg


#### Invitation for in between emails AFTER T2 ####
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if (is.na(reg$T2done[i]) == 0 && (reg$T2done[i] == 1 && reg$T3Datum[i] > Sys.Date()+14)) {
    #print(i)
    if (((is.na(reg$Mail3Sent[i]) == 1 && (is.na(reg$Mail2Sent[i]) == 0 && reg$Mail2Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-29) | 
           reg$completedLimeT2[i] == (Sys.Date()-31) | 
           reg$completedLimeT2[i] == (Sys.Date()-30) |
           reg$completedLimeT2[i] == (Sys.Date()-60) |
           reg$completedLimeT2[i] == (Sys.Date()-59) |
           reg$completedLimeT2[i] == (Sys.Date()-61) |
           reg$completedLimeT2[i] == (Sys.Date()-90) |
           reg$completedLimeT2[i] == (Sys.Date()-89) |
           reg$completedLimeT2[i] == (Sys.Date()-91))) ||
        ((is.na(reg$Mail4Sent[i]) == 1 && (is.na(reg$Mail3Sent[i]) == 0 && reg$Mail3Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-29) | 
           reg$completedLimeT2[i] == (Sys.Date()-31) | 
           reg$completedLimeT2[i] == (Sys.Date()-30) |
           reg$completedLimeT2[i] == (Sys.Date()-59) | 
           reg$completedLimeT2[i] == (Sys.Date()-61) | 
           reg$completedLimeT2[i] == (Sys.Date()-60) |
           reg$completedLimeT2[i] == (Sys.Date()-90) |
           reg$completedLimeT2[i] == (Sys.Date()-89) |
           reg$completedLimeT2[i] == (Sys.Date()-91) |
           reg$completedLimeT2[i] == (Sys.Date()-120) |
           reg$completedLimeT2[i] == (Sys.Date()-119) |
           reg$completedLimeT2[i] == (Sys.Date()-121))) || 
        ((is.na(reg$Mail5Sent[i]) == 1 && (is.na(reg$Mail4Sent[i]) == 0 && reg$Mail4Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-1-29) | 
           reg$completedLimeT2[i] == (Sys.Date()-31) | 
           reg$completedLimeT2[i] == (Sys.Date()-30) |
           reg$completedLimeT2[i] == (Sys.Date()-59) | 
           reg$completedLimeT2[i] == (Sys.Date()-61) | 
           reg$completedLimeT2[i] == (Sys.Date()-60) |
           reg$completedLimeT2[i] == (Sys.Date()-89) | 
           reg$completedLimeT2[i] == (Sys.Date()-90) | 
           reg$completedLimeT2[i] == (Sys.Date()-91) |
           reg$completedLimeT2[i] == (Sys.Date()-120) |
           reg$completedLimeT2[i] == (Sys.Date()-119) |
           reg$completedLimeT2[i] == (Sys.Date()-121) |
           reg$completedLimeT2[i] == (Sys.Date()-150) |
           reg$completedLimeT2[i] == (Sys.Date()-149) |
           reg$completedLimeT2[i] == (Sys.Date()-151))) ||
        ((is.na(reg$Mail6Sent[i]) == 1 && (is.na(reg$Mail5Sent[i]) == 0 && reg$Mail5Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-29) | 
          reg$completedLimeT2[i] == (Sys.Date()-31) | 
          reg$completedLimeT2[i] == (Sys.Date()-30) |
          reg$completedLimeT2[i] == (Sys.Date()-59) | 
           reg$completedLimeT2[i] == (Sys.Date()-61) | 
           reg$completedLimeT2[i] == (Sys.Date()-60) |
           reg$completedLimeT2[i] == (Sys.Date()-89) | 
           reg$completedLimeT2[i] == (Sys.Date()-90) | 
           reg$completedLimeT2[i] == (Sys.Date()-91) |
           reg$completedLimeT2[i] == (Sys.Date()-120) | 
           reg$completedLimeT2[i] == (Sys.Date()-119) | 
           reg$completedLimeT2[i] == (Sys.Date()-121) |
           reg$completedLimeT2[i] == (Sys.Date()-150) |
           reg$completedLimeT2[i] == (Sys.Date()-149) |
           reg$completedLimeT2[i] == (Sys.Date()-151) |
           reg$completedLimeT2[i] == (Sys.Date()-180) |
           reg$completedLimeT2[i] == (Sys.Date()-179) |
           reg$completedLimeT2[i] == (Sys.Date()-181))) ||
        ((is.na(reg$Mail7Sent[i]) == 1 && (is.na(reg$Mail6Sent[i]) == 0 && reg$Mail6Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-29) | 
          reg$completedLimeT2[i] == (Sys.Date()-31) | 
          reg$completedLimeT2[i] == (Sys.Date()-30) |
          reg$completedLimeT2[i] == (Sys.Date()-59) | 
           reg$completedLimeT2[i] == (Sys.Date()-61) | 
           reg$completedLimeT2[i] == (Sys.Date()-60) |
           reg$completedLimeT2[i] == (Sys.Date()-89) | 
           reg$completedLimeT2[i] == (Sys.Date()-90) | 
           reg$completedLimeT2[i] == (Sys.Date()-91) |
           reg$completedLimeT2[i] == (Sys.Date()-120) | 
           reg$completedLimeT2[i] == (Sys.Date()-119) | 
           reg$completedLimeT2[i] == (Sys.Date()-121) |
           reg$completedLimeT2[i] == (Sys.Date()-151) | 
           reg$completedLimeT2[i] == (Sys.Date()-149) | 
           reg$completedLimeT2[i] == (Sys.Date()-150) |
           reg$completedLimeT2[i] == (Sys.Date()-180) |
           reg$completedLimeT2[i] == (Sys.Date()-179) |
           reg$completedLimeT2[i] == (Sys.Date()-181) |
           reg$completedLimeT2[i] == (Sys.Date()-200) |
           reg$completedLimeT2[i] == (Sys.Date()-199) |
           reg$completedLimeT2[i] == (Sys.Date()-201))) ||
        ((is.na(reg$Mail8Sent[i]) == 1 && (is.na(reg$Mail7Sent[i]) == 0 && reg$Mail7Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-59) | 
          reg$completedLimeT2[i] == (Sys.Date()-61) | 
          reg$completedLimeT2[i] == (Sys.Date()-60) |
          reg$completedLimeT2[i] == (Sys.Date()-89) | 
           reg$completedLimeT2[i] == (Sys.Date()-90) | 
           reg$completedLimeT2[i] == (Sys.Date()-91) |
           reg$completedLimeT2[i] == (Sys.Date()-120) | 
           reg$completedLimeT2[i] == (Sys.Date()-119) | 
           reg$completedLimeT2[i] == (Sys.Date()-121) |
           reg$completedLimeT2[i] == (Sys.Date()-151) | 
           reg$completedLimeT2[i] == (Sys.Date()-149) | 
           reg$completedLimeT2[i] == (Sys.Date()-150) |
           reg$completedLimeT2[i] == (Sys.Date()-181) |
           reg$completedLimeT2[i] == (Sys.Date()-179) |
           reg$completedLimeT2[i] == (Sys.Date()-180) |
           reg$completedLimeT2[i] == (Sys.Date()-210) |
           reg$completedLimeT2[i] == (Sys.Date()-211) |
           reg$completedLimeT2[i] == (Sys.Date()-209) |
           reg$completedLimeT2[i] == (Sys.Date()-240) |
           reg$completedLimeT2[i] == (Sys.Date()-239) |
           reg$completedLimeT2[i] == (Sys.Date()-241))) ||
        ((is.na(reg$Mail9Sent[i]) == 1 && (is.na(reg$Mail8Sent[i]) == 0 && reg$Mail8Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-59) | 
          reg$completedLimeT2[i] == (Sys.Date()-61) | 
          reg$completedLimeT2[i] == (Sys.Date()-60) |
          reg$completedLimeT2[i] == (Sys.Date()-89) | 
          reg$completedLimeT2[i] == (Sys.Date()-90) | 
          reg$completedLimeT2[i] == (Sys.Date()-91) |
          reg$completedLimeT2[i] == (Sys.Date()-120) | 
           reg$completedLimeT2[i] == (Sys.Date()-119) | 
           reg$completedLimeT2[i] == (Sys.Date()-121) |
           reg$completedLimeT2[i] == (Sys.Date()-151) | 
           reg$completedLimeT2[i] == (Sys.Date()-149) | 
           reg$completedLimeT2[i] == (Sys.Date()-150) |
           reg$completedLimeT2[i] == (Sys.Date()-181) |
           reg$completedLimeT2[i] == (Sys.Date()-179) |
           reg$completedLimeT2[i] == (Sys.Date()-180) |
           reg$completedLimeT2[i] == (Sys.Date()-209) |
           reg$completedLimeT2[i] == (Sys.Date()-210) |
           reg$completedLimeT2[i] == (Sys.Date()-211) |
           reg$completedLimeT2[i] == (Sys.Date()-240) |
           reg$completedLimeT2[i] == (Sys.Date()-241) |
           reg$completedLimeT2[i] == (Sys.Date()-239) |
           reg$completedLimeT2[i] == (Sys.Date()-270) |
           reg$completedLimeT2[i] == (Sys.Date()-269) |
           reg$completedLimeT2[i] == (Sys.Date()-271))) ||
        ((is.na(reg$Mail10Sent[i]) == 1 && (is.na(reg$Mail9Sent[i]) == 0 && reg$Mail9Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-89) | 
          reg$completedLimeT2[i] == (Sys.Date()-90) | 
          reg$completedLimeT2[i] == (Sys.Date()-91) |
          reg$completedLimeT2[i] == (Sys.Date()-120) | 
          reg$completedLimeT2[i] == (Sys.Date()-119) | 
          reg$completedLimeT2[i] == (Sys.Date()-121) |
          reg$completedLimeT2[i] == (Sys.Date()-151) | 
           reg$completedLimeT2[i] == (Sys.Date()-149) | 
           reg$completedLimeT2[i] == (Sys.Date()-150) |
           reg$completedLimeT2[i] == (Sys.Date()-181) |
           reg$completedLimeT2[i] == (Sys.Date()-179) |
           reg$completedLimeT2[i] == (Sys.Date()-180) |
           reg$completedLimeT2[i] == (Sys.Date()-209) |
           reg$completedLimeT2[i] == (Sys.Date()-210) |
           reg$completedLimeT2[i] == (Sys.Date()-211) |
           reg$completedLimeT2[i] == (Sys.Date()-239) |
           reg$completedLimeT2[i] == (Sys.Date()-240) |
           reg$completedLimeT2[i] == (Sys.Date()-241) |
           reg$completedLimeT2[i] == (Sys.Date()-270) |
           reg$completedLimeT2[i] == (Sys.Date()-269) |
           reg$completedLimeT2[i] == (Sys.Date()-271) |
           reg$completedLimeT2[i] == (Sys.Date()-300) |
           reg$completedLimeT2[i] == (Sys.Date()-299) |
           reg$completedLimeT2[i] == (Sys.Date()-301))) ||
        ((is.na(reg$Mail11Sent[i]) == 1 && (is.na(reg$Mail10Sent[i]) == 0 && reg$Mail10Sent[i] < Sys.Date()-14)) &&
         (reg$completedLimeT2[i] == (Sys.Date()-120) | 
          reg$completedLimeT2[i] == (Sys.Date()-119) | 
          reg$completedLimeT2[i] == (Sys.Date()-121) |
          reg$completedLimeT2[i] == (Sys.Date()-151) | 
          reg$completedLimeT2[i] == (Sys.Date()-149) | 
          reg$completedLimeT2[i] == (Sys.Date()-150) |
          reg$completedLimeT2[i] == (Sys.Date()-181) |
           reg$completedLimeT2[i] == (Sys.Date()-179) |
           reg$completedLimeT2[i] == (Sys.Date()-180) |
           reg$completedLimeT2[i] == (Sys.Date()-209) |
           reg$completedLimeT2[i] == (Sys.Date()-210) |
           reg$completedLimeT2[i] == (Sys.Date()-211) |
           reg$completedLimeT2[i] == (Sys.Date()-239) |
           reg$completedLimeT2[i] == (Sys.Date()-240) |
           reg$completedLimeT2[i] == (Sys.Date()-241) |
           reg$completedLimeT2[i] == (Sys.Date()-269) |
           reg$completedLimeT2[i] == (Sys.Date()-270) |
           reg$completedLimeT2[i] == (Sys.Date()-271) |
           reg$completedLimeT2[i] == (Sys.Date()-300) |
           reg$completedLimeT2[i] == (Sys.Date()-299) |
           reg$completedLimeT2[i] == (Sys.Date()-301) |
           reg$completedLimeT2[i] == (Sys.Date()-330) |
           reg$completedLimeT2[i] == (Sys.Date()-329) |
           reg$completedLimeT2[i] == (Sys.Date()-331)))){
      temp <- rbind(temp, reg[i,])
      k <- k+1
    }}
}
temp <- temp[-1,] 


if (length(temp)==0){print("No invites sent.")} else{
  for (i in 1:length(temp[,1])){
    num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
    if (is.na(temp$Mail1Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Es ist nun ca. einen Monat her, dass du den ersten Teil der Studie ausgefüllt hast.</p> 
                         <p>Wir möchten dich nun bitten, eine kurze Frage zu deinem <b>aktuellen</b> Sprachgebrauch zu beantworten (die erste von insgesamt",
                         as.character(temp$BetweenQs[i]), " solcher Emails über die nächsten Monate).</p>
                         <p>Es sollte dich nicht mehr als 2 Minuten kosten, und sollte möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink2[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail1Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem <b>aktuellen</b> Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink3[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail2Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Und wieder ist ein Monat vergangen. Wir hoffen, dir geht es gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink4[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail3Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink5[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail4Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink6[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail5Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink7[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail6Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == 0 && is.na(temp$Mail7Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink8[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail7Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == 0 && is.na(temp$Mail7Sent[i]) == 0 && is.na(temp$Mail8Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink9[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail8Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == 0 && is.na(temp$Mail7Sent[i]) == 0 && is.na(temp$Mail8Sent[i]) == 0 && is.na(temp$Mail9Sent[i]) == T ){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink10[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail9Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == 0 && is.na(temp$Mail7Sent[i]) == 0 && is.na(temp$Mail8Sent[i]) == 0 && is.na(temp$Mail9Sent[i]) == 0 && is.na(temp$Mail10Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink11[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail10Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == 0 && is.na(temp$Mail7Sent[i]) == 0 && is.na(temp$Mail8Sent[i]) == 0 && is.na(temp$Mail9Sent[i]) == 0 && is.na(temp$Mail10Sent[i]) == 0 && is.na(temp$Mail11Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink12[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail11Sent[num] <-  as.character(as.Date(Sys.Date()))}
    else if (is.na(temp$Mail1Sent[i])==0 && is.na(temp$Mail2Sent[i]) == 0 && is.na(temp$Mail3Sent[i]) == 0 && is.na(temp$Mail4Sent[i]) == 0 && is.na(temp$Mail5Sent[i]) == 0 && is.na(temp$Mail6Sent[i]) == 0 && is.na(temp$Mail7Sent[i]) == 0 && is.na(temp$Mail8Sent[i]) == 0 && is.na(temp$Mail9Sent[i]) == 0 && is.na(temp$Mail10Sent[i]) == 0 && is.na(temp$Mail11Sent[i]) == 0 && is.na(temp$Mail12Sent[i]) == T){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /><p>Ein weiterer Monat ist vergangen. Wir hoffen, dir geht es nach wie vor gut.</p> 
                         <p>Es ist mal wieder an der Zeit, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 1 oder 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink13[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail12Sent[num] <-  as.character(as.Date(Sys.Date()))}
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
      debug = F)
    print(paste(i,". email sent to: ",as.character(temp$email[i]), sep = ""))
  }}

## Reminders to fill in the inbetween questionnaire 
temp <- matrix(NA, 1,ncol(reg))
colnames(temp) <- colnames(reg)
for (i in 1:length(reg[,1])){  
  k <- 1
  if ((is.na(reg$Mail1Sent[i]) != 1 &&
       is.na(reg$completedLime2[i]) == 1 &&
       is.na(reg$Mail1Rem[i]) == 1 &&
       (reg$Mail1Sent[i] == (Sys.Date()-2) | 
        reg$Mail1Sent[i] == (Sys.Date()-3) | 
        reg$Mail1Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail2Sent[i]) != 1 &&
       is.na(reg$completedLime3[i]) == 1 &&
       is.na(reg$Mail2Rem[i]) == 1 &&
       (reg$Mail2Sent[i] == (Sys.Date()-2) | 
        reg$Mail2Sent[i] == (Sys.Date()-3) | 
        reg$Mail2Sent[i] == (Sys.Date()-4))) || 
      (is.na(reg$Mail3Sent[i]) != 1 &&
       is.na(reg$completedLime4[i]) == 1 &&
       is.na(reg$Mail3Rem[i]) == 1 &&
       (reg$Mail3Sent[i] == (Sys.Date()-2) | 
        reg$Mail3Sent[i] == (Sys.Date()-3) | 
        reg$Mail3Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail4Sent[i]) != 1 &&
       is.na(reg$completedLime5[i]) == 1 &&
       is.na(reg$Mail4Rem[i]) == 1 &&
       (reg$Mail4Sent[i] == (Sys.Date()-2) | 
        reg$Mail4Sent[i] == (Sys.Date()-3) | 
        reg$Mail4Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail5Sent[i]) != 1 &&
       is.na(reg$completedLime6[i]) == 1 &&
       is.na(reg$Mail5Rem[i]) == 1 &&
       (reg$Mail5Sent[i] == (Sys.Date()-2) | 
        reg$Mail5Sent[i] == (Sys.Date()-3) | 
        reg$Mail5Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail6Sent[i]) != 1 &&
       is.na(reg$completedLime7[i]) == 1 &&
       is.na(reg$Mail6Rem[i]) == 1 &&
       (reg$Mail6Sent[i] == (Sys.Date()-2) |
        reg$Mail6Sent[i] == (Sys.Date()-3) |
        reg$Mail6Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail7Sent[i]) != 1 &&
       is.na(reg$completedLime8[i]) == 1 &&
       is.na(reg$Mail7Rem[i]) == 1 &&
       (reg$Mail7Sent[i] == (Sys.Date()-2) |
        reg$Mail7Sent[i] == (Sys.Date()-3) |
        reg$Mail7Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail8Sent[i]) != 1 &&
       is.na(reg$completedLime9[i]) == 1 &&
       is.na(reg$Mail8Rem[i]) == 1 &&
       (reg$Mail8Sent[i] == (Sys.Date()-2) |
        reg$Mail8Sent[i] == (Sys.Date()-3) |
        reg$Mail8Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail9Sent[i]) != 1 &&
       is.na(reg$completedLime10[i]) == 1 &&
       is.na(reg$Mail9Rem[i]) == 1 &&
       (reg$Mail9Sent[i] == (Sys.Date()-2) |
        reg$Mail9Sent[i] == (Sys.Date()-3) |
        reg$Mail9Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail10Sent[i]) != 1 &&
       is.na(reg$completedLime11[i]) == 1 &&
       is.na(reg$Mail10Rem[i]) == 1 &&
       (reg$Mail10Sent[i] == (Sys.Date()-2) |
        reg$Mail10Sent[i] == (Sys.Date()-3) |
        reg$Mail10Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail11Sent[i]) != 1 &&
       is.na(reg$completedLime12[i]) == 1 &&
       is.na(reg$Mail11Rem[i]) == 1 &&
       (reg$Mail11Sent[i] == (Sys.Date()-2) |
        reg$Mail11Sent[i] == (Sys.Date()-3) |
        reg$Mail11Sent[i] == (Sys.Date()-4))) ||
      (is.na(reg$Mail12Sent[i]) != 1 &&
       is.na(reg$completedLime13[i]) == 1 &&
       is.na(reg$Mail12Rem[i]) == 1 &&
       (reg$Mail12Sent[i] == (Sys.Date()-2) |
        reg$Mail12Sent[i] == (Sys.Date()-3) |
        reg$Mail12Sent[i] == (Sys.Date()-4)))){
    temp <- rbind(temp, reg[i,])
    k <- k+1
  }
}
temp <- temp[-1,] 

if (length(temp)==0){print("No reminders necessary.")} else {
  for (i in 1:length(temp[,1])){
    num <- which(tolower(as.character(reg$tid)) == tolower(as.character(temp$tid[i])))
    if ((is.na(temp$Mail1Rem[i]) == T) && (is.na(temp$completedLime2[i]) == T)){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br />
                         <p>Wir möchten dich eben erinnern, eine kurze Frage zu deinem aktuellen Sprachgebrauch zu beantworten.</p>
                         <p>Es sollte dich nicht mehr als 2 Minuten kosten, und sollte möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink2[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail1Rem[num] <- 1}
    else if ((is.na(temp$Mail2Rem[i]) == T) && (is.na(temp$completedLime3[i]) == T)){
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br />
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink3[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail2Rem[num] <- 1}
    else if ((is.na(temp$Mail3Rem[i]) == T) && (is.na(temp$completedLime4[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br />
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und mglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink4[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail3Rem[num] <- 1}
    else if ((is.na(temp$Mail4Rem[i]) == T) && (is.na(temp$completedLime5[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br />
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink5[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail4Rem[num] <- 1}
    else if ((is.na(temp$Mail5Rem[i]) == T) && (is.na(temp$completedLime6[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br />
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink6[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p>
                         <br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail5Rem[num] <- 1}
    else if ((is.na(temp$Mail6Rem[i]) == T) && (is.na(temp$completedLime7[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink7[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail6Rem[num] <- 1}
    else if ((is.na(temp$Mail7Rem[i]) == T) && (is.na(temp$completedLime8[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink8[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail7Rem[num] <- 1}
    else if ((is.na(temp$Mail8Rem[i]) == T) && (is.na(temp$completedLime9[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink9[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail8Rem[num] <- 1}
    else if ((is.na(temp$Mail9Rem[i]) == T) && (is.na(temp$completedLime10[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink10[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail9Rem[num] <- 1}
    else if ((is.na(temp$Mail10Rem[i]) == T) && (is.na(temp$completedLime11[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink11[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail10Rem[num] <- 1}
    else if ((is.na(temp$Mail11Rem[i]) == T) && (is.na(temp$completedLime12[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink12[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail11Rem[num] <- 1}
    else if ((is.na(temp$Mail12Rem[i]) == T) && (is.na(temp$completedLime13[i]) == T)) {
      html_body <- paste("<html> Liebe(r)",
                         as.character(temp$firstname[i]),
                         ", <br /> 
                         <p>Wir möchten dich noch eben erinnern, ein paar kurze Angaben zu deinem aktuellen Sprachgebrauch zu machen:</p>
                         <p>Wie beim letzten Mal, sollte es nicht mehr als 2 Minuten dauern, und möglichst zeitnah beantwortet werden, am besten sofort, oder <b>spätestens in den nächsten 3 Tagen</b>.</p> 
                         Hier der Link:</b> <br / >",
                         temp$LimeLink13[i], 
                         "</p><p>Viel Spaß und beste Grüße,<br />
                         Anne Mickan</p><br /><br />
                         (Studieninfo: http://www.mpi.nl/experiments/online-experiments/sprachstudie)
                         </html>")
      reg$Mail12Rem[num] <- 1}
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
      debug = F)
    print(paste(i,". reminder sent to: ",as.character(temp$email[i]), sep = ""))
  }}

############################################################
#### Save the updated participant database both as txt  ####
############################################################
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION")
write.table(reg, "REGISTRATION.txt", quote = F, row.names = F, col.names = T, sep = "\t")

