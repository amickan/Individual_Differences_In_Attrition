### Moving files for Experiment 4

# identify the folder with the to-be-moved files
current.folder <- "//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/T3_download"

### get list of participants 
## Check whether tasks have been completed
setwd("U:/PhD/EXPERIMENT 4/SCRIPTS/ErasmusSpain/ADMINISTRATION")
reg <- read.delim("REGISTRATION.txt", header = T, stringsAsFactors = F)
A <- reg[which(is.na(reg$T3done)==0 & reg$T3done==1),]$token
#A <- A[!is.na(A)]
# B <- 1005 1006 1007 1008 1017 1018 1021 1025 1026 1027 1029 1033 1038 1041 1042 1046 1052 1057 1064 1069 1070 1075 1083 1087 1092 1093 1094 1095 1097 1098 1101 1102 1110 1111 1114 1116 1120 1122 1127 1131 1132 1135 1137 1140 1144 1147 1148 1149 1152 1153 1154 1158 1163 1164 1175 1182


for (i in 1:length(A)){
  pNumber <- A[i]
  new.parent <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/", sep="")
  new.T1 <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", sep="")
  new.folder.org <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", pNumber,"_Original/", sep="")
  new.folder.pic <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", pNumber,"_PicNaming/", sep="")
  new.folder.flu <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", pNumber,"_Fluency/", sep="")
  if (dir.exists(new.parent)){}else{
    dir.create(new.parent)}
  if (dir.exists(new.T1)){}else{
    dir.create(new.T1)}
  if (dir.exists(new.folder.org)){} else {
    dir.create(new.folder.org)}
  if (dir.exists(new.folder.pic)){} else {
    dir.create(new.folder.pic)}
  if (dir.exists(new.folder.flu)){} else {
    dir.create(new.folder.flu)}
  
  # find the files that you want
  files <- as.data.frame(list.files(current.folder, full.names=T))
  colnames(files) <- "FileName"
  ppfiles <- files[grep(pNumber,files$FileName), ]
  
  if (length(ppfiles)==0){} else {
  list.of.files.org <- ppfiles[grep("ogg|webm",ppfiles)]
  list.of.files.pic <- ppfiles[grep("naming*.+?wav",ppfiles)]
  list.of.files.flu <- ppfiles[grep("fluency*.+?wav",ppfiles)]
  
  # copy the files to the new folder
  file.copy(list.of.files.org, new.folder.org, overwrite = T, recursive = T, copy.mode = T)
  file.copy(list.of.files.pic, new.folder.pic, overwrite = T, recursive = T, copy.mode = T)
  file.copy(list.of.files.flu, new.folder.flu, overwrite = T, recursive = T, copy.mode = T)
  
  file.remove(as.character(list.of.files.org))
  file.remove(as.character(list.of.files.pic))
  file.remove(as.character(list.of.files.flu))}
  
  print(pNumber)
  
}

## remove all test files
for (i in 1:length(A)){
  pNumber <- A[i]
  new.folder.pic <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3",pNumber,"_PicNaming/", sep="")
  new.folder.flu <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3",pNumber,"_Fluency/", sep="") 
  
  # find the files that you want
  files <- as.data.frame(list.files(new.folder.pic, full.names=T))
  colnames(files) <- "FileName"
  ppfiles <- files[grep("test",files$FileName), ]
  
  if (length(ppfiles)==0){} else {
    file.remove(as.character(ppfiles))}
  
  # find the files that you want
  files <- as.data.frame(list.files(new.folder.flu, full.names=T))
  colnames(files) <- "FileName"
  ppfiles <- files[grep("test",files$FileName), ]
  
  if (length(ppfiles)==0){} else {
    file.remove(as.character(ppfiles))}
  
  print(pNumber)
}

# check for empty folder 
for (i in 1:length(A)){
  pNumber <- A[i]
  new.folder.org <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", pNumber,"_Original/", sep="")
  new.folder.pic <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", pNumber,"_PicNaming/", sep="")
  new.folder.flu <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/", pNumber,"/",pNumber,"_T3/", pNumber,"_Fluency/", sep="")
  
  if(length(list.files(new.folder.pic)) ==0){
    print(pNumber)
  } else if (length(list.files(new.folder.flu)) ==0){
    print(pNumber)
  }
}

# move control participants to folders
### get list of participants 
## Check whether tasks have been completed
control <- read.delim(here("ADMINISTRATION","CONTROL_REGISTRATION.txt"), header = T, stringsAsFactors = F)
A <- control[which(is.na(control$Done)==0 & control$Done==1),]$token

for (i in 1:length(A)){
  pNumber <- A[i]
  new.parent <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/CONTROL/", pNumber,"/", sep="")
  new.folder.org <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/CONTROL/", pNumber,"/", pNumber,"_Original/", sep="")
  new.folder.flu <- paste("//cnas.ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/CONTROL/", pNumber,"/", pNumber,"_Fluency/", sep="")
  if (dir.exists(new.parent)){}else{
    dir.create(new.parent)}
  if (dir.exists(new.folder.org)){} else {
    dir.create(new.folder.org)}
  if (dir.exists(new.folder.flu)){} else {
    dir.create(new.folder.flu)}
  
  # find the files that you want
  files <- as.data.frame(list.files(current.folder, full.names=T))
  colnames(files) <- "FileName"
  ppfiles <- files[grep(pNumber,files$FileName), ]
  
  if (length(ppfiles)==0){} else {
    list.of.files.org <- ppfiles[grep("ogg|webm",ppfiles)]
    list.of.files.flu <- ppfiles[grep("fluency*.+?wav",ppfiles)]
    
    # copy the files to the new folder
    file.copy(list.of.files.org, new.folder.org, overwrite = T, recursive = T, copy.mode = T)
    file.copy(list.of.files.flu, new.folder.flu, overwrite = T, recursive = T, copy.mode = T)
    
    file.remove(as.character(list.of.files.org))
    file.remove(as.character(list.of.files.flu))}
  
  print(pNumber)
  
}
