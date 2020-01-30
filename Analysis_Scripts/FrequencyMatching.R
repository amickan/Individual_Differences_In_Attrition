German <- read.delim("//ru.nl/wrkgrp/STD-OnlineStudy_DataCoding/German.txt", header = F)
subtlex <- read.delim("U:/PhD/EXPERIMENT 4/SUBTLEX-DE_cleaned_with_Google00.txt", stringsAsFactors = F)

subtlex$lgSUBTLEXN <- gsub(",", ".", subtlex$lgSUBTLEX)

for (i in 1:nrow(German)){
  num <- which(tolower(subtlex$Word) == tolower(German$V1[i]))
  if (length(num)== 0){
    German$SubLog[i] <- NA
    German$SubMln[i] <- NA
  }
  else if (length(num)>1){
    German$SubLog[i] <- NA
    German$SubMln[i] <- NA
  }
  else {
    German$SubLog[i] <-  as.numeric(as.character(subtlex$lgSUBTLEXN[num]))
    German$SubMln[i] <- as.numeric(subtlex$WFfreqcount[num])
  }
}

write.table(German, "Germanfrequencies.txt", quote = F, sep = "\t", row.names = F)
