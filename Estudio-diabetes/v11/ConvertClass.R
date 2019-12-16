fileNames <- c(#"Year1vsRest-0h", "Year1vsRest-4h", "Year1vsRest-0h-4h", "Year1+Year2vsRest-0h", "Year1+Year2vsRest-4h", "Year1+Year2vsRest-0h-4h"
  #"AllDiabetes-0h-4h-FINDRISk", "AllDiabetes-0h-4h-Hba1c", "Year1-0h-4h-FINDRISK", "Year1-0h-4h-Hba1c", "Year2-0h-4h-FINDRISk", "Year2-0h-4h-Hba1c"
  #"AllDiabetes-0h-4h", "AllDiabetes-0h", 
  #"AllDiabetes-4h"
  #"DiabetesAll-FindRISK", "DiabetesAll-HbA1c", "Year1-HbA1c", "Year1-FindRISK"
  "Year2-HbA1c", "Year2-FindRISK")

for(fileName in fileNames){
  
  dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ",", row.names = 1)
  
  #Convert class in factor
  dataset[dataset$Class==0,"Class"] <- "N"
  dataset[dataset$Class==1,"Class"] <- "S"
  
  write.table(dataset, file=paste0(fileName,"v2.csv"), row.names = T, col.names = T, quote = F, sep = ",")
  
}