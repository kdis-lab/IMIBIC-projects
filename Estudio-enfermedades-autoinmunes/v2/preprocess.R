library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/v2")

fileT <- "Neutro"

dataset <- read.csv(paste(fileT,".csv",sep = ""))

for(index in 1:45){
  
  if(sum(is.na(dataset[,index]))>0)
  {
    dataset[is.na(dataset[,index]),index] <- median(dataset[,index], na.rm = TRUE)
    
  }
}

methods <- c("zv","nzv","YeoJohnson","center","scale")

preProcValues <- preProcess(dataset, method = methods)

datasetTransformed <- predict(preProcValues, dataset)

#detecting inconsistent or dupplicated examples

datasetTransformed <- datasetTransformed[!duplicated(datasetTransformed[,1:(ncol(datasetTransformed)-1)]),]

write.table(datasetTransformed, file= paste(fileT, "-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")