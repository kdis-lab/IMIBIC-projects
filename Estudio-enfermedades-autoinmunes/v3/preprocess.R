library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/v3")

fileT <- "Neutro"

dataset <- read.csv(paste(fileT,".csv",sep = ""), na.strings = "")

threshold <- 0.3

cnames <- colnames(dataset)[1:45]

for(index in cnames){
  
  sumaNA<-sum(is.na(dataset[,index]))
  
  indexesCeros <- dataset[,index]==0 & !is.na(dataset[,index])
  
  sumacero <- sum(indexesCeros)
  
  to <- sumaNA+sumacero
  
  #Remove the factor if the number of NA and ceros is greater than the threshold
  if(to/nrow(dataset) >= threshold){
    
    print("Removing...")
    print(paste(index,": NA (", sumaNA, ") ", "Ceros (", sumacero , ") percent:", to/nrow(dataset)))
  
    dataset[,index] <-  NULL
  }
  else{
    #removing Os
    dataset[indexesCeros, index] <- NA
  }
}

methods <- c("zv","nzv","YeoJohnson","center","scale","bagImpute")

preProcValues <- preProcess(dataset, method = methods)

datasetTransformed <- predict(preProcValues, dataset)

#detecting inconsistent or dupplicated examples

datasetTransformed <- datasetTransformed[!duplicated(datasetTransformed[,1:(ncol(datasetTransformed)-1)]),]

write.table(datasetTransformed, file= paste(fileT, "-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")