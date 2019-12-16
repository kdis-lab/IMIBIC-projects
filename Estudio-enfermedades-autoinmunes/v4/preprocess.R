library(caret)

removeSparseVars <- function(dataset, init, end, removeCeros = FALSE){

  threshold <- 0.6
  
  cnames <- colnames(dataset)[init:end]
  
  for(index in cnames){
    
    sumaNA <- sum(is.na(dataset[,index]))
    
    sumacero <- 0
    indexesCeros <- c()
    
    if(removeCeros){
      
      indexesCeros <- dataset[,index] == 0 & !is.na(dataset[,index])
      
      sumacero <- sum(indexesCeros)      
    }
    
    to <- sumaNA + sumacero
    
    #Remove the factor if the number of NA and ceros is greater than the threshold
    if(to/nrow(dataset) >= threshold){
      
      print("Removing...")
      print(paste(index,": NA (", sumaNA, ") ", "Ceros (", sumacero , ") percent:", to/nrow(dataset)))
      
      dataset[,index] <-  NULL
    }
    else if(removeCeros){
     #removing Os
     dataset[indexesCeros, index] <- NA
    }
  }
  return(dataset)
}

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/v4/")

fileT <- "Neutro"

dataset <- read.csv(paste(fileT,".csv",sep = ""), na.strings = "")

#removing var Edad
dataset[,"Edad"] <- NULL

methods <- c("zv", "nzv", "center","scale","YeoJohnson","medianImpute")

#Creating a separate dataset for each categorical variable.
varSet <- (ncol(dataset)-5):ncol(dataset)

for(i in varSet){
  
  #Extract the subset
  datasetTrans <- dataset[,c(1:(ncol(dataset)-6),i)]
  
  #removing the examples with NA in the categoric var
  datasetTrans <- datasetTrans[!is.na(datasetTrans[,ncol(datasetTrans)]),]
  
  #remove the sparse vars
  datasetTrans <- removeSparseVars(datasetTrans, 1, ncol(datasetTrans)-1, removeCeros = TRUE)
  
  # Making the preprocessing steps
  preProcValues <- preProcess(datasetTrans, method = methods)
  
  datasetTrans <- predict(preProcValues, datasetTrans)
  
  #detecting inconsistent or dupplicated examples
  datasetTrans <- datasetTrans[!duplicated(datasetTrans[,1:(ncol(datasetTrans)-1)]),]
  
  write.table(datasetTrans, file= paste(fileT, colnames(datasetTrans)[ncol(datasetTrans)], "-preproc.csv",sep = ""),
              quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
}