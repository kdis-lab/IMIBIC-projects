library(DMwR)

## Imputation with mean / median / mode/ kNN

#Replacing the missing values with the mean / median / mode is a crude way of treating 
#missing values. Depending on the context, like if the variation is low or if the variable
#has low leverage over the response, such a rough approximation is acceptable and could
#possibly give satisfactory results.

# knnImputation uses k-Nearest Neighbours approach to impute missing values. What kNN 
# imputation does in simpler terms is as follows: For every observation to be imputed, 
# it identifies closest observations based on the euclidean distance and computes 
#the weighted average (weighted based on distance) of these 'k' obs.

replaceMissingValues <- function(rawdata, kNN = 10){
  
  #Always, the last column will be the class
  cls <- levels(rawdata$Class)
  
  #for each class
  for(cl in cls){
    
    subdata <- rawdata[rawdata$Class == cl,]
    
    rawdataImp <- knnImputation(subdata, scale = T, k = kNN, meth = "weighAvg")
    
    rawdata[rawdata$Class==cl,] <- rawdataImp
  }
  
  return(rawdata)
}

# load the csv
path <- "F:/DATA/backup-29-03-2019/workspace/IMIBIC-projects/Estudio-próstata/v6/parafinas"

#load the dataset
dataset <-read.csv(paste0(path,".csv"), sep = ",", na.strings = "")

dataset <- replaceMissingValues(rawdata = dataset)

write.table(dataset, file= paste0(path,"-withoutMissingValues.csv"),
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE)