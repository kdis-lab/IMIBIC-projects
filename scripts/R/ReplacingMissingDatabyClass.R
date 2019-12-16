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

replaceMissingValues <- function(rawdata, numericImputation = "median", fixedValue=0, kNN = 5){
  
  func <- NULL
  
  if(numericImputation == "median") func <- median
  
  if(numericImputation == "mean") func <- mean
  
  if(numericImputation == "value") func <- fixedValue
  
  if(numericImputation == "kNN") {
    
    rawdata <- knnImputation(rawdata, k = kNN)
    return(rawdata)
  }
  
  classes <-sapply(rawdata, class)
  
  #Always, the last column will be the class
  cls <- unique(rawdata$Class)
  
  #for each class
  for(cl in cls){
    
    subdata <- rawdata[rawdata$Class == cl,]
    
    for(i in 1: (ncol(rawdata)-1)){
      
      if(classes[i]=="numeric")
      { 
        if(is.function(func)){
          
          subdata[is.na(subdata[,i]), i] <- func(subdata[,i], na.rm =TRUE)
        }
        
        else if(is.numeric(func)){
          subdata[is.na(subdata[,i]), i] <- fixedValue
        }
      }
      else if(classes[i]=="factor") #compute the mode
      { 
        ux <- unique(subdata[,i])
        
        subdata[subdata[,i]=="", i] <- ux[which.max(tabulate(match(subdata[,i], ux)))]
        
      }
    }
    
    rawdata[rawdata$Class==cl,] <- subdata
  }
  
  return(rawdata)
}

# load the csv
path <- "D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/tumores-cerebrales/Tumor-IIIvsIV-raw-norm-withoutOutilers"

#load the dataset
dataset <-read.csv(paste(path,".csv", sep = ""), sep = ",")

dataset <- replaceMissingValues(rawdata = dataset, numericImputation = "median")

write.table(dataset, file= paste(path,"-withoutMissingValues.csv",sep = ""),
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE)