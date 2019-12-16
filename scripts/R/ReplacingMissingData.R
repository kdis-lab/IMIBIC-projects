library(DMwR)

## Deleting the observations

#If you have large number of observations in your dataset, where all the classes 
#to be predicted are sufficiently represented in the training data, then try deleting 
# those observations (rows) that contain missing values. Make sure after deleting the 
#observations, you have:
  
#1. Have sufficent data points, so the model doesn't lose power.
#2. Not to introduce bias (meaning, disproportionate or non-representation of classes).
removeObservationsWithMissingValues <- function(rawdata){
  return (na.omit(rawdata))
}

removeObservationsByIndexes <- function(rawdata, vectorIndexes){
  return (rawdata[-vectorIndexes,])
}

##Deleting the variable

#If a particular variable is having more missing values that rest of the variables in the 
#dataset, and, if by removing that one variable you can save many observations. I would, 
#then, suggest to remove that particular variable, unless it is a really important predictor
#that makes a lot of business sense. It is a matter of deciding between the importance of 
#the variable and losing out on a number of observations.

deleteVariablesByVarIndexes <- function(rawdata, vectorIndexes){
  return (rawdata[-vectorIndexes])
}

deleteVariablesByVarNames <- function(rawdata, vectorNames){
  return (rawdata[ , !(names(rawdata) %in% vectorNames)])
}

## Imputation with mean / median / mode/ kNN

#Replacing the missing values with the mean / median / mode is a crude way of treating 
#missing values. Depending on the context, like if the variation is low or if the variable
#has low leverage over the response, such a rough approximation is acceptable and could
#possibly give satisfactory results.

# knnImputation uses k-Nearest Neighbours approach to impute missing values. What kNN 
# imputation does in simpler terms is as follows: For every observation to be imputed, 
# it identifies closest observations based on the euclidean distance and computes 
#the weighted average (weighted based on distance) of these 'k' obs.

replaceMissingValues <- function(rawdata, numericImputation = "median", vectorVar, fixedValue=0, kNN = 5){
  
  func <- NULL
  
  if(numericImputation == "median") func <- median
  
  if(numericImputation == "mean") func <- mean
  
  if(numericImputation == "value") func <- fixedValue
  
  if(numericImputation == "kNN") {
    
    rawdata <- knnImputation(rawdata, k = kNN)
    return(rawdata)
  }
  
  classes <-sapply(rawdata, class)
  
  for(i in vectorVar){
    
    if(classes[i]=="numeric")
    { 
      if(is.function(func)){
         rawdata[is.na(rawdata[,i]), i] <- func(rawdata[,i], na.rm =TRUE)
      }
     
       else if(is.numeric(func)){
        rawdata[is.na(rawdata[,i]), i] <- fixedValue
      }
    }
    else if(classes[i]=="factor") #compute the mode
    { 
      ux <- unique(rawdata[,i])
            
      rawdata[rawdata[,i]=="", i] <- ux[which.max(tabulate(match(rawdata[,i], ux)))]
      
    }
  }
  
  return(rawdata)
}

# load the csv
path <- "/home/oscar/workspace/IMIBIC/datasets/neuroendocrino/splicingT"

#load the dataset
dataset <-read.csv(paste(path,".csv", sep = ""))

dataset <- replaceMissingValues(rawdata = dataset, numericImputation = "kNN")

write.table(dataset, file= paste(path,"-withoutMissingValues.csv",sep = ""),
            quote = TRUE, sep="," , row.names = FALSE, col.names = TRUE)
