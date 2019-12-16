library(FSelector)
library(caret)

output <- "results/"

fileNames <- c("parafinas-withoutMissingValues")

#For each dataset
for(fileName in fileNames){
  
  path <- paste0(output,"/", fileName, "/")
  
  if(!dir.exists(path)){
    dir.create(path, recursive = T)
  }

  mydata <- read.csv(paste0(fileName,".csv"), sep = ",")
  
  # For doing a LOGCV
  
  #For replicability
  set.seed(123)
  
  groups <- c(1:(nrow(mydata)/2), 1:(nrow(mydata)/2))
  
  multiIndexes <- groupKFold(groups)
  
  weigthsAverage <- NULL
  
  methodNames <- c("information.gain", "gain.ratio", "symmetrical.uncertainty", "cfs", "consistency", "relief",
                   "chi.squared")
  
  classColumnName <- colnames(mydata)[ncol(mydata)]
  colNames <- colnames(mydata)[-ncol(mydata)]
  
  nameFormula <- as.formula(paste(classColumnName," ~ .", sep = ""))
  
  times <- 0
  
  #We take the rank values as weights
  
  for(indexes in multiIndexes){
      
    subdata <- mydata[indexes,]
    
    numberSamples <- nrow(subdata)
    
    for(method in methodNames){
      
      f <- match.fun(method)
      
      if(method == "relief"){
        
        weigthsReliefF <- NULL
        
        #Repeat the Relief algorithm for various k
        for(k in 5:10){
          
          weigths <- f(nameFormula, subdata, neighbours.count = k, sample.size = numberSamples)
          weigths$attr_importance <- rank(weigths$attr_importance)
          
          if(is.null(weigthsReliefF)){
            weigthsReliefF <- weigths
          }
          else{
            weigthsReliefF <- weigthsReliefF + weigths
          }
          times <- times +1
        }
        
        weigths <- weigthsReliefF
        
        rm(weigthsReliefF)
      }
      
      if(method == "cfs"|| method == "consistency"){
        
        subset <- f(nameFormula, subdata)
        
        ind <- match(subset,colNames)
        
        # Only the atts that belong to the subset are set to 1, otherwise are set to 0.
        weigths <- data.frame(attr_importance= rep(0, length(colNames)))
        
        rownames(weigths) <- colNames
        
        weigths[ind, "attr_importance"] <- 1
        
        weigths$attr_importance <- rank(weigths$attr_importance)
        
        times <- times +1
      }
      
      if(method == "information.gain" || method == "gain.ratio" || method == "symmetrical.uncertainty" || method == "chi.squared"){
        
        weigths <- f(nameFormula, subdata)
        weigths$attr_importance <- rank(weigths$attr_importance)
        times <- times +1
      }
  
      if(is.null(weigthsAverage)){
        weigthsAverage <- weigths
      }
      else{
        weigthsAverage <- weigthsAverage + weigths
      }
    }
  }
  
  weigthsAverage <- weigthsAverage / (times*length(colNames))
  
  rm(weigths)
  
  #Constructing the new data
  newData <- data.frame(Index = 1:length(colNames), Var= colNames, Importance = weigthsAverage$attr_importance)
  
  newData <- newData[order(-newData$Importance),]
  
  write.table(newData, file = paste0(path, fileName, "-sortedfeatures.csv"), quote = FALSE, sep="\t",
              row.names = FALSE, col.names = TRUE)
}
