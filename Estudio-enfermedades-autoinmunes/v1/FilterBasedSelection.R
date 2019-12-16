library(FSelector)
library(caret)
library(bclust)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/")

fileName <- "PreVSPostNeutrofilos-preproc"

mydata <- read.csv(paste(fileName,".csv",sep = ""))

# To create a stratified repeated k-fold cross validation. For doing a LOOCV it is needed to set the parameter 
# k equal to the number of samples in the dataset

#For replicability
set.seed(123)

folds <- nrow(mydata)
ntimes <- 1

multiIndexes <- createMultiFolds(y = mydata[, ncol(mydata)], k = folds, times = ntimes)

weigthsAverage <- NULL

methodNames <- c("information.gain", "gain.ratio", "symmetrical.uncertainty", "cfs", "consistency", "relief",
                 "oneR", "random.forest.importance", "chi.squared")

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
    
    if(method == "random.forest.importance")
    {
      weigths1 <- f(nameFormula, subdata, importance.type = 1)
      weigths2 <- f(nameFormula, subdata, importance.type = 2)
      
      weigths$attr_importance <- rank(weigths1$attr_importance) + rank(weigths2$attr_importance)
      times <- times +2
      rm(weigths1, weigths2)
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
    
    if(method == "information.gain" || method == "gain.ratio" || method == "symmetrical.uncertainty" || 
       method == "oneR" || method == "chi.squared"){
      
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
newData <- data.frame(Index = 0:(length(colNames)-1), Name= colNames, Importance = weigthsAverage$attr_importance)

newData <- newData[order(-newData$Importance),]

write.table(newData, file = paste("results/", fileName,"-sortedfeatures.txt",sep = ""), quote = FALSE, sep="\t", 
            row.names = FALSE, col.names = FALSE)

png(paste("results/", fileName,"-VarImp.png",sep = ""), width = 8, height = 6, units="in", res=200)

viplot(newData$Importance,xlab = as.vector(newData$Name),
       col=heat.colors(length(newData$Importance)), xlab.mar = 7)

dev.off()