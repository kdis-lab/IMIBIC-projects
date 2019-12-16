library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/")

dataset <- read.csv("datasets/cancer/v3/TCGAOscar.csv")

colNames <- colnames(dataset)

form <- as.formula(paste(colNames[ncol(dataset)]," ~ ."))

fitControl <- trainControl(method="repeatedcv", number = 10,
                           repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, search="random", allowParallel= TRUE, 
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

outputNuestraCohorte <- "reports/cancer/v3/NuestraCohorteZscore/Classification/"
outputTCGA <- "reports/cancer/v3/TCGAOscar/Classification/"

#List of algorithms to evaluate
algorithms <- c("rf", "adaboost", "C5.0", "cforest", "ctree", "earth", "fda", "gamboost", "gbm", "glmStepAIC", "LMT",
                "LogitBoost", "msaenet", "nodeharvest", "PART", "RRF", "sdwd", "sparseLDA")

aucThreshold <- 0.9
numberFactorThresold <- 5

#Evaluate each algorithm with its combination in TCGA
for(algorithm in algorithms){
  
  combinationsDF <- read.csv(paste(outputNuestraCohorte,algorithm,".csv", sep = ""))
  
  #Filter the combinations
  combinationsDF <- combinationsDF[combinationsDF$NumberAtts <= numberFactorThresold & combinationsDF$metricOpt >= aucThreshold,]
  
  #The dataframe where we stored the results for this algorithm
  dfTCGA <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), metricOpt=c(0))
  dfTCGA <- dfTCGA[-1,]
  
  if(nrow(combinationsDF)>0){
    
    #Evaluate each combination
    for(comb in 1:nrow(combinationsDF)){
      
      factorList <- combinationsDF[comb, "Atts"]
      
      items <- unlist(strsplit(as.character(factorList), " "))
      
      indexes <- match(items, colNames)
      
      subdataset <- dataset[, c(indexes,ncol(dataset))]
      
      set.seed(123)
      
      modelFit <- train(form, data = subdataset,
                        method= algorithm,
                        metric = "ROC",
                        maximize = TRUE,
                        tuneLength = 30,
                        trControl = fitControl)
      
      bestIndex <- as.numeric(rownames(modelFit$bestTune)[1])
      
      auc <- modelFit$results[bestIndex,"ROC"]
      
      dfTCGA <- rbind(dfTCGA, data.frame(ID = nrow(dfTCGA) + 1, NumberAtts= length(indexes), Atts= factorList, metricOpt=auc))
      
      #Saving the model for graphing plots a posteriory if it is necessary
      saveRDS(modelFit, paste(outputTCGA,"modelfit-",algorithm,"-", nrow(dfTCGA),".rds", sep = ""))
      
    }
  }
  
  #order the rows of the dataframe
  dfTCGA <- dfTCGA[order(-dfTCGA[,ncol(dfTCGA)]), ]
  
  #save the dataframe
  write.table(dfTCGA, file= paste(outputTCGA, algorithm, ".csv", sep = ""), quote = FALSE, sep="," , row.names = FALSE, 
              col.names = TRUE)
}


