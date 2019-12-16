library(caret)

library(doMC)
#registerDoMC(cores = 5)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/v3/")

threshold <- 0.85

output <- "results/"

#load the dataset and ranking of features
dataset <-  read.csv("NuestraCohorteZscore.csv")

rankingFeature <- read.csv("NuestraCohorteZscore-sortedfeatures.csv", sep = "\t")

nFeatures <- nrow(rankingFeature)

classIndex <- ncol(dataset)

algorithmsAbleMulti <- c("rf", "AdaBoost.M1", "AdaBag", "bagFDA", "bagEarth", "LogitBoost", "J48", "C5.0", "cforest", 
                         "ctree", "xgbLinear", "xgbTree", "fda", "glmnet",
                         "LMT", "msaenet", "earth", "pam",
                         "JRip", "PART", "RRF", "OneR", "sdwd", "sparseLDA", "gbm", "wsrf")

algorithmsTwoClassesOnly <- c("adaboost", "bartMachine", "ada", "gamboost", "glmStepAIC", 
                              "nodeHarvest")

numberClasses <- length(levels(dataset[,ncol(dataset)]))

algorithms <- algorithmsAbleMulti
metricOpt <- "AUC"
summaryFunction <- multiClassSummary

if(numberClasses == 2)
{
  algorithms <- c(algorithms, algorithmsTwoClassesOnly)
  metricOpt <- "ROC"  
  summaryFunction <- twoClassSummary
}

set.seed(123)

form <- as.formula(paste(colnames(dataset)[ncol(dataset)]," ~ ."))

fitControl <- trainControl(method="repeatedcv", number = 10,
                           repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, search="random", allowParallel= TRUE, 
                           summaryFunction = summaryFunction, verboseIter = FALSE)

#execute the algorithms
for(algorithm in algorithms){
  
  #The dataframe where we stored the results for this algorithm
  df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), metricOpt=c(0))
  
  df <- df[-1,]
  
  fileName <- paste(output,algorithm,".csv")
  
  #Execute the search by ranking of features
  #######################
  
  #All features will be considered as pivot one time
  for(feature in 1:nFeatures){
    
    pivotFeature <- rankingFeature[feature,"Index"]
    
    listFeatures <- c(pivotFeature)
    
    aucGlobal <- -1
    
    for(otherFeature in feature:nFeatures)
    {
      # The last case
      if(otherFeature != feature)
      {
        listFeatures <- c(listFeatures, rankingFeature[otherFeature,"Index"])
      }
      
      #extract the subset
      subdataset <- dataset[, c(listFeatures, classIndex)]
      
      #pass this subdataset to the classification algorithm
      #We evaluate a maximum of 50 combinations of different parameter values
      modelFit <- train(form, data = subdataset,
                        method=algorithm,
                        metric = metricOpt,
                        maximize = TRUE,
                        tuneLength = 30,
                        trControl = fitControl)
      
      bestTuneIndex <- as.numeric(rownames(modelFit$bestTune)[1])
      
      auc <- modelFit$results[bestTuneIndex, metricOpt]
      
      #register the model
      if(auc>= threshold){
        
        vars <- predictors(modelFit)
        
        df <- rbind(df, data.frame(ID = nrow(df) + 1, NumberAtts= length(vars), Atts= paste(vars, collapse = ' '), metricOpt=auc))
        
        #Saving the model for graphing plots a posteriory if it is necessary
        saveRDS(modelFit, paste(output,"modelfit-",algorithm,"-", nrow(df),".rds", sep = ""))
      }
      
      if(aucGlobal < auc){
        
        aucGlobal <- auc
      }
      else{
        #remove the last feature added. due to did not apport any advantage
        listFeatures <- listFeatures[1:(length(listFeatures)-1)]
      }
    }
  }

  #######################End of search by ranking
  
  #order the rows of the dataframe
  df <- df[ order(-df[,ncol(df)]), ]
  
  #save the dataframe
  write.table(df, file= fileName, quote = FALSE, sep="," , row.names = FALSE, 
              col.names = TRUE)
}