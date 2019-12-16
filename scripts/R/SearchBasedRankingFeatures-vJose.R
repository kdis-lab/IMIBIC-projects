library(caret)
library(rJava)

#library(doMC)
#registerDoMC(cores = 8)

#setwd("D:/Investigacion/IMIBIC/VHC_171128/")

threshold <- 0.8

output <- "results/models/"

#load the dataset and ranking of features
dataset <-  read.csv("VHC_180131.csv")

rankingFeature <- read.csv("VHC_180131-sortedfeatures.csv", sep = "\t")

nFeatures <- nrow(rankingFeature)

classIndex <- ncol(dataset)

algorithms <- c( "J48", "C5.0",  "PART", "rf", "sparseLDA", "LMT")

numberClasses <- length(levels(dataset[,ncol(dataset)]))

metricOpt <- "AUC"
summaryFunction <- multiClassSummary

if(numberClasses == 2)
{
  cat("TWO CLASSES")
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
  
  cat(paste0(algorithm, "\n"))
  
  #The dataframe where we stored the results for this algorithm
  df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), metricOpt=c(0))
  
  df <- df[-1,]
  
  fileName <- paste(output,algorithm,".csv")
  
  #Execute the search by ranking of features
  #######################
  
  #All features will be considered as pivot one time
  for(feature in 1:nFeatures){
    
    cat(paste0("\t", feature, "\n"))
    
    pivotFeature <- rankingFeature[feature,"Index"]
    
    listFeatures <- c(pivotFeature)
    
    aucGlobal <- -1
    
    for(otherFeature in feature:nFeatures)
    {
      cat(paste0("\t\t", otherFeature, "\n"))
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
        #Cache rJava object classifier in order to save it with the object
        .jcache(modelFit$finalModel$classifier)
        saveRDS(modelFit, paste(output,"modelfit-",algorithm,"-", nrow(df),".rds", sep = ""))
        
        if(algorithm == "J48"){
          pdf(paste(output,"tree-",algorithm,"-", nrow(df),".pdf", sep = ""), width = 12, height = 8)
          plot(modelFit$finalModel)
          dev.off()
        }
        else if(algorithm == "PART"){
          write(modelFit$finalModel, file = paste(output,"rules-",algorithm,"-", nrow(df),".txt", sep = ""))
        }
        else if(algorithm == "rf"){
          write(paste(output,"rules-",algorithm,"-", nrow(df),";", modelFit$finalModel$ntree, sep = ""), file = "rf.txt", append = TRUE)
        }
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