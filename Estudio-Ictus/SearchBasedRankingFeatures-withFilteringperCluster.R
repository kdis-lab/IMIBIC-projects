library(caret)

library(doMC)
registerDoMC(cores = 2)

setwd("/media/ogreyesp/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

threshold <- 1

output <- "results/ranking-based-Classification-withFilteringperCluster/"

#load the dataset and ranking of features
dataset <-  read.csv("Ictus-preproc-withoutOutliers.csv")

datasetCluster <- read.csv("results/ranking-based-Clustering/Hierarchical-Clustering.csv")

rankingFeature <- read.csv("results/feature-selection/sortedfeatures-reduced.csv", sep = "\t")

nFeatures <- nrow(rankingFeature)

classIndex <- ncol(dataset)

# We only used a Random Forest and a logistic regression, and we selected only those models that had an AUC equal
# to 1 in both algorithms RandomForest and LogisticRegression

set.seed(123)

form <- as.formula(paste(colnames(dataset)[classIndex], " ~ ."))

fitControl <- trainControl(method="LOOCV", classProbs = TRUE,
                           savePredictions = TRUE, search="grid", allowParallel= TRUE, 
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

#The dataframe where we stored the results for this algorithm
  df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), AUCRF=c(0), AUCLogistic=c(0))
  
  df <- df[-1,]
  
  fileName <- paste(output,"models.csv")
  
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
      #We evaluate a maximum of 25 combinations of different parameter values
      
      #executing random forest
      modelFit <- train(form, data = subdataset,
                        method= "glm",
                        metric = "ROC",
                        maximize = TRUE,
                        tuneLength = 25,
                        trControl = fitControl, family="binomial")
      
      bestTuneIndex <- as.numeric(rownames(modelFit$bestTune)[1])
      
      auc <- modelFit$results[bestTuneIndex, "ROC"]
      
      if(auc >= threshold){
        
        #then pass to check the Logistic Regression
        
        modelFit <- train(form, data = subdataset,
                          method= "rf",
                          metric = "ROC",
                          maximize = TRUE,
                          tuneLength = 25,
                          trControl = fitControl)
        
        bestTuneIndex <- as.numeric(rownames(modelFit$bestTune)[1])
        
        auc2 <- modelFit$results[bestTuneIndex, "ROC"]
        
        #register the model
        if(auc2 >= threshold){
          
          vars <- predictors(modelFit)
          
          #Check whether the combination of proteins exist in the cluster's results
          if(nrow(datasetCluster[as.character(datasetCluster$Atts) == paste(vars, collapse = " "),])){
            
            df <- rbind(df, data.frame(ID = nrow(df) + 1, NumberAtts= length(vars), 
                                       Atts= paste(vars, collapse = ' '), AUCRF=auc2, AUCLogistic=auc))
            
            #Saving the model for graphing plots a posteriory if it is necessary
            saveRDS(modelFit, paste(output, "modelfit-", nrow(df), ".rds", sep = ""))          
          }
        }
      }
      
      if(aucGlobal <= auc){
        
        aucGlobal <- auc
      }
      else{
        #remove the last feature added due to did not apport any advantage
        listFeatures <- listFeatures[1:(length(listFeatures)-1)]
      }
    }
  }

  #######################End of search by ranking
  
  #order the rows of the dataframe
  df <- df[order(-df[,ncol(df)]), ]
  
  #save the dataframe
  write.table(df, file= fileName, quote = FALSE, sep="," , row.names = FALSE, 
              col.names = TRUE)