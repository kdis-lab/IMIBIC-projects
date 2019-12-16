library(caret)
library(ggplot2)
library(plotROC)
library(pROC)
library(ROCR)

library(doMC)
registerDoMC(cores = 2)

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/diabetes/v4/")

threshold <- 0.7

plotROC <- TRUE

output <- "results/Classification-withoutRNU1/"

#load the dataset and ranking of features
dataset <- read.csv("Diabetes-withoutRNU1-preproc.csv")

rankingFeature <- read.csv("Diabetes-withoutRNU1-preproc-sortedfeatures.csv", sep = "\t")

nFeatures <- nrow(rankingFeature)

classIndex <- ncol(dataset)

set.seed(123)

form <- as.formula(paste(colnames(dataset)[ncol(dataset)]," ~ ."))

fitControl <- trainControl(method="repeatedcv", number = 10,
                           repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE,
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

  #The dataframe where we stored the results for this algorithm
  df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), metricOpt=c(0))
  
  df <- df[-1,]
  
  fileName <- paste(output,"LogisticRegression.csv")
  
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
      modelFit <- train(form, data = subdataset,
                        method="glm",
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, family="binomial")
      
      auc <- modelFit$results[1, "ROC"]
      
      #register the model
      if(auc>= threshold){
        
        vars <- predictors(modelFit)
        
        df <- rbind(df, data.frame(ID = nrow(df) + 1, NumberAtts= length(vars), Atts= paste(vars, collapse = ' '), metricOpt=auc))
        
        #Saving the model for graphing plots a posteriory if it is necessary
        saveRDS(modelFit, paste(output,"modelfit-LogisticRegression","-", nrow(df),".rds", sep = ""))
        
        #######################To generate the ROC curve of the best model.
        if(plotROC){
          
          g <- ggplot(modelFit$pred, aes(m = S, d=factor(obs, levels = c("S", "N")))) + 
           geom_roc(n.cuts=0) + coord_equal() + style_roc()
          
          g + annotate("text", x=0.85, y=0.15, label=paste("AUC =", modelFit$results[1,"ROC"]))
          
          ggsave(width = 8, height = 5, filename = paste(output,"modelfit-LogisticRegression","-", nrow(df),".png",sep = ""), bg = "transparent") 
          
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