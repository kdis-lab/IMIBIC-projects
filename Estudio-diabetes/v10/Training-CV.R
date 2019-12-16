library(caret)
library(pROC)
library(doMC)
registerDoMC(cores = 2)

output <- "results/"

fileNames <- c("Year1vsRest-0h", "Year1vsRest-4h", "Year1vsRest-0h-4h", "Year1+Year2vsRest-0h", "Year1+Year2vsRest-4h", "Year1+Year2vsRest-0h-4h")

algorithmNames <-c("glm", "rf", "J48")

fitControl <- trainControl(method="repeatedcv", number = 10, repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE,
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula("Class ~ .")

# Seed for reproducibility
set.seed(123)

for(fileName in fileNames){
  
  dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ",", row.names = 1)
  
  for(alg in algorithmNames){
    
    outputT <- paste0(output, alg,"/")
    
    if(!dir.exists(outputT)){
      dir.create(outputT, recursive = T)
    }
    
    if(alg=="glm"){
      
      modelFit <- train(form, data = dataset,
                        method="glm",
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, family="binomial")    
    }
    
    if(alg=="rf"){
      
      modelFit <- train(form, data = dataset,
                        method="rf",
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, 
                        tuneLength = 30)
    }
    
    plsProbs <- predict(modelFit, newdata = dataset, type = "prob") # Predict over the same training data
    
    rocT <- roc(dataset$Class, plsProbs$S) # Draw ROC curve.
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT, fileName,'/AUC-TrainingSet.png', sep = ""))
    dev.off()
    
    if(alg=="glm"){
      predictions <- modelFit$pred
    }
    
    if(alg=="rf"){
      # Extracting the best tune
      predictions <- modelFit$pred[modelFit$pred$mtry == modelFit$bestTune$mtry,]
    }
    
    response <- as.numeric(modelFit$trainingData$.outcome=='S')
    predictor <- aggregate(S ~ rowIndex, predictions, mean)[,'S']
    
    rocT <- roc(response, predictor)
    
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT, fileName,'/AUC-CV-FullDataset.png', sep = ""))
    dev.off()
    
    # Save the CV residuals
    cvResiduals <- data.frame(class=response, prediction=predictor)
    
    #save the dataframe
    write.table(cvResiduals, file= paste0(outputT, fileName, "/CV-FullDataset.csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
  }
}