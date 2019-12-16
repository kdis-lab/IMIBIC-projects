library(caret)
library(pROC)
library(doMC)
registerDoMC(cores = 2)

setwd("/media/ogreyesp/DATA/workspace/IMIBIC/datasets/diabetes/v9")

output <- "results/"

fileNames <- c("0h-4h")

algorithmNames <-c("glm", "rf")

fitControl <- trainControl(method="repeatedcv", number = 10, repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE,
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula("DIABETES ~ .")

# Seed for reproducibility
set.seed(123)

for(fileName in fileNames){
  
  dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ";", row.names = 1)
  
  for(alg in algorithmNames){
    
    outputT <- paste0(output, fileName, "/", alg, "/")
    
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
    
    rocT <- roc(dataset$DIABETES, plsProbs$S) # Draw ROC curve.
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT, '-AUC-overTrainingSet.png', sep = ""))
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
    
    dev.copy(png, paste0(outputT, 'AUC-CV-FullDataset.png'))
    dev.off()
    
    # Save the CV residuals
    cvResiduals <- data.frame(class=response,prediction=predictor)
    
    #save the dataframe
    write.table(cvResiduals, file= paste0(outputT, "CV-FullDataset.csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
  }
}