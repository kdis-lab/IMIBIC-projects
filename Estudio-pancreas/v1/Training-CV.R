rm(list=ls())
library(caret)
library(pROC)
library(rJava)
#library(doMC) it does not work with J48
#registerDoMC(cores = 2)

# Seed for reproducibility
set.seed(123)

evaluationMethod <- "LOOCV" #CV

output <- "results/"

fileNames <- c("NormalvsLesionvsTumor", "NormalvsTumor")

algorithmNames <-c(# "rf", "J48"
                   "PART", "LMT")

tuneLenght <-c(glmnet=5, rf=30, J48=5, PART= 5, LMT= 5)

form <- as.formula("Class ~ .")

for(fileName in fileNames){
  
  dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ",", row.names = 1)
  
  numberClasses <- length(levels(dataset[,ncol(dataset)]))
  
  if(numberClasses==2)
  {
    functionSummary <- twoClassSummary
    metric <- "ROC"
  }
  else{
    functionSummary <- multiClassSummary
    metric <- "AUC"
  }
  
  if(evaluationMethod=="LOOCV"){
      fitControl <- trainControl(method="LOOCV", classProbs = TRUE, savePredictions = TRUE, allowParallel= TRUE,
                                 summaryFunction = functionSummary, verboseIter = FALSE) 
  } else{
      fitControl <- trainControl(method="repeatedcv", repeats = 3, number = 10 ,classProbs = TRUE, savePredictions = TRUE, allowParallel= TRUE,
                                 summaryFunction = functionSummary, verboseIter = FALSE) 
  }

  for(alg in algorithmNames){
    
    outputT <- paste0(output, alg,"/", fileName, "/")
    
    if(!dir.exists(outputT)){
      dir.create(outputT, recursive = T)
    }
    
   modelFit <- train(form, data = dataset,
                        method=alg,
                        metric = metric,
                        maximize = TRUE,
                        trControl = fitControl, 
                        tuneLength = tuneLenght[[alg]], preProcess = c("zv","center", "scale"))
 
    # Predict over the same training data.
    # type of response
    if(alg=="glmnet" || alg == "rf" || alg== "J48" || alg== "PART" || alg== "LMT"){
      typeResponse <- "raw"
    }
    else{typeResponse <- "response"}
   
    predictorNominal <- predict(modelFit, newdata = dataset, type = typeResponse)
    predictorNumeric <- as.numeric(predictorNominal)
    truthNumeric <- as.numeric(dataset$Class)
    
    if(numberClasses == 2)
    {
      rocT <- roc(truthNumeric, predictorNumeric) 
    }else{
      rocT <- multiclass.roc(truthNumeric, predictorNumeric) 
    }
    
    # Save the residuals to generate ROC Curve. Esto para los biologos.
    predictorProbs <- predict(modelFit, newdata = dataset, type = "prob")
    
    residuals <- data.frame(class=dataset$Class)
    residuals <-cbind(residuals, predictorProbs)
    
    #save the dataframe
    write.table(residuals, file= paste0(outputT, "TrainingSet-residuals.csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
    
    #Compute the metrics
    dataResults <- data.frame("AUC"= rocT$auc[1])
    
    #Write the results in a file
    write.table(dataResults, file = paste0(outputT, "TrainingSet-results.csv"), quote = F, sep = "\t", col.names = T, row.names = F)

    # This case is for generate the results of CV process. Extraction of the internal predictions and final results
    
    tuneParameters <- colnames(modelFit$bestTune)
    
    predictions <- modelFit$pred
    results <- modelFit$results
    
    # to filter the best predictions and results associated to the best parameters
    for(param in tuneParameters){
      
      predictions <- predictions[predictions[,param] == modelFit$bestTune[1,param],]
      results <- results[results[,param] == modelFit$bestTune[1,param],]
    }
    
    # Save the residuals to generate ROC Curve. Esto para los biologos.

    #save the dataframe
    write.table(predictions, file= paste0(outputT, "CV-residuals.csv"), quote = F, sep="," , row.names = F,
                col.names=T)
    
    #Write the results in a file
    write.table(results, file = paste0(outputT, "CV-results.csv"), quote = F, sep = "\t", col.names = T, row.names = F)
    
    #Saving the model for posterior analysis
    if(alg=="J48"){
      #Cache rJava object classifier in order to save it with the object
      .jcache(modelFit$finalModel$classifier)
    }
    
    saveRDS(modelFit, paste(outputT, "modelfit-", alg, ".rds", sep = ""))
  }
}