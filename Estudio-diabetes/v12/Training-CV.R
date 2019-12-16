library(caret)
library(pROC)
library(verification)
library(rJava)
#library(doMC) it does not work with J48
#registerDoMC(cores = 2)

output <- "results/"

fileNames <- c(#"Alldiabetes-FindRisk", "Alldiabetes-Hba1c", 
               "Alldiabetes-Hba1c+FindRisk", 
               #"TwoYears-FindRisk", "TwoYears-Hba1c", 
               "TwoYears-Hba1c+FindRisk")

algorithmNames <-c(#"glm", "J48", 
                "rf")

tuneLenght <-c(rf=30, J48=5)

fitControl <- trainControl(method="repeatedcv", number = 10, repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE,
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula("Class ~ .")

# Seed for reproducibility
set.seed(123)

for(fileName in fileNames){
  
  dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ",", row.names = 1)
  
  for(alg in algorithmNames){
    
    outputT <- paste0(output, alg,"/", fileName, "/")
    
    if(!dir.exists(outputT)){
      dir.create(outputT, recursive = T)
    }
    
    if(alg=="glm"){
      
      modelFit <- train(form, data = dataset,
                        method=alg,
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, family="binomial")    
    }
    
    if(alg=="rf" || alg=="J48"){
      
      modelFit <- train(form, data = dataset,
                        method=alg,
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, 
                        tuneLength = tuneLenght[[alg]])
    }
    
    # Predict over the same training data. Cosas de biologos...
    truth <- as.numeric(dataset$Class=='S')
    predictor <- predict(modelFit, newdata = dataset, type = "prob")
    
    rocT <- roc(truth, predictor$S) # Draw ROC curve.
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT,'TrainingSet-ROC-Curve.png', sep = ""))
    dev.off()
    
    # Save the residuals to generate ROC Curve. Esto para los biologos.
    cvResiduals <- data.frame(class=truth, N= predictor$N, S= predictor$S)
    
    #save the dataframe
    write.table(cvResiduals, file= paste0(outputT, "TrainingSet-residuals.csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
    
    #Compute the metrics
    ROCV <- roc.area(truth, predictor$S)
    pvalue <- ROCV$p.value
    auc <- ROCV$A
    
    #To extract sensitivity and specifitivity
    predictorFactor <- predict(modelFit, newdata = dataset)
    sensitivity <- sensitivity(predictorFactor, dataset$Class)
    specificity <- specificity(predictorFactor, dataset$Class)
    
    dataResults <- data.frame("AUC"= auc, "p-value"= pvalue, "sensitivity"=sensitivity, "specificity"= specificity)
    
    #Write the results in a file
    write.table(dataResults, file = paste0(outputT, "TrainingSet-results.csv"), quote = F, sep = "\t", col.names = T, row.names = F)

    # This case is for generate the results of CV process.
    
    if(alg=="glm"){
      predictions <- modelFit$pred
    }
    
    if(alg=="rf"){
      # Extracting the best tune
      predictions <- modelFit$pred[modelFit$pred$mtry == modelFit$bestTune$mtry,]
    }
    
    if(alg=="J48"){
      # Extracting the best tune
      predictions <- modelFit$pred[modelFit$pred$C == modelFit$bestTune$C & modelFit$pred$M == modelFit$bestTune$M,]
    }
    
    rocT <- roc(predictions$obs, predictions$S)
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT, '/CV-ROC-Curve.png', sep = ""))
    dev.off()
    
    #save the dataframe
    write.table(predictions, file= paste0(outputT, "CV-residuals.csv"), quote = F, sep="," , row.names = F,
                col.names=T)
    
    labels <- rep(0, length(predictions$obs))
    labels[predictions$obs=="S"] <- 1
    
    #Compute the metrics
    ROCV <- roc.area(labels, predictions$S)
    pvalue <- ROCV$p.value
    auc <- ROCV$A
    
    #To extract sensitivity and specifitivity
    sensitivity <- modelFit$results[rownames(modelFit$bestTune)[1],]$Sens
    specificity <- modelFit$results[rownames(modelFit$bestTune)[1],]$Spec
    
    dataResults <- data.frame("AUC"= auc, "p-value"= pvalue, "sensitivity"=sensitivity, "specificity"= specificity)
    
    #Write the results in a file
    write.table(dataResults, file = paste0(outputT, "CV-results.csv"), quote = F, sep = "\t", col.names = T, row.names = F)
    
    #Saving the model for posterior analysis
    if(alg=="J48"){
      #Cache rJava object classifier in order to save it with the object
      .jcache(modelFit$finalModel$classifier)
    }
    saveRDS(modelFit, paste(outputT, "modelfit-", alg, ".rds", sep = ""))
  }
}