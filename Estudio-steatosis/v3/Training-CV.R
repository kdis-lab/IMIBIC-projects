library(caret)
library(pROC)
library(verification)
library(rJava)
library(doMC)
registerDoMC(cores = 2)

output <- "results/Curve3/"

fileNames <- c("sanos-enfermos")

algorithmNames <-c(#"glm",
                    "rf"
                  #,"J48"
                   )

tuneLenght <-c(rf=30, J48=5)

fitControl <- trainControl(method="LOOCV", classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE,
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula("Class ~ .")

#columns <- c("RBM45", "U4", "PTB", "U6ATAC", "SRM160", "Class")
#columns <- c("PTB", "RBM22", "SRSF1", "SRM160", "U1", "U6", "U6ATAC", "Class")
columns <- c("CUGBP", "PTB", "RBM22", "RBM3", "SRM160", "U6", "U6ATAC", "Class")

# Seed for reproducibility
set.seed(123)

for(fileName in fileNames){
  
  dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ",")
  
  dataset <- dataset[, columns]
  
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
    truth <- as.numeric(dataset$Class=='E')
    predictor <- predict(modelFit, newdata = dataset, type = "prob")
    
    rocT <- roc(truth, predictor$E) # Draw ROC curve.
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT,'TrainingSet-ROC-Curve.png', sep = ""))
    dev.off()
    
    # Save the residuals to generate ROC Curve. Esto para los biologos.
    cvResiduals <- data.frame(class=truth, prediction=predictor$E)
    
    #save the dataframe
    write.table(cvResiduals, file= paste0(outputT, "TrainingSet-residuals.csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
    
    #Compute the metrics
    ROCV <- roc.area(truth, predictor$E)
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
    
    truth <- as.numeric(modelFit$trainingData$.outcome=='E')
    predictor <- aggregate(E ~ rowIndex, predictions, mean)[,'E']
    
    rocT <- roc(truth, predictor)
    plot(rocT, print.auc = TRUE)
    
    dev.copy(png, paste(outputT, '/CV-ROC-Curve.png', sep = ""))
    dev.off()
    
    # Save the residuals to generate ROC Curve. Esto para los biologos.
    cvResiduals <- data.frame(class=truth, prediction=predictor)
    
    #save the dataframe
    write.table(cvResiduals, file= paste0(outputT, "CV-residuals.csv"), quote = F, sep="," , row.names = F,
                col.names=T)
    
    #Compute the metrics
    ROCV <- roc.area(truth, predictor)
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