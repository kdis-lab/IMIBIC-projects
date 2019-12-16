library(caret)
library(pROC)
library(verification)

output <- "results/"

fileName <- "parafinas-withoutMissingValues"

form <- as.formula("Class ~ .")

# Seed for reproducibility
set.seed(123)

dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ",")

dataset <- dataset[,c("TIA1", "SRSF10", "U2AF2", "SRSF9", "SRRM4", "SND1", "ESRP1", "SRSF3", "KHDRBS1", "SNRNP200", "SRRM1", "Class")]
    
outputT <- paste0(output,"rf/", fileName, "/")
    
if(!dir.exists(outputT)){
      dir.create(outputT, recursive = T)
}

# To perfom leave one group out
ns <- nrow(dataset)/2

subjects <- c(c(1:ns), c(1:ns))

folds <- groupKFold(subjects, k = ns)

fitControl <- trainControl(method="LGOCV", classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE, 
                           summaryFunction = twoClassSummary, verboseIter = FALSE, index = folds)

modelFit <- train(form, data = dataset,
                        method="rf",
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, tuneLength = 30, 
                        preProcess=c("zv", "nzv", "center","scale","YeoJohnson")
                  )
    
# Predict over the same training data. Cosas de biologos...
truth <- as.numeric(dataset$Class=='T')
predictor <- predict(modelFit, newdata = dataset, type = "prob")
    
rocT <- roc(truth, predictor$T, ci= T) # Draw ROC curve.
plot(rocT, print.auc = TRUE)
    
dev.copy(png, paste(outputT,'TrainingSet-ROC-Curve.png', sep = ""))
dev.off()
    
# Save the residuals to generate ROC Curve. Esto para los biologos.
cvResiduals <- data.frame(class=truth, N= predictor$N, T= predictor$T)
    
#save the dataframe
write.table(cvResiduals, file= paste0(outputT, "TrainingSet-residuals.csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
    
#Compute the metrics
ROCV <- roc.area(truth, predictor$T)
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
    
# Extracting the best tune
predictions <- modelFit$pred[modelFit$pred$mtry == modelFit$bestTune$mtry,]

rocT <- roc(predictor = predictions$T, response = predictions$obs, ci= T, levels=rev(levels(dataset$Class)))
plot(rocT, print.auc = TRUE)
    
dev.copy(png, paste(outputT, '/CV-ROC-Curve.png', sep = ""))
dev.off()
    
#save the dataframe
write.table(predictions, file= paste0(outputT, "CV-residuals.csv"), quote = F, sep="," , row.names = F,
                col.names=T)
    
labels <- rep(0, length(predictions$obs))
labels[predictions$obs=="T"] <- 1
    
#Compute the metrics
ROCV <- roc.area(labels, predictions$T)
pvalue <- ROCV$p.value
auc <- ROCV$A
    
#To extract sensitivity and specifitivity
sensitivity <- modelFit$results[rownames(modelFit$bestTune)[1],]$Sens
specificity <- modelFit$results[rownames(modelFit$bestTune)[1],]$Spec
    
dataResults <- data.frame("AUC"= auc, "p-value"= pvalue, "sensitivity"=sensitivity, "specificity"= specificity)
    
#Write the results in a file
write.table(dataResults, file = paste0(outputT, "CV-results.csv"), quote = F, sep = "\t", col.names = T, row.names = F)
    
saveRDS(modelFit, paste(outputT, "modelfit-rf.rds", sep = ""))


library(lime)

explainer <- lime(dataset[, -12], modelFit, bin_continuous = TRUE, quantile_bins = FALSE)

explanation <- explain(dataset[c(2, 84), -12], explainer, n_labels = 1, n_features = 11)

# Only showing part of output for better printing
explanation

plot_features(explanation, ncol = 2)
plot_explanations(explanation)

explanation <- explain(dataset[, -12], explainer, n_labels = 1, n_features = 5)
plot_explanations(explanation)


