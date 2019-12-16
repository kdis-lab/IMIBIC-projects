library(caret)
library(pROC)
library(verification)

alg <- "rf"

path <- "/media/ogreyesp/DATA/workspace/IMIBIC/datasets/diabetes/v11/results/rf/AllDiabetes-0h/"

modelName <- "modelfit-rf-19"

modelFit <- readRDS(paste0(path, modelName, ".rds"))

if(alg =="glm"){
  predictions <- modelFit$pred
}

if(alg =="rf"){
  # Extracting the best tune
  predictions <- modelFit$pred[modelFit$pred$mtry == modelFit$bestTune$mtry,]
}

if(alg=="J48"){
  # Extracting the best tune
  predictions <- modelFit$pred[modelFit$pred$C == modelFit$bestTune$C & modelFit$pred$M == modelFit$bestTune$M,]
}

results <- modelFit$results[rownames(modelFit$bestTune)[1],]
sensitivity <- results$Sens
specificity <- results$Spec

response <- as.numeric(modelFit$trainingData$.outcome=='S')
predictor <- aggregate(S ~ rowIndex, predictions, mean)[,'S']

rocV <- roc.area(response, predictor)
pvalue <-rocV$p.value
auc <- rocV$A

# The library pROC is better for ploting the ROC curve.
rocT <- roc(response, predictor)

plot(rocT, print.auc = TRUE)

dev.copy(png, paste0(path, '/CV-subset-ROC-Curve.png'))
dev.off()

#Save the results
dataResults <- data.frame("AUC"= auc, "p-value"= pvalue, "sensitivity"=sensitivity, "specificity"= specificity)

# Write the results in a file
write.table(dataResults, file = paste0(path, "CV-subset-results.csv"), quote = F, sep = "\t", col.names = T, row.names = F)

# Save the CV residuals. Esto es para que posteriormente los biologos grafiquen sus cosas...
cvResiduals <- data.frame(class=response, prediction=predictor)

#save the dataframe
write.table(cvResiduals, file= paste0(path, "CV-subset-residuals.csv"), quote = FALSE, sep="," , row.names = FALSE, 
            col.names = TRUE)