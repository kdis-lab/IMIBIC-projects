library(caret)
library(pROC)
library(verification)

alg <- "rf"

path <- "/media/ogreyesp/DATA/workspace/IMIBIC/datasets/diabetes/v10/results/rf/Year1vsRest-0h-4h/"

modelName <- "modelfit-rf-61"

modelFit <- readRDS(paste0(path, modelName, ".rds"))

if(alg =="glm"){
  predictions <- modelFit$pred
}

if(alg =="rf"){
  # Extracting the best tune
  predictions <- modelFit$pred[modelFit$pred$mtry == modelFit$bestTune$mtry,]
}

results <- modelFit$results[rownames(modelFit$bestTune)[1],]

response <- as.numeric(modelFit$trainingData$.outcome=='S')
predictor <- aggregate(S ~ rowIndex, predictions, mean)[,'S']

rocV <- roc.area(response, predictor)
cat(rocV$p.value)
cat(rocV$A)

# The library pROC is better for ploting the ROC curve.
rocT <- roc(response, predictor)

plot(rocT, print.auc = TRUE)

# Save the CV residuals. Esto es para que posteriormente los biologos grafiquen sus cosas.
cvResiduals <- data.frame(class=response,prediction=predictor)

#save the dataframe
write.table(cvResiduals, file= paste0(path, alg, "-CV-residuals.csv"), quote = FALSE, sep="," , row.names = FALSE, 
            col.names = TRUE)