library(caret)
library(plotROC)

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

outputFolder <- "results/Classification-builtinFS/"

#Not available in Windows OS
## All subsequent models are then run in parallel
library(doMC)
registerDoMC(cores = 2)

algorithms <- c(
  #"glmnet",
  "wsrf",
  "rf",
  "sparseLDA")

mydata <- read.csv("Ictus-preproc-withoutOutliers.csv")

set.seed(123)

#we use an automatic grid by means of using the parameter tunelength
#see http://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/

fitControl <- trainControl(method="LOOCV", classProbs = TRUE,
                           savePredictions = TRUE, search="grid", allowParallel= TRUE, 
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula(paste(colnames(mydata)[ncol(mydata)]," ~ ."))

#execute the algorithms
for(algorithm in algorithms){
  
  modelFit <- train(form, data = mydata,
                    method=algorithm,
                    metric = "ROC",
                    maximize = TRUE,
                    tuneLength = 50,
                    trControl = fitControl)
  
  #Saving all the results
  write.table(modelFit$results, file= paste(outputFolder,"results-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")
  
  #Saving the variable importance
  varImportance <- varImp(modelFit, scale= TRUE)
  
  write.table(varImportance$importance, file= paste(outputFolder,"varImportance-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
  
  #Saving the model for graphing plots a posteriory.
  # save the model to disk
  saveRDS(modelFit, paste(outputFolder,"modelfit-",algorithm, ".rds", sep = ""))
  
  # To save which predictors were used in the final model.
  write.table(data.frame(Predictor = predictors(modelFit)), file= paste(outputFolder,"predictors-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = FALSE, col.names = FALSE, na = "")
  
  
  #Only plot the graph for those model that contain tunning process
  if(nrow(modelFit$results)>1){
    
    ggplot(modelFit)
    
    ggsave(width = 8, height = 5, filename = paste(outputFolder, algorithm,"-ROC-tuning",".png",sep = ""), bg = "transparent")
  }
  
  #######################To generate the ROC curve of the best model.
  
  tuneParameters <- colnames(modelFit$bestTune)
  bestIndex <- rownames(modelFit$bestTune)[1]

  results <- modelFit$results[bestIndex,"ROC"]
  
  predictions <- modelFit$pred
  
  # to filter the best predictions and results associated to the best parameters
  for(param in tuneParameters){
    
    predictions <- predictions[predictions[,param] == modelFit$bestTune[1,param],]
  }
  
  g <- ggplot(predictions, aes(m = S, d=factor(obs, levels = c("S","N")))) + 
    geom_roc(n.cuts=0) + coord_equal() + style_roc()
  
  g + annotate("text", x=0.85, y=0.15, label=paste("AUC =", results))
  
  ggsave(width = 8, height = 5, filename = paste(outputFolder, algorithm,"-","ROC-Curve",".png",sep = ""), bg = "transparent")
}