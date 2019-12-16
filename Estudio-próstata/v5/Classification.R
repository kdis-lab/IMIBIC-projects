library(caret)
library(plotROC)

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/v5/")

outputFolder <- "results/"

#Not available in Windows
# library(doMC)
# registerDoMC(cores = 5)
## All subsequent models are then run in parallel

mydata <- read.csv("GrassovPTvsM.csv")

algorithm <- "rf"

metricOpt <- "ROC"
summaryFunction <- twoClassSummary

set.seed(123)

# we use an automatic grid by means of using the parameter tunelength
# see http://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/

fitControl <- trainControl(method="LOOCV", classProbs = TRUE,
                    savePredictions = TRUE, allowParallel= TRUE, search = "grid",
                    summaryFunction = summaryFunction, verboseIter = FALSE)

form <- as.formula(paste(colnames(mydata)[ncol(mydata)]," ~ ."))

#execute the algorithms

modelFit <- train(form, data = mydata,
                    method=algorithm,
                    metric = metricOpt,
                    maximize = TRUE,
                    tuneLength = 10,
                    trControl = fitControl)
  
#Saving all the results
write.table(modelFit$results, file= paste(outputFolder,"results-", algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")
  
#Saving the variable importance
varImportance <- varImp(modelFit, scale= TRUE)
  
write.table(varImportance$importance, file= paste(outputFolder,"varImportance-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
  
# To save the model to disk
saveRDS(modelFit, paste(outputFolder,"modelfit-",algorithm, ".rds", sep = ""))
  
# To save which predictors were used in the final model.
write.table(data.frame(Predictor = predictors(modelFit)), file= paste(outputFolder,"predictors-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = FALSE, col.names = FALSE, na = "")

#Only plot the graph for those model that contain tunning process
if(nrow(modelFit$results)>1){
    
    ggplot(modelFit)
    
    ggsave(width = 8, height = 5, filename = paste(outputFolder, algorithm,"-",metricOpt,"-tuning",".png",sep = ""), bg = "transparent")
}

#######################To generate the ROC curve of the best model.

bestIndex <- rownames(modelFit$bestTune)[1]
selectedIndices <- modelFit$pred$mtry == modelFit$bestTune[1, "mtry"]
results <- modelFit$results[bestIndex,"ROC"]

g <- ggplot(modelFit$pred[selectedIndices,], aes(m = PT, d=factor(obs, levels = c("PT","M")))) + 
     geom_roc(n.cuts=0) + coord_equal() + style_roc()

g + annotate("text", x=0.85, y=0.15, label=paste("AUC =", results))

ggsave(width = 8, height = 5, filename = paste(outputFolder, algorithm,"-","ROC-Curve",".png",sep = ""), bg = "transparent")
