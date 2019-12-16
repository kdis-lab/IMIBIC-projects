library(caret)
library(pROC)

fileName <- "4h-preproc"

output <- "results/preprocessing/"

dataset <- read.csv(paste(fileName,".csv", sep = ""))

set.seed(123)

fitControl <- trainControl(method="repeatedcv", number = 10, repeats = 3, classProbs = TRUE,
                           savePredictions = TRUE, allowParallel= TRUE,
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula("DIABETES ~ .")

#pass this dataset to the classification algorithm
modelFit <- train(form, data = dataset,
                        method="glm",
                        metric = "ROC",
                        maximize = TRUE,
                        trControl = fitControl, family="binomial")

plsProbs <- predict(modelFit, newdata = dataset, type = "prob") # Predict over the same training data

#https://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r

result.roc <- roc(dataset$DIABETES, plsProbs$S) # Draw ROC curve.
plot(result.roc, print.auc = TRUE)

dev.copy(png, paste(output, fileName,'-prediction-overTraining.png', sep = ""))
dev.off()

#confusionMatrix(plsProbs, dataset$DIABETES)
