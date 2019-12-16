library(caret)
library(ggplot2)
library(plotROC)
library(pROC)
library(ROCR)

setwd("/media/ogreyesp/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/diabetes/v5/")

dataset <- read.csv("Anno2-preproc.csv")

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

plsProbs <- predict(modelFit, newdata = dataset)

confusionMatrix(plsProbs, dataset$DIABETES)