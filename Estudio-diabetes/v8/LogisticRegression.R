library(caret)
library(pROC)

fileName <- "Indices-0h-4h"

output <- "results/LogisticRegression/"

dataset <- read.csv(paste(fileName,".csv", sep = ""), sep = ";", row.names = 1)

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

# See discusion at this url https://stats.stackexchange.com/questions/195264/repeated-crossvalidation-finalmodel-and-roc-curves
# Here different forms to compute the ROC curve are exposed.

#ROC 1. roc.1 is irrelevant as it evaluates a model on the same data used to train it (the finalModel is 
#just the fit on Data ignoring the CV argument, built to apply on a different dataset for future prediction)

plsProbs <- predict(modelFit, newdata = dataset, type = "prob") # Predict over the same training data

roc.1 <- roc(dataset$DIABETES, plsProbs$S) # Draw ROC curve.
plot(roc.1, print.auc = TRUE)

dev.copy(png, paste(output, fileName,'-prediction-overTraining.png', sep = ""))
dev.off()

# ROC 2. roc.2 is 'almost' accurate as it will consider each prediction independently (averaging the 
# prediction, not the probabilities)

roc.2 <- roc(modelFit$pred$obs, modelFit$pred$S) # Draw ROC curve.

plot(roc.2, print.auc = TRUE)

dev.copy(png, paste(output, fileName,'-prediction-almost-CV.png', sep = ""))
dev.off()

# ROC 3. roc.3 is the correct way to do it as it averages the prediction probabilities for each sample 
# among the repeated CV (contrary to roc.2 where the prediction results are averaged)

roc.3 <- roc(as.numeric(modelFit$trainingData$.outcome=='S'),aggregate(S ~ rowIndex,modelFit$pred,mean)[,'S'])

plot(roc.3, print.auc = TRUE)

dev.copy(png, paste(output, fileName,'-prediction-correct-CV.png', sep = ""))
dev.off()