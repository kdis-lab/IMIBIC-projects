library(caret)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/")

#Not available in Windows
# library(doMC)
# registerDoMC(cores = 5)
## All subsequent models are then run in parallel

#Required packages

mydata <- read.csv("ControlesVSEnfermosLinfocitos-preproc.csv")

wants <- c("caret", "adabag", "bartMachine", "bst", "C50", "deepboost","earth","evtree", "extraTrees",
           "gbm", "glmnet", "inTrees", "mda", "nodeHarvest", "pamr", "penalizedLDA", "party", "plyr", "mboost", 
           "Matrix", "MASS", "ordinalNet", "obliqueRF", "rFerns", "randomForest", "RWeka", 
           "rotationForest","rpart", "RRF", "sparseLDA", "wsrf", "xgboost", "ROCR", "Metrics")

has   <- wants %in% rownames(installed.packages())

if(any(!has)) install.packages(wants[!has])

algorithms <- c("rf","AdaBoost.M1", "AdaBag", "bagEarth", "bagFDA", "bartMachine", "BstLm", "bstSm", "C5.0",
               "cforest", "ctree", "deepboost","extraTrees", "gamboost", "gbm", "glmnet",
                "J48", "JRip", "PART", "LMT", "nodeHarvest", "ordinalNet", "ORFlog", "ORFpls", "ORFridge",
                "ORFsvm", "pam", "PenalizedLDA", "wsrf", "rFerns", "rpart", "rpartCost", "RRF", "sparseLDA",
                "xgbLinear", "xgbTree")

set.seed(825)

# To create a stratified repeated k-fold cross validation
#multiIndexes<-createMultiFolds(y=mydata$readmitted, k = 10, times = 3)

#we use an automatic grid by means of using the parameter tunelength
# see http://machinelearningmastery.com/tuning-machine-learning-models-using-the-caret-r-package/

fitControl <- trainControl(method="LOOCV", classProbs=TRUE,
                    savePredictions = TRUE, search="grid", allowParallel= TRUE, 
                    summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula(paste(colnames(mydata)[ncol(mydata)]," ~ ."))

#execute the algorithms
for(algorithm in algorithms){

  modelFit <- train(form, data = mydata, 
                    method=algorithm,
                    metric = "ROC",
                    maximize = TRUE,
                    tuneLength = 20,
                    trControl = fitControl)
  
  #Saving all the results
  write.table(modelFit$results, file= paste("results/results-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")
  
  #Saving the variable importance
  varImportance <- varImp(modelFit, scale=FALSE)
  
  write.table(varImportance$importance, file= paste("results/varImportance-",algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
  
  #Saving the model for graphing plots a posteriory.
  # save the model to disk
  saveRDS(modelFit, paste("results/modelfit-",algorithm, ".rds", sep = ""))
}