library(caret)
library(FactoMineR)
library(Factoshiny)
library(FactoInvestigate)

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

source("HeatMap.R")

mydata <- read.csv("Ictus-preproc-withoutOutliers.csv", sep = ",", na.strings = "")

predictors <- read.csv("results/Classification-builtinFS/predictors-sparseLDA.csv", header = FALSE)

idx <-match(c(as.character(predictors[,1]), "Class"),colnames(mydata))

mydata <- mydata[, match(c(as.character(predictors[,1]), "Class"), colnames(mydata))]

set.seed(123)

fitControl <- trainControl(method="LOOCV", classProbs = TRUE,
                           savePredictions = TRUE, search="grid", allowParallel= TRUE, 
                           summaryFunction = twoClassSummary, verboseIter = FALSE)

form <- as.formula("Class ~ .")

modelFit <- train(form, data = mydata,
                  method="sparseLDA",
                  metric = "Spec",
                  maximize = TRUE,
                  tuneLength = 50,
                  trControl = fitControl)

idx <- match(c(predictors(modelFit),"Class"), colnames(mydata))

clusterMyData <- mydata[,idx]

PlotHeatMap(data = clusterMyData, imgName = "results/Clustering/sparseLDA")

#PCA analysis############
#PCA with supplementary variables
res <- PCA(clusterMyData,
  scale.unit = FALSE,
  quali.sup = ncol(clusterMyData),
  graph = FALSE)

Investigate(res, document = "word_document", file = "pca.Rmd")

# Hierarchical clusterer
HCPCshiny(res)