library(caret)
library(ggplot2)
library(plotROC)
library(pROC)

path <- "D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/"

setwd(path)

mydata <- read.csv(paste(path,"ControlesVSEnfermosLinfocitos-preproc.csv",sep = ""), sep = ",")

rfFuncs$summary <- twoClassSummary

ctrl <- rfeControl(functions = rfFuncs,
                   method = "LOOCV",
                   verbose = FALSE, saveDetails = TRUE)
subsets <- c(1:45)

rfProfile <- rfe(x= mydata[,-ncol(mydata)], y= mydata[,ncol(mydata)], sizes = subsets,
                 rfeControl =  ctrl, metric = "ROC")

rfProfile
plot(rfProfile)

# estimate variable importance
importance <- varImp(rfProfile, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)