library(caret)

path <- "D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/"

fileName <- "PreVSPostNeutrofilos"

setwd(path)

mydata <- read.csv(paste(path, fileName, ".csv",sep = ""),sep = ";", na.strings = "")

#Number of missing values
numberMissingValues<-colSums(is.na(mydata))

#Percet of missing values
percents<-colSums(is.na(mydata))*100/nrow(mydata)

# Which atrr have a percent of missing values greater than 50%
sparseAtts <- which(percents > 50)

#method = "zv" identifies numeric predictor columns with a single value
#(i.e. having zero variance) and excludes them from further calculations.
#method = "nzv" does the same by applying nearZeroVar exclude "near zero-variance" 
#predictors.
#method="bagImpute"  Imputation via bagging fits a bagged tree model for each predictor (as a function of all the others). 
#This method is simple, accurate and accepts missing values, but it has much higher computational cost.

methods <- c("zv","nzv", "YeoJohnson","center","scale", "bagImpute")

colClassName <- colnames(mydata)[ncol(mydata)]

preProcValues <- preProcess(mydata, method = methods, outcome = mydata[, colClassName])

databaseTransformed <- predict(preProcValues, mydata)

# To verify the columns that were removed
colnames(mydata)[which(!(colnames(mydata) %in% colnames(databaseTransformed)))]

write.table(databaseTransformed, file= paste(path, fileName,"-preproc.csv",sep = ""),
            quote = FALSE, sep=";" , row.names = FALSE, col.names = TRUE, na = "")