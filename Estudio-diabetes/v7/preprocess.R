library(caret)

setwd("D:/workspace/IMIBIC/datasets/diabetes/v7/")

fileT <- "4h.csv"

dataset <- read.csv(fileT, na.strings = "", sep = ",", row.names = 1)

methods <- c("zv", "nzv", "center","scale","YeoJohnson")

preProcValues <- preProcess(dataset, method = methods)

datasetTransformed <- predict(preProcValues, dataset)

#detecting inconsistent or dupplicated examples

datasetTransformed <- datasetTransformed[!duplicated(datasetTransformed[,1:(ncol(datasetTransformed)-1)]),]

write.table(datasetTransformed, file= paste(fileT, "-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
