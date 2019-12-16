library(caret)

setwd("/media/ogreyesp/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/diabetes/v5/")

fileT <- "Anno1.csv"

dataset <- read.csv(fileT, na.strings = "")

methods <- c("zv", "nzv", "center","scale","YeoJohnson", "bagImpute")

preProcValues <- preProcess(dataset, method = methods)

datasetTransformed <- predict(preProcValues, dataset)

#detecting inconsistent or dupplicated examples

datasetTransformed <- datasetTransformed[!duplicated(datasetTransformed[,1:(ncol(datasetTransformed)-1)]),]

write.table(datasetTransformed, file= paste(fileT, "-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")