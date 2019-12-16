library(caret)

fileT <- "datos"

dataset <- read.csv(paste0(fileT, ".csv"), na.strings = "")

methods <- c("zv", "nzv", "center","scale", "YeoJohnson")

preProcValues <- preProcess(dataset, method = methods)

datasetTransformed <- predict(preProcValues, dataset)

#detecting inconsistent or dupplicated examples
datasetTransformed <- datasetTransformed[!duplicated(datasetTransformed[,1:(ncol(datasetTransformed)-1)]),]

write.table(datasetTransformed, file= paste(fileT, "-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = F, col.names = TRUE, na = "")
