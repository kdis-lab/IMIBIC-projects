library(caret)

# load the database
setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

dataset <- read.csv("Ictus.csv")

methods <- c("zv","nzv","YeoJohnson","center","scale")

preProcValues <- preProcess(dataset, method = methods, outcome = dataset$Class)

datasetTransformed <- predict(preProcValues, dataset)

#detecting inconsistent or dupplicated examples

datasetTransformed <- datasetTransformed[!duplicated(datasetTransformed[, 1:(ncol(datasetTransformed)-1)]),]

write.table(datasetTransformed, file= "Ictus-preproc.csv",
            quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
