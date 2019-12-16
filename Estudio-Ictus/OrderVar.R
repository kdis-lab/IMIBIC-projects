setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

outputFolder <- "results/Classification-builtinFS/"

algorithm <- "wsrf"

columName <- "Overall"

vars <- read.csv(paste(outputFolder, "varImportance-", algorithm, ".csv", sep=""))

vars <- vars[vars[,1] !=0 , ,drop=FALSE]

vars<-vars[order(-vars[,1]),, drop=FALSE]

write.table(vars, file= paste(outputFolder,"varImportance-order-", algorithm, ".csv", sep = ""),
              quote = FALSE, sep="," , row.names = TRUE, col.names = TRUE, na = "")
