dataset <- read.csv("results/CSC-sortedfeatures.csv", na.strings = "", sep = ",")

dataset$Importance <- abs(dataset$Importance)

dataset <- dataset[order(-dataset$Importance),]

write.csv( dataset, file="results/CSC-sortedfeaturesv2.csv", row.names = FALSE, col.names = TRUE, sep = ",")
