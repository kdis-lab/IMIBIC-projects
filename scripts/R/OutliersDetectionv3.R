library(outliers)
library(DMwR)

mydata <- read.csv("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/neuroencodrino-pulmonar/NormalesP1vsNormalesP2-raw-norm.csv", sep = ",")

subdata <- mydata[,-ncol(mydata)]

#Calculate the number of NA values in each column before computing outliers
colSums(is.na(subdata))

#Detecting all values beyond 98th %ile based on z-scores
logicalOutliers <- scores(subdata, type = "iqr")

#Count the number of outliers by columns
colSums(logicalOutliers)

# delete those values equal to TRUE (outliers)
subdata[as.matrix(logicalOutliers)] <- NA

#Number of missing values after removing outliers
colSums(is.na(subdata))

#copying the column class
mydataWithoutOutliers<- cbind(subdata,Class = mydata$Class)

write.table(mydataWithoutOutliers, file = "D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/neuroencodrino-pulmonar/NormalesP2vsTumoralesP1-withoutOutilers.csv",
            quote = FALSE,
            sep = "," ,
            row.names = FALSE, col.names = TRUE, na = "")