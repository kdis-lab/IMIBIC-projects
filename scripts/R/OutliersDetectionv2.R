library(outliers)
library(DMwR)

mydata <- read.csv("/home/oscar/workspace/IMIBIC/datasets/diabetes/basal0h-raw.csv", sep = ",")

subdata <- mydata[,-1]

#Calculate the number of NA values in each column before computing outliers

countMissingByColumns <- colSums(is.na(subdata))

print(countMissingByColumns)

# Detect variables that have more than 75% of missing values

columnsWithAlmostAllMissingValues <- countMissingByColumns >= 0.75*nrow(subdata)

if(sum(columnsWithAlmostAllMissingValues)>0){
  
  print("Columns with many missing values")
  print(namesCol[columnsWithAlmostAllMissingValues])
  
  #remove these columns
  subdata <- subdata[,!columnsWithAlmostAllMissingValues]
  
}

# We try to replace all missing data by kNN imputation, k=5
subdata <- knnImputation(subdata, k = 5)

#Detecting all values beyond 95th %ile based on z-scores
logicalOutliers <- scores(subdata, type = "z", prob = 0.95)

#Count the number of outliers by columns
colSums(logicalOutliers)

# delete those values equal to TRUE (outliers)
subdata[as.matrix(logicalOutliers)] <- NA

# We try to replace all removed outliers by kNN imputation, k=5
subdata <- knnImputation(subdata, k = 5)

#copying the column id
mydataWithoutNorm <- cbind(id = mydata$id,subdata)

write.table(mydataWithoutNorm, file = "/home/oscar/workspace/IMIBIC/datasets/diabetes/basal0h-withoutNorm.csv",
            quote = FALSE,
            sep = "," ,
            row.names = FALSE, col.names = TRUE)

subdataZNormalized <- scores(subdata, type = "z")

#copying the column id
mydataNorm <- cbind(id = mydata$id,subdataZNormalized)

write.table(mydataNorm, file = "/home/oscar/workspace/IMIBIC/datasets/diabetes/basal0h-Norm.csv",
            quote = FALSE,
            sep = "," ,
            row.names = FALSE, col.names = TRUE)