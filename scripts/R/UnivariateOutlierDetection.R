# For a given continuous variable, outliers are those observations that lie 
# outside 1.5 *IQR, where IQR, the 'Inter Quartile Range' is the difference between
# 75th and 25th quartiles. The outliers are removed with the median value of the group that they belong.

#function that takes a vector of data and a coefficient,
#returns boolean vector if a certain point is an outlier or not
remove_outliers <- function(varT, mydata, coef=1.5){
  
  vAll <- mydata[,varT]
  
  classVar <- mydata[,ncol(mydata)]
  
  classNames <- levels(mydata[,ncol(mydata)])
  
  #for each class
  for(ClassType in classNames){
    
    vClass <- mydata[classVar == ClassType, varT]
    
    quantilesG <- quantile(vClass,probs=c(0.25,0.75), na.rm = TRUE)
    
    IQRG <- quantilesG[2]-quantilesG[1]
    
    indexes <- (vAll < (quantilesG[1]-coef*IQRG) | vAll > (quantilesG[2]+coef*IQRG)) & classVar == ClassType
    
    indexes[is.na(indexes)] <- FALSE
    
    print(paste("Ouliers of var ", varT, " in class ", ClassType,": ", sum(indexes), sep = ""))
    
    mydata[indexes, varT] <- median(vClass, na.rm =TRUE)
  }
  
  return (mydata)
}

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/")

fileT <-"TCGA-2factores"

mydata <- read.csv(paste(fileT,".csv", sep = ""), sep = ",", na.strings = "")

colNamesT <- colnames(mydata)[1:(ncol(mydata)-1)]

# Remove the outliers in each numeric predictor
for(nameC in colNamesT){
  
  if(class(mydata[,nameC])=="integer" || class(mydata[,nameC])=="numeric")
  {
    mydata <- remove_outliers(nameC,mydata)
  }
}

sum(is.na(mydata))

write.table(mydata, file = paste(fileT,"-withoutOutliers.csv",sep = ""),
            quote = FALSE, sep = "," , row.names = FALSE, col.names = TRUE, na = "")
