# La importancia del centrado se puede consultar en
# http://www.theanalysisfactor.com/center-on-the-mean/
# Robert et al. Centering, scaling, and transformations: improving 
#the biological information content of metabolomics data

library(caret)

#We use the center method (subtracts the mean of the predictor's data). We focused in the differences

#We use the scale method (divides by the standard deviation)

#We use the zv method (identifies numeric predictor columns with a single value 
#(i.e. having zero variance) and excludes them from further calculations.)

#We use the nzv option. It excludes "near zero-variance" predictors.

#We use the Yeo-Johnson transformation (it is similar to the Box-Cox model but can accommodate 
#predictors with zero and/or negative values (while the predictors values for the Box-Cox 
#transformation must be strictly positive.)

#We used bag impute 

#k-nearest neighbor imputation is carried out by finding the k closest samples 
#(Euclidian distance) in the training set. Imputation via bagging fits a bagged tree model 
#for each predictor (as a function of all the others). This method is simple, accurate and 
#accepts missing values, but it has much higher computational cost. On the other hand, 
#imputation via medians takes the median of each predictor in the training set, 
#and uses them to fill missing values. This method is simple, fast, and accepts missing values, 
#but treats each predictor independently, and may be inaccurate.
# These are the possible methods for inputation that are accepted in preprocess method
#"knnImpute", "bagImpute", "medianImpute"

#We are not sure if it is convenient to use method = "corr" (it seeks to filter out highly correlated predictors.) 

#The operations are applied in this order: zero-variance filter, near-zero variance filter, 
#correlation filter, Box-Cox/Yeo-Johnson/exponential transformation, centering, scaling, range, imputation, PCA, ICA then spatial sign

# load the database
setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/diabetes/v2/")

fileT <-"FC"

database <-read.csv(paste(fileT,".csv", sep = ""))

methods <- c("zv","nzv","YeoJohnson","center","scale","bagImpute")

preProcValues <- preProcess(database, method = methods, outcome = database$Class)

databaseTransformed <- predict(preProcValues, database)

#detecting inconsistent or dupplicated examples

databaseTransformed <- databaseTransformed[!duplicated(databaseTransformed[,1:(ncol(databaseTransformed)-1)]),]

write.table(databaseTransformed, file= paste(fileT,"-preproc.csv",sep = ""),
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")