library(caret)
library(doMC)

registerDoMC(cores = 8)

threshold <- 0.8

output <- "results/"

datasets <- c("datos-preproc")

algorithms <- c("rf", "glm", "C5.0")

tuneLenght <-c(rf=30, C5.0 = 5)

metricOpt <- "ROC"

for(fileName in datasets){
  
  #load the dataset and ranking of features
  dataset <-  read.csv(paste(fileName,".csv",sep = ""), sep = ",")  
  
  rankingFeature <- read.csv(paste(output,fileName,"-sortedfeatures.csv", sep = ""), sep = "\t")
  
  nFeatures <- nrow(rankingFeature)
  
  classIndex <- ncol(dataset)
  
  numberClasses <- length(levels(dataset[,ncol(dataset)]))
  
  set.seed(123)
  
  form <- as.formula("Class ~ .")
  
  fitControl <- trainControl(method="LOOCV", classProbs = TRUE,
                             savePredictions = TRUE, allowParallel= TRUE, 
                             summaryFunction = twoClassSummary, verboseIter = FALSE)
  
  #execute the algorithms
  for(algorithm in algorithms){
    
    outputTemp <- paste0(output, algorithm, "/", fileName,"/")
    
    if(!dir.exists(outputTemp)){
      
      dir.create(outputTemp, recursive = TRUE)
    }
    
    #The dataframe where we stored the results for this algorithm
    df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), ROC= c(0), Sens= c(0), Spec = c(0))
    
    df <- df[-1,]
    
    #Execute the search by ranking of features
    #######################
    
    #All features will be considered as pivot one time
    for(feature in 1:nFeatures){
      
      pivotFeature <- rankingFeature[feature,"Index"]
      
      listFeatures <- c(pivotFeature)
      
      aucGlobal <- -1
      
      for(otherFeature in feature:nFeatures)
      {
        
        # The last case
        if(otherFeature != feature)
        {
          listFeatures <- c(listFeatures, rankingFeature[otherFeature,"Index"])
        }
        
        #extract the subset
        subdataset <- dataset[, c(listFeatures, classIndex)]
        
        #pass this subdataset to the classification algorithm
        
        if(algorithm == "glm"){
          modelFit <- train(form, data = subdataset,
                            method=algorithm,
                            metric = metricOpt,
                            maximize = TRUE,
                            tuneLength = 30,
                            trControl = fitControl, family="binomial")
        }
        else{
          
          modelFit <- train(form, data = subdataset,
                            method=algorithm,
                            metric = metricOpt,
                            maximize = TRUE,
                            tuneLength = tuneLenght[[algorithm]],
                            trControl = fitControl)
        }
        
        bestTuneIndex <- as.numeric(rownames(modelFit$bestTune)[1])
        
        auc <- modelFit$results[bestTuneIndex, metricOpt]
        sens <- modelFit$results[bestTuneIndex, "Sens"]
        spec <- modelFit$results[bestTuneIndex, "Spec"]
        
        #register the model
        if(auc >= threshold){
          
          vars <- predictors(modelFit)
          
          df <- rbind(df, data.frame(ID = nrow(df) + 1, NumberAtts= length(vars), Atts= paste(vars, collapse = ' '), ROC=auc,  Sens= sens, Spec = spec))
          
          saveRDS(modelFit, paste(outputTemp, "modelfit-",algorithm,"-", nrow(df),".rds", sep = ""))
        }
        
        if(aucGlobal < auc){
          
          aucGlobal <- auc
        }
        else{
          #remove the last feature added. due to did not apport any advantage
          listFeatures <- listFeatures[1:(length(listFeatures)-1)]
        }
      }
    }
    
    #######################End of search by ranking
    
    #order the rows of the dataframe
    df <- df[ order(-df[,ncol(df)-2]), ]
    
    #save the dataframe
    write.table(df, file= paste0(outputTemp, algorithm,".csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
  }
}