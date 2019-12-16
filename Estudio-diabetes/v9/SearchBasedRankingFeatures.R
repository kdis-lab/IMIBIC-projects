library(caret)
library(rJava)

#library(doMC)
#registerDoMC(cores = 8)

threshold <- 0.75

output <- "results/"

datasets <- c("0h", "4h", "0h-4h", "Indices", "Indices-0h", "Indices-4h", "Indices-0h-4h")

algorithms <- c("rf", "glm")

#These two can not be performed with the library doMC "J48", "PART"

metricOpt <- "ROC"

for(fileName in datasets){
  
  #load the dataset and ranking of features
  dataset <-  read.csv(paste(fileName,".csv",sep = ""), sep = ";", row.names = 1)  
  
  rankingFeature <- read.csv(paste(output,fileName,"-sortedfeatures.csv", sep = ""), sep = "\t")
  
  nFeatures <- nrow(rankingFeature)
  
  classIndex <- ncol(dataset)
  
  numberClasses <- length(levels(dataset[,ncol(dataset)]))
  
  set.seed(123)
  
  form <- as.formula(paste(colnames(dataset)[ncol(dataset)]," ~ ."))
  
  fitControl <- trainControl(method="repeatedcv", number = 10,
                             repeats = 3, classProbs = TRUE,
                             savePredictions = TRUE, allowParallel= TRUE, 
                             summaryFunction = twoClassSummary, verboseIter = FALSE)
  
  #execute the algorithms
  for(algorithm in algorithms){
    
    outputTemp <- paste(output, fileName, "/", algorithm,"/", sep = "")
    
    if(!dir.exists(outputTemp)){
      
      dir.create(outputTemp, recursive = TRUE)
    }
    
    #The dataframe where we stored the results for this algorithm
    df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), metricOpt=c(0))
    
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
                            tuneLength = 30,
                            trControl = fitControl)
        }
        
        bestTuneIndex <- as.numeric(rownames(modelFit$bestTune)[1])
        
        auc <- modelFit$results[bestTuneIndex, metricOpt]
        
        #register the model
        if(auc >= threshold){
          
          vars <- predictors(modelFit)
          
          df <- rbind(df, data.frame(ID = nrow(df) + 1, NumberAtts= length(vars), Atts= paste(vars, collapse = ' '), metricOpt=auc))
          
          #Saving the model for graphing plots a posteriory if it is necessary
          #Cache rJava object classifier in order to save it with the object
          .jcache(modelFit$finalModel$classifier)
          saveRDS(modelFit, paste(outputTemp, "modelfit-",algorithm,"-", nrow(df),".rds", sep = ""))
          
          if(algorithm == "J48"){
            pdf(paste(outputTemp, "tree-", algorithm,"-", nrow(df),".pdf", sep = ""), width = 12, height = 8)
            plot(modelFit$finalModel)
            dev.off()
          }
          else if(algorithm == "PART"){
            write(modelFit$finalModel, file = paste(outputTemp,"rules-",algorithm,"-", nrow(df),".txt", sep = ""))
          }
          else if(algorithm == "rf"){
            write(paste("rf-", nrow(df),";", modelFit$finalModel$ntree, sep = ""), file = paste(outputTemp, "rf.txt", sep = ""), append = TRUE)
          }
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
    df <- df[ order(-df[,ncol(df)]), ]
    
    #save the dataframe
    write.table(df, file= paste(outputTemp, algorithm,".csv", sep = ""), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
  }
}
