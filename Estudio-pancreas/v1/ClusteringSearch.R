#Libreria para calcular la pureza y la entropia
library(IntNMF)
library(pheatmap)
library(Cairo)


PlotHeatMap<-function(data, imgName="HeatMap", format="png", smplDist= "euclidean", 
                      clstDist= "ward.D", viewOpt="detail", rowV=TRUE, colV=TRUE, border=TRUE){
  
  classes <- data[ncol(data)]
  data <- data[-ncol(data)]
  
  colnames(data)<-substr(colnames(data),1,18) # some names are too long
  
  #rownames(data) <- 1:nrow(data)
  
  rownames(classes) <- rownames(data)
  
  #Set the name of image
  imgName <- paste(imgName, ".", format, sep="")
  
  minW <- 630;
  myW <- nrow(data)*18 + 150;
  
  if(myW < minW){
    myW <- minW;
  }   
  
  w <- round(myW/72,2);
  
  myH <- ncol(data)*18 + 150;
  h <- round(myH/72,2);
  
  if(border){
    border.col<-"grey60";
  }else{
    border.col <- NA;
  }
  
  Cairo(file = imgName, unit="in", dpi=72, width=w, height=h, type=format, bg="white");
  
  # Specify colors
  ann_colors = list(Tumor=c(N = "red", T = "green"))
  ann_colors = list(Tumor=c(N = "red", T = "green", L="blue"))
  
  
  pheatmap(t(data),fontsize=8, fontsize_row=8, clustering_distance_rows = smplDist,
           clustering_distance_cols = smplDist, clustering_method = clstDist, 
           border_color = border.col, cluster_rows = colV, 
           cluster_cols = rowV, annotation_col=classes, annotation_colors = ann_colors)
  
  dev.off()
}

determineIndex <-function(a) {
  indexes <- which(a==max(a))
  return (indexes[1])
}

datasets <- c("NormalvsTumor", "NormalvsLesionvsTumor")

#Establecemos los métodos a probar
methods <- c("single",
             "complete",
             "average",
             "mcquitty",
             "ward.D",
             "ward.D2",
             "centroid",
             "median")

threshold <- 0.8

output <- "results/"

for(fileName in datasets){
  
  #load the dataset and ranking of features
  dataset <-  read.csv(paste(fileName,".csv",sep = ""), sep = ",", row.names = 1)
  
  rankingFeature <- read.csv(paste(output,fileName,"-sortedfeatures.csv", sep = ""), sep = "\t")
  
  nFeatures <- nrow(rankingFeature)
  
  classIndex <- ncol(dataset)
  
  numberClasses <- length(levels(dataset[,ncol(dataset)]))
  
  #execute the algorithms
  for(method in methods){
    
    outputTemp <- paste0(output, "Clustering/",method, "/", fileName,"/")
    
    if(!dir.exists(outputTemp)){
      
      dir.create(outputTemp, recursive = TRUE)
    }
    
    #The dataframe where we stored the results for this algorithm
    df <- data.frame(ID = c(0), NumberAtts=c(0), Atts=c(" "), purity=c(0))
    
    df <- df[-1,]
    
    #Execute the search by ranking of features
    #######################
    
    #All features will be considered as pivot one time
    for(feature in 1:nFeatures){
      
      pivotFeature <- rankingFeature[feature,"Index"]
      
      listFeatures <- c(pivotFeature)
      
      purityGlobal <- -1
      
      for(otherFeature in feature:nFeatures)
      {
        
        # The last case
        if(otherFeature != feature)
        {
          listFeatures <- c(listFeatures, rankingFeature[otherFeature,"Index"])
        }
        
        #extract the subset
        subdataset <- as.data.frame(dataset[, listFeatures])
        colnames(subdataset) <- colnames(dataset)[listFeatures]
        
        #pass this subdataset to the clustering algorithm
        clustering <- hclust(dist(subdataset), method = method)
        clustering.cut <- cutree(clustering, numberClasses)
        
        # compute purity
        purity <- ClusterPurity(clustering.cut, dataset$Class)
        
        #Assign the true class index to each cluster
        #tab <-table(clustering.cut, dataset$Class)
        #trueClassIndex <- apply(tab, 1, determineIndex)
        
        #classPredicted <- clustering.cut
        
        #for(clusterIndex in unique(clustering.cut)){
          
          #classPredicted[classPredicted==clusterIndex] <- trueClassIndex[clusterIndex]
        #}
        
        #Computing the auc
       #if(numberClasses == 2)
        #{
         # rocT <- roc(as.numeric(dataset$Class), classPredicted) 
        #}else{
         # rocT <- multiclass.roc(as.numeric(dataset$Class), classPredicted)
        #}
        
        #auc <- rocT$auc[1]
        
        #print(purity)
        
        #register the model
        if(purity >= threshold){
          
          df <- rbind(df, data.frame(ID = nrow(df) + 1, NumberAtts= length(colnames(subdataset)), 
                                    Atts= paste(colnames(subdataset), collapse = ' '), 
                                    purity=purity))
          if(ncol(subdataset)>1){
            PlotHeatMap(data = dataset[,c(colnames(subdataset),"Class")], imgName = paste0(outputTemp, "HeatMap-", nrow(df)), clstDist = method)
          }
          
        }
        
        if(purityGlobal < purity){
          
          purityGlobal <- purity
        }
        else{
          #remove the last feature added. due to did not apport any advantage
          listFeatures <- listFeatures[1:(length(listFeatures)-1)]
        }
      }
    }
    
    #############End of search by ranking
    
    #order the rows of the dataframe
    df <- df[order(-df[,ncol(df)]), ]
    
    #save the dataframe
    write.table(df, file= paste0(outputTemp, method,".csv"), quote = FALSE, sep="," , row.names = FALSE, 
                col.names = TRUE)
  }
}

