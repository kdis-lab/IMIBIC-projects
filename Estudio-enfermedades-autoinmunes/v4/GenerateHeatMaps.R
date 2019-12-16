library(pheatmap)
library(Cairo)

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/")

PlotHeatMap<-function(data, imgName="HeatMap", format="png", smplDist= "euclidean", 
                      clstDist= "ward.D", viewOpt="detail", rowV=TRUE, colV=TRUE, border=TRUE){
  
  classes <- data[ncol(data)]
  data <- data[-ncol(data)]
  
  colnames(data)<-substr(colnames(data),1,18) # some names are too long
  
  rownames(data) <- 1:nrow(data)
  
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
  ann_colors = list(Clinico=c(N = "red", S = "green"))
  
  pheatmap(t(data),fontsize=8, fontsize_row=8, clustering_distance_rows = smplDist,
           clustering_distance_cols = smplDist, clustering_method = clstDist, 
           border_color = border.col, cluster_rows = colV, 
           cluster_cols = rowV, annotation_col=classes, annotation_colors = ann_colors)
  
  dev.off()
}

#distance measure used in clustering rows. Possible values are "correlation"
#for Pearson correlation and all the distances supported by dist, such as "euclidean", "maximum", 
# "manhattan", "canberra", "binary" or "minkowski".
#etc. If the value is none of the above it is assumed that a distance matrix is provided.
clustering_distance_rows <- "euclidean"

#distance measure used in clustering columns
clustering_distance_cols <- "euclidean"

#the agglomeration method to be used. This should be one of "ward.D", "ward.D2", "single", 
#"complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

clustering_method <- "ward.D"

#scale character indicating if the values should be centered and scaled in either the row
#direction or the column direction, or none. Corresponding values are "row",
#"column" and "none"

scaleOpt <- "row"

#Load the dataset

datasetNames <- c("LinfoAnt-preproc", "MonoAnt-preproc", "NeutroAnt-preproc", 
                  "LinfoCMIT-preproc", "MonoCMIT-preproc", "NeutroCMIT-preproc", 
                  "LinfoComplicObstet-preproc", "MonoComplicObstet-preproc", "NeutroComplicObstet-preproc",
                  "LinfodsDNA-preproc", "MonodsDNA-preproc", "NeutrodsDNA-preproc",
                  "LinfoHta-preproc", "MonoHta-preproc", "NeutroHta-preproc",
                  "LinfoTrombosis-preproc", "MonoTrombosis-preproc", "NeutroTrombosis-preproc")

# The threshold value to filter out the clustering configurations
threshold <- 0.80

#for each dataset
for(datasetName in datasetNames){
  
  datasetPath <- paste("datasets/enfermedades-autoinmunes/v4/with0s/",
                       datasetName, sep = "")
  
  data <- read.csv(paste(datasetPath,".csv", sep=""))
  
  #Load csv with cluster configurations
  
  pathOutput <- paste("reports/enfermedades-autoinmunes/v4/with0s/", 
                      datasetName, "v2/HierarchicalClusterer/notAllSignificantFactors", sep = "")
  
  clusters <- read.csv(paste(pathOutput,".csv",sep=""))
  
  if(nrow(clusters)!=0){
    
    #for each cluster
    
    for(i in 1:nrow(clusters)){
      
      auc <- clusters[i,"AUC"]
      
      if(auc >= threshold){
        
        atts <- clusters[i,"Atts"]
        items <- unlist(strsplit(as.character(atts), " "))
        
        indexes <- match(items, colnames(data))
        
        #construct the dataframe
        indexes <- c(indexes,ncol(data))
        newData <- data [,indexes]
        
        imgName <- paste(pathOutput,i,sep = "")
        
        PlotHeatMap(data = newData, imgName = imgName, smplDist = clustering_distance_rows, 
                    clstDist = clustering_method)
      }
    }
  }  
}