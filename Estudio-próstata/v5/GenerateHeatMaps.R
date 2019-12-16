library(pheatmap)
library(Cairo)

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

setwd("/media/ogreyesp/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/v5/")

#Load the dataset

data <- read.csv("Grasso-ControlesvsM.csv")

output <- "results/ControlesvsM/Clustering/"

#Load csv with cluster configurations

clusters <- read.csv(paste(output,"HierarchicalClustering.csv",sep=""))

# The threshold value to filter out the clustering configurations
threshold <- 0.9

if(nrow(clusters)!=0){
  
  #for each cluster
  
  for(i in 1:nrow(clusters)){
    
    auc <- clusters[i,"AUC"]
    
    if(auc >= threshold){
    
      atts <- clusters[i,"Atts"]
      items <- unlist(strsplit(as.character(atts), " "))
      
      indexes <- match(items,colnames(data))
      
      #construct the dataframe
      indexes <- c(indexes,ncol(data))
      newData <- data [,indexes]
      
      imgName <- paste(output, i,sep = "")
      
      PlotHeatMap(data = newData, imgName = imgName)
    }
  }
}