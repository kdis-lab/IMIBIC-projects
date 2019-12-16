library(pheatmap)
library(Cairo)

setwd("/media/ogreyesp/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

source("HeatMap.R")

#Load the dataset
datasetName <- "Ictus-preproc-withoutOutliers.csv"
data <- read.csv(datasetName)

#Load csv with cluster configurations

pathOutput <- "results/ranking-based-Clustering/"

clusters <- read.csv(paste(pathOutput,"Hierarchical-Clustering.csv", sep = ""))

# The threshold value to filter out the clustering configurations
threshold <- 1

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
      
      imgName <- paste(pathOutput,(length(indexes)-1), "-", i, sep = "")
      
      PlotHeatMap(data = newData, imgName = imgName)
    }
  }
}