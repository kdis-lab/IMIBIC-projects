library(pheatmap)
library(Cairo)

PlotHeatMap<-function(data, imgName="HeatMap", format="png", smplDist= "euclidean", 
                      clstDist= "ward.D", viewOpt="detail", rowV=TRUE, colV=TRUE, border=TRUE){
  
  classes <- data[ncol(data)]
  data <- data[-ncol(data)]
  
  colnames(data) <- substr(colnames(data),1,18) # some names are too long
  
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
  ann_colors = list(Class=c(N = "red", S = "green"))
  
  pheatmap(t(data),fontsize=8, fontsize_row=8, clustering_distance_rows = smplDist,
           clustering_distance_cols = smplDist, clustering_method = clstDist, 
           border_color = border.col, cluster_rows = colV, 
           cluster_cols = rowV, annotation_col=classes, annotation_colors = ann_colors)
  
  dev.off()
}