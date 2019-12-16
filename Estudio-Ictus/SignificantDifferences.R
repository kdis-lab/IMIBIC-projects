library(ggplot2)
library(ggsignif)

setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

dataset <- read.csv("Ictus-preproc.csv")

colnamesT <- colnames(dataset)[-ncol(dataset)]

classes <- levels(dataset$Class)

colClassName <- colnames(dataset)[ncol(dataset)]

showOutliers <- TRUE

#for each column
for(nameC in colnamesT){
  
  test <- t.test(dataset[dataset$Class == "N", nameC], dataset[dataset$Class == "S", nameC])
  
  output <- "results/"
  
  if(0.01 <= test$p.value && test$p.value <= 0.05){
    output <- paste(output,"0.05/", sep="")
    } else if (0.001 <= test$p.value && test$p.value <= 0.01) {
    output <- paste(output,"0.01/", sep = "")
  } else if (test$p.value <= 0.001){
    output <- paste(output,"0.001/", sep = "")
  } else {
    output <- paste(output, "NS/", sep = "")
  }
  
  boxplotGraghic <- ggplot(dataset, aes_string(x=colClassName, y= nameC)) +
                    stat_boxplot(geom = "errorbar", width = 0.5) + 
                    geom_boxplot() + 
                    geom_signif(comparisons = list(c("N", "S")), map_signif_level=TRUE, test = "t.test", na.rm = TRUE)
  
  ggsave(width = 5, height = 5, filename = paste(output,nameC,".png",sep = ""), bg = "transparent")
}