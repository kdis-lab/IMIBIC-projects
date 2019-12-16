rm(list=ls())
library(ggpubr)

# load the csv
output <- "results/statistical-tests/"

fileName <- "NormalvsLesionvsTumor"

outputT <-paste0(output, fileName, "/")

if(!dir.exists(outputT)){
  dir.create(outputT, recursive = T)
}

#load the dataset
dataset <-read.csv(paste0(fileName, ".csv"), sep = ",", na.strings = "", row.names = 1)

colnamesT <- colnames(dataset)[-ncol(dataset)]

classes <- levels(dataset[, ncol(dataset)])

colClassName <- colnames(dataset)[ncol(dataset)]

#for each column
for(nameC in colnamesT){
  
  formula <- as.formula(paste0(nameC," ~ Class"))
  a <- compare_means(formula, data = dataset, method = "kruskal.test")
  
  #Pairwise comparisons
  combinations <-combn(classes,2)
  
  my_comparisons <- list()
  
  for(com in 1:ncol(combinations)){
    my_comparisons<-c(my_comparisons, list(combinations[,com]))
  }
  
  # Default method = "kruskal.test" for multiple groups
  ggboxplot(dataset, x = "Class", y = nameC,
            color = "Class", palette = "jco") + 
    stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
    stat_compare_means(label.y = max(dataset[,nameC])) # Add global p-value
  
  ggsave(width = 5, height = 5, filename = paste(outputT,nameC,".png",sep = ""), bg = "transparent")
}