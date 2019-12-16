library(ggpubr)

# load the csv
output <- "results/statistical-tests/"

fileName <- "NormalvsTumor"

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
  a<-compare_means(formula, data = dataset, paired = TRUE)
  
  plot <- ggpaired(dataset, x = "Class", y = nameC,
           color = "Class", line.color = "gray", line.size = 0.4,
           palette = "jco") + stat_compare_means(paired = TRUE)
  
  ggsave(width = 5, height = 5, filename = paste(outputT,nameC,".png",sep = ""), bg = "transparent")
}