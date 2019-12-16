library(ggpubr)

#https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/

# load the csv
output <- "results/statistical-tests/"

fileName <- "NormalvsTumor"

outputT <-paste0(output, fileName, "/")

if(!dir.exists(outputT)){
  dir.create(outputT, recursive = T)
}

#load the dataset
dataset <-read.csv(paste0(fileName, ".csv"), sep = ",", na.strings = "")

colnamesT <- colnames(dataset)[-ncol(dataset)]

classes <- levels(dataset[, ncol(dataset)])

colClassName <- colnames(dataset)[ncol(dataset)]

#for each column
for(nameC in colnamesT){
  
  formula <- as.formula(paste0(nameC," ~ Class"))
  
  a<-compare_means(formula, data = dataset)
  
  p <- ggboxplot(dataset, x = "Class", y = nameC,
                 color = "Class", palette = "jco")
  #  Add p-value
  p + stat_compare_means()
  
  # To change to t-test
  #p + stat_compare_means(method = "t.test")
  
  ggsave(width = 5, height = 5, filename = paste(outputT,nameC,".png",sep = ""), bg = "transparent")
}