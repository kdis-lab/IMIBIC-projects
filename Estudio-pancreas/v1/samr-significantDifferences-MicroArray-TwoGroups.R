rm(list=ls())
library(samr)

#Transforms the dataset

#data <- read.csv("NormalvsTumor.csv", sep = ",", row.names = 1, na.strings = "")
#data <-data.frame(t(data))
#Remove the last row
#data <- data[-nrow(data),]
#write.table(data, file = "NormalvsTumor-transformed.csv", na = "", sep = "\t", quote = F, row.names = T, col.names = T)

data <- read.csv("NormalvsTumor-transformed.csv", sep = "\t", na.strings = "")

########## two class unpaired

y <- c(rep(1,19),rep(2,19))
#y<-c(1:19,-1:-19)

samfit<- SAM(data, y, resp.type="Two class unpaired", fdr.output=0.001, 
             testStatistic = "wilcoxon", genenames = rownames(data), nperms=1000)

print(samfit)
plot(samfit)

# que delta se uso
print(samfit$del)

#cuales genes son significativos.
print(samfit$siggenes.table)

#cuales valores de delta se usaron
print(samfit$delta.table)
