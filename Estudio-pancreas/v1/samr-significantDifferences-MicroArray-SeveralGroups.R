rm(list=ls())
library(samr)

#Transforms the dataset

#data <- read.csv("NormalvsLesionvsTumor.csv", sep = ",", row.names = 1, na.strings = "")
#data <-data.frame(t(data))
#Remove the last row
#data <- data[-nrow(data),]
#write.table(data, file = "NormalvsLesionvsTumor-transformed.csv", na = "", sep = "\t", quote = F, row.names = T, col.names = T)

dataOriginal <- read.csv("NormalvsLesionvsTumor.csv", sep = ",", row.names = 1, na.strings = "")
data <- read.csv("NormalvsLesionvsTumor-transformed.csv", sep = "\t", na.strings = "")

classes <- dataOriginal[,ncol(dataOriginal)]

classesIndex <- rep(1,nrow(dataOriginal))

classesIndex[classes=="L"] <- 2
classesIndex[classes=="T"] <- 3

########## multiclass analysis

samfit<- SAM(data, classesIndex, resp.type="Multiclass", fdr.output=0.001, genenames = rownames(data), nperms=1000)

print(samfit)
plot(samfit)

# que delta se uso
print(samfit$del)

#cuales genes son significativos.
print(samfit$siggenes.table)

#cuales valores de delta se usaron
print(samfit$delta.table)

#save the genes up
if(samfit$siggenes.table$ngenes.up>0)
{
  tab <- data.frame(Index= samfit$siggenes.table$genes.up[,2], Var= samfit$siggenes.table$genes.up[,1], Importance= samfit$siggenes.table$genes.up[,3])
  write.table(tab, file = "results/NormalvsLesionvsTumor-sortedfeatures-up.csv", quote = F, col.names = TRUE, row.names = FALSE, sep = "\t")
}

if(samfit$siggenes.table$ngenes.lo>0)
{
  tab <- data.frame(Index= samfit$siggenes.table$genes.lo[,2], Var= samfit$siggenes.table$genes.lo[,1], Importance= samfit$siggenes.table$genes.lo[,3])
  write.table(tab, file = "results/NormalvsLesionvsTumor-sortedfeatures-down.csv", quote = F, col.names = TRUE, row.names = FALSE, sep = "\t")
  
}

