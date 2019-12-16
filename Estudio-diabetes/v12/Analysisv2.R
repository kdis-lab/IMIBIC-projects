fileName <- paste0("Alldiabetes")

scenario <-"4h"

genesScenarios=list("0h"= c("U20h", "U40h", "U6ATAC0h", "ESRP10h", "SRSF10h"),
                    "4h"=c("U44h", "U24h", "U64h", "U6ATAC4h", "U4ATAC4h", "U124h", "NOVA14h", "ESRP14h", "SRSF14h"))
#load the dataset
dataset <-read.csv(paste0(fileName, ".csv"), sep = ",", na.strings = "", row.names = 1)

meanBMIClassS <- aggregate(BMI ~ Class, dataset, mean)[2, "BMI"]

# To detect the element more distant from the mean
distance <- (dataset[dataset$Class=="S", "BMI"] - meanBMIClassS)^2

#Indice relativo respecto a la clase S
row <- which(distance==max(distance))

# The number of samples in class N
nrowN <- nrow(dataset[dataset$Class=="N",])

datasetTemp <- dataset[-(row+nrowN),]

t.test(as.formula("BMI ~ Class"), data= datasetTemp, alternative="less")
t.test(as.formula("Edad ~ Class"), data= datasetTemp)

sexo <- as.factor(datasetTemp$Sexo)
levels(sexo) <- c("H", "M")
tab <-table(sexo, datasetTemp$Class)
tab

chisq.test(tab)

for(gen in genesScenarios[[scenario]]){
  print(t.test(as.formula(paste0(gen, " ~ Class")), data= datasetTemp, alternative="greater"))
}

