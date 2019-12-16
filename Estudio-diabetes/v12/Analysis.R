scenario <- "4h"

fileName <- paste0("Alldiabetes-", scenario)

genesScenarios=list("0h"=c("U20h","U40h", "U6ATAC0h", "U50h", "ESRP10h", "SRSF10h"), 
                    "4h"=c("U44h", "U24h", "U64h", "U6ATAC4h", "U4ATAC4h", "U124h", "NOVA14h", "ESRP14h", "SRSF14h"))
#load the dataset
dataset <-read.csv(paste0(fileName, ".csv"), sep = ",", na.strings = "", row.names = 1)

sexo <- as.factor(dataset$Sexo)
levels(sexo) <- c("H", "M")

wilcox.test(as.formula("BMI ~ DIABETES"), data= dataset)
wilcox.test(as.formula("Edad ~ DIABETES"), data= dataset)

tab <-table(sexo, dataset$DIABETES)
tab

chisq.test(tab)

for(gen in genesScenarios[[scenario]]){
  print(wilcox.test(as.formula(paste0(gen, " ~ DIABETES")), data= dataset))
}