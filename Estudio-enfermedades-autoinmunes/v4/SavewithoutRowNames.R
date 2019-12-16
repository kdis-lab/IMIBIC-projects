setwd("/media/oscar/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/v4/without0s/")

fileNames <- c("LinfoAnt-preproc","LinfoCMIT-preproc","LinfoComplicObstet-preproc","LinfodsDNA-preproc",
               "LinfoHta-preproc", "LinfoTrombosis-preproc", "MonoAnt-preproc","MonoCMIT-preproc","MonoComplicObstet-preproc",
               "MonodsDNA-preproc", "MonoHta-preproc", "MonoTrombosis-preproc", "NeutroAnt-preproc","NeutroCMIT-preproc",
               "NeutroComplicObstet-preproc", "NeutrodsDNA-preproc", "NeutroHta-preproc", "NeutroTrombosis-preproc")

for(fileName in fileNames){
  
  mydata <- read.csv(paste(fileName,".csv",sep = ""))
  
  write.table(mydata, file = paste(fileName,"v2.csv",sep = ""), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ",")
}