minmax <- function(x) {
  
  minValue <-min(x, na.rm=TRUE)
  maxValue <- max(x, na.rm=TRUE)
  
  (x - minValue)/(maxValue - minValue)
}

normalization <- function(data, func){
  
  # use lapply to apply a function to every column in a data frame
  normalized <- as.data.frame(lapply(data, func))
  
  normalized
}

# load the csv
path <- "D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/tumores-cerebrales/Tumor-IIIvsIV-raw"

database <-read.csv(paste(path,".csv", sep = ""))

#If the minmax method is prefered, call function minmax
database[,1:(ncol(database)-1)] <- normalization(database[,1:(ncol(database)-1)],minmax)

#If the z-score standarization is prefered, call built-in function scale()
#database[,1:(ncol(database)-1)] <- normalization(database,scale)

write.table(database, file= paste(path,"-norm.csv",sep = ""), 
            quote = FALSE, sep="," , row.names = FALSE, col.names = TRUE, na = "")