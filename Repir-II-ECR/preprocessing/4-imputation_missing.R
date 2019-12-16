rm(list = ls())
source("../utils.R")

mergedDataset <- read.csv("datos-demograficos-visitas-tratamientos-clean-with-missing.csv", na.strings = "")

# By deafult the imputation of numeric values is made by mean function
meanFunction <- T

i <- 1

longitudinal <- F

# Either of the follow longitudinal imputation function can be used: 
# average_previous_values, locf, average_before_after_values, nocb, average_last_next_values
longitudinalFuncName <- "average_last_next_values"

imputationLongitudinalFunction <- average_last_next_values

for(col in colnames(mergedDataset)){
  
  # From 1 to 14, the variables are global
  # From 15 to 57, the variables are longitudinal
  if(i > 14)
  { print(paste0("Longitudinal variable ", col))
    longitudinal <- T}
  else{
    print(paste0("Global variable ", col))
  }
  
  data <- imputationLongitudinalFunction(mergedDataset, col, meanFunction = meanFunction, longitudinal = longitudinal)
  
  if(!is.null(data)) # missin values in the variable
  {
    mergedDataset[, col] <- data[, col]
  }
  
  i <- i+1
}

numericFunc <- "mean"

if(!meanFunction)
  numericFunc <- "median"

write.csv(mergedDataset, file = paste0("data-missing-imputation/datos-demograficos-visitas-tratamientos-missing-imputation-", numericFunc, "-", longitudinalFuncName ,".csv"), na = "", row.names = F)
