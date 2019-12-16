rm(list = ls())
source("utils.R")

mergedDataset <- read.csv("datos-demograficos-visitas-tratamientos-clean-with-missing.csv", na.strings = "")

# By deafult the imputation of numeric values is made by mean function
meanFunction <- F

longitudinal <- F

# Either of the follow longitudinal imputation function can be used:
# average_previous_values, locf, average_before_after_values, nocb, average_last_next_values
longitudinalFuncName <- "average_previous_values"

imputationLongitudinalFunction <- average_previous_values

i <- 0

for(col in colnames(mergedDataset)){
  
  i <- i+1
  
  #Los valores omitidos para las fechas y variables que se calculan a partir de otras no se calculan en este momento
  if(sum(startsWith(col, c("Id", "Fecha", "IMC", "SC", "BUN", "Filtrado_glomerular")))> 0)
  {
    print(paste0("Jumping ", col))
    next
  }
  
  # From 1 to 14, the variables are global
  # From 15 to 62, the variables are longitudinal
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
}

# Ahora se estiman los valores de variables dependientes

# Computing IMC
talla_peso <- mergedDataset[is.na(mergedDataset$IMC), c("Talla", "Peso")]
mergedDataset[is.na(mergedDataset$IMC), "IMC"] <- round(talla_peso$Peso * 10000/ (talla_peso$Talla ^ 2), digits = 2)

# Computing SC
talla_peso <- mergedDataset[is.na(mergedDataset$SC), c("Talla", "Peso")]
mergedDataset[is.na(mergedDataset$SC), "SC"] <- round(sqrt(talla_peso$Peso * talla_peso$Talla/3600), digits = 2)

# Computing BUN
urea <- mergedDataset[is.na(mergedDataset$BUN), "Urea"]
mergedDataset[is.na(mergedDataset$BUN), "BUN"] <- round(urea/2.1428, digits = 2)

# Computing Filtrado_glomerular
filter_before_2013 <- is.na(mergedDataset$Filtrado_glomerular) & (get_age("2012-12-31", mergedDataset$Fecha_visita) <= 0)

filter_before_2013_less_1 <- filter_before_2013 &  (mergedDataset$Edad_visita <= 1)
talla_cr <- mergedDataset[filter_before_2013_less_1, c("Talla", "Cr")]
mergedDataset[filter_before_2013_less_1, "Filtrado_glomerular"] <- talla_cr$Talla * 0.45 / talla_cr$Cr

filter_before_2013_between_1_14 <- filter_before_2013 &  (mergedDataset$Edad_visita > 1) & (mergedDataset$Edad_visita <=14)
talla_cr <- mergedDataset[filter_before_2013_between_1_14, c("Talla", "Cr")]
mergedDataset[filter_before_2013_between_1_14, "Filtrado_glomerular"] <- talla_cr$Talla * 0.55 / talla_cr$Cr

filter_before_2013_greater_14_male <- filter_before_2013 &  (mergedDataset$Edad_visita > 14) & (mergedDataset$Sexo == "Hombre")
talla_cr <- mergedDataset[filter_before_2013_greater_14_male, c("Talla", "Cr")]
mergedDataset[filter_before_2013_greater_14_male, "Filtrado_glomerular"] <- talla_cr$Talla * 0.70 / talla_cr$Cr

filter_before_2013_greater_14_female <- filter_before_2013 &  (mergedDataset$Edad_visita > 14) & (mergedDataset$Sexo == "Mujer")
talla_cr <- mergedDataset[filter_before_2013_greater_14_female, c("Talla", "Cr")]
mergedDataset[filter_before_2013_greater_14_female, "Filtrado_glomerular"] <- talla_cr$Talla * 0.57 / talla_cr$Cr

filter_after_2013 <- is.na(mergedDataset$Filtrado_glomerular) & (get_age("2012-12-31", mergedDataset$Fecha_visita) > 0)

filter_after_2013_less_1 <- filter_after_2013 & (mergedDataset$Edad_visita <= 1)
talla_cr <- mergedDataset[filter_after_2013_less_1, c("Talla", "Cr")]
mergedDataset[filter_after_2013_less_1, "Filtrado_glomerular"] <- talla_cr$Talla * 0.45 / talla_cr$Cr

filter_after_2013_greater_1 <- filter_after_2013 & (mergedDataset$Edad_visita > 1)
talla_cr <- mergedDataset[filter_after_2013_greater_1, c("Talla", "Cr")]
mergedDataset[filter_after_2013_greater_1, "Filtrado_glomerular"] <- talla_cr$Talla * 0.413 / talla_cr$Cr

# Eliminar los campos fechas, no serviran para mas nada
mergedDataset$Fecha_visita <- NULL
mergedDataset$Fecha_nacimiento <- NULL
mergedDataset$Fecha_diagnostico_ERC <- NULL

# Checking that all NAs have been removed
sapply(mergedDataset, function(x) sum(is.na(x)))

numericFunc <- "mean"

if(!meanFunction)
  numericFunc <- "median"

write.csv(mergedDataset, file = paste0("data-missing-imputation/datos-demograficos-visitas-tratamientos-missing-imputation-", numericFunc, "-", longitudinalFuncName ,".csv"), na = "", row.names = F)