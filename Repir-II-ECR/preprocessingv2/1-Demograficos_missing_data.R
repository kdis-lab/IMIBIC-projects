library(visdat)
library(naniar)
library(UpSetR)
source("utils.R")

data <- read.csv("datos-demograficos.csv", na.strings = "")

# Para eliminar los rows correspondientes a visitas
data <- data[!is.na(data$Sexo),]

data$Fecha_nacimiento <- as.Date(data$Fecha_nacimiento)
data$Fecha_inicio_ERC <- as.Date(data$Fecha_inicio_ERC)
data$Fecha_diagnostico_ERC <- as.Date(data$Fecha_diagnostico_ERC)

visualize_missing <- F

if(visualize_missing){
#Visualizing missing data
#########################################
#########################################
  vis_dat(data[, -1])
  
  gg_miss_var(data[, -1], show_pct = T)
  
  # Setting nintersects to NA it will plot all sets and all intersections.
  gg_miss_upset(data[, -1], nsets = n_var_miss(data[,-1]), nintersects = NA)
  
  # This plot shows the number of missing values in each case
  gg_miss_case(data, show_pct = T)
  
  # This plot shows the cumulative sum of missing values, reading the rows of the dataset from the top to bottom
  gg_miss_case_cumsum(data, breaks = 100)

}

#Creacion de nuevas variables
#########################################
#########################################
#Convertimos la fecha de nacimiento y Fecha_diagnostico_ERC en Edad_diagnostico, para poder trabajar con esta variable
data$Edad_diagnostico <- round(get_age(data$Fecha_nacimiento, data$Fecha_diagnostico_ERC, dec = T), 2)

# Eliminar los outliers.
#########################################
#########################################

#Peso_nacimiento. Lo que hice fue borrar manualmente valores atípicos. 709 valores omitidos
indexes <- (data$Peso_nacimiento < 0.5 | data$Peso_nacimiento > 5) & !is.na(data$Peso_nacimiento)
print(paste0("Outliers in Peso_nacimiento ", sum(indexes)))
data[indexes, "Peso_nacimiento"] <- NA

#Semanas_gestacion. Se eliminan aquellos valores que estan por debajo de las 25 semanas y por encima de las 42 semanas.
indexes <- (data$Semanas_gestacion < 25 | data$Semanas_gestacion > 42) & !is.na(data$Semanas_gestacion)
print(paste0("Outliers in Peso_nacimiento ", sum(indexes)))
data[indexes, "Semanas_gestacion"] <- NA

# Se elimina la fecha de inicio ERC ya que en la mayoria de los casos no se sabe
data$Fecha_inicio_ERC <- NULL

#Salvar el dataset con valores perdidos
write.csv(data, file= "datos-demograficos-clean-with-missing.csv", row.names = F, na = "")
