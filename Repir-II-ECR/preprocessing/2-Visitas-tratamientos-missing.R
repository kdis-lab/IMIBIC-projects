library(visdat)
library(naniar)
library(UpSetR)

data <- read.csv("datos-visitas-tratamientos.csv", na.strings = "")

# Para eliminar los rows correspondientes a cambios
data <- data[!is.na(data$Fecha_visita), ]

# Para eliminar los rows de visita que no tienen datos en ningun campo, i.e. todos los valores son NA
v <- apply(data[,-c(1,2)], 1, function(x) all(is.na(x)))

data <- data[!v,]

data$Fecha_visita <- as.Date(data$Fecha_visita)

visualize_missing <- F

# Remplazo de missing values teniendo en cuenta variables condicionales que dependen de la respuesta de otra variable
########################################
########################################

#En el caso de IECA y ARA solo se cumplimentan si en el campo Antiproteinuricos han puesto que Si. 
#Esta es la razon de que haya tantos valores perdidos en IECA /ARA. La solucion es ponerlas en No si el campo Antiproteinuricos es No.

data[(data$Antiproteinuricos == "No" & !is.na(data$Antiproteinuricos)) & is.na(data$IECA), "IECA"] <- "No"
data[(data$Antiproteinuricos == "No" & !is.na(data$Antiproteinuricos)) & is.na(data$ARA), "ARA"] <- "No"

#En el caso de Quelantes_calcio y Quelantes_libre_calcio solo se cumplimentan si en el campo Quelantes_fosforo han puesto que Si.
#Esta es la razon de que haya tantos valores perdidos en estas dos variables. La solucion es ponerlas en No si el campo Quelantes_fosforo es No.
data[(data$Quelantes_fosforo == "No" & !is.na(data$Quelantes_fosforo)) & is.na(data$Quelantes_calcio), "Quelantes_calcio"] <- "No"
data[(data$Quelantes_fosforo == "No" & !is.na(data$Quelantes_fosforo)) & is.na(data$Quelantes_libres_calcio), "Quelantes_libres_calcio"] <- "No"

#En el caso de Vitamina_D3, dihidroxivintamina, hidroxivitamina, otras_vitaminas solo se cumplimentan si en el campo Toma_vitamina_D han puesto que Si.
#Esta es la razon de que haya tantos valores perdidos en estas variables. La solucion es ponerlas en No si el campo  Toma_vitamina_D es No.
data[(data$Toma_Vitamina_D == "No" & !is.na(data$Toma_Vitamina_D)) & is.na(data$Vitamina_D3), "Vitamina_D3"] <- "No"
data[(data$Toma_Vitamina_D == "No" & !is.na(data$Toma_Vitamina_D)) & is.na(data$hidroxivitamina), "hidroxivitamina"] <- "No"
data[(data$Toma_Vitamina_D == "No" & !is.na(data$Toma_Vitamina_D)) & is.na(data$dihidroxivintamina), "dihidroxivintamina"] <- "No"
data[(data$Toma_Vitamina_D == "No" & !is.na(data$Toma_Vitamina_D)) & is.na(data$otra_vitamina), "otra_vitamina"] <- "No"

#En el caso de IECA/ARA solo se cumplimenta si en el campo Hipotensores han puesto que Si.
#Esta es la razon de que haya tantos valores perdidos en esta variable. La solucion es ponerla en No si el campo  Hipotensores es No.
data[(data$Hipotensores == "No" & !is.na(data$Hipotensores)) & is.na(data$hipotensor_IECA_ARA), "hipotensor_IECA_ARA"] <- "No"

# Eliminar filtrado glomerular en los casos que tienen valor Infinito, esto es debido a que la Cr es 0
data[is.infinite(data$Filtrado_glomerular), "Filtrado_glomerular"] <- NA

#Visualizing missing data
#########################################
#########################################

if(visualize_missing){
  
  # Primero visualizar las variables correspondientes a visitas
  vis_dat(data[, c(2:27, 54)])
  # Luego visualizar las variables correspondientes a tratamientos
  vis_dat(data[, c(28:53)])
  
  gg_miss_var(data[, c(2:27, 54)], show_pct = T)
  gg_miss_var(data[, c(28:53)], show_pct = T)
  
  # Setting nintersects to NA it will plot all sets and all intersections.
  gg_miss_upset(data[,c(2:27, 54)], nsets = n_var_miss(data[,-1]), nintersects = 50)
  gg_miss_upset(data[, c(28:53)], nsets = n_var_miss(data[,-1]), nintersects = NA)
  
  # This plot shows the number of missing values in each case
  gg_miss_case(data[,c(2:27, 54)], show_pct = T)
  gg_miss_case(data[, c(28:53)], show_pct = T)
  
  # This plot shows the cumulative sum of missing values, reading the rows of the dataset from the top to bottom
  gg_miss_case_cumsum(data[,c(2:27, 54)], breaks = 500)
  gg_miss_case_cumsum(data[, c(28:53)], breaks = 500)
}

# Eliminar los outliers.
#########################################
#########################################
indexes <- (data$Peso < 3 | data$Peso > 110) & !is.na(data$Peso)
print(paste0("Outliers in Peso ", sum(indexes)))
data[indexes, c("Peso", "SC")] <- NA  # En este caso el SC tambien se pone igual a NA porque este se calcula a partir del peso

indexes <- (data$Talla < 45 | data$Talla > 200) & !is.na(data$Talla)
print(paste0("Outliers in Talla ", sum(indexes)))
# En este caso el SC tambien se pone igual a NA porque este se calcula a partir del peso. También se pone el FG en NA ya que este se calcula a partir de la Cr y Talla
data[indexes, c("Talla", "SC", "Filtrado_glomerular")] <- NA

indexes <- (data$SC < 0.05 | data$SC > 2) & !is.na(data$SC)
print(paste0("Outliers in SC ", sum(indexes)))
data[indexes, "SC"] <- NA # Eliminar casos especificos de SC

indexes <- (data$Perimetro_cefalico < 30 | data
            $Perimetro_cefalico > 60) & !is.na(data$Perimetro_cefalico)
print(paste0("Outliers in Perimetro_cefalico ", sum(indexes)))
data[indexes, "Perimetro_cefalico"] <- NA

indexes <- (data$IMC < 5 | data$IMC > 40) & !is.na(data$IMC)
print(paste0("Outliers in IMC ", sum(indexes)))
data[indexes, "IMC"] <- NA

indexes <- (data$PAS < 60 | data$PAS > 200) & !is.na(data$PAS)
print(paste0("Outliers in PAS ", sum(indexes)))
data[indexes, "PAS"] <- NA

indexes <- (data$PAD < 30 | data$PAD > 120) & !is.na(data$PAD)
print(paste0("Outliers in PAD ", sum(indexes)))
data[indexes, "PAD"] <- NA

indexes <- (data$BUN < 5 | data$BUN > 150) & !is.na(data$BUN)
print(paste0("Outliers in BUN ", sum(indexes)))
data[indexes, "BUN"] <- NA

indexes <- (data$Na < 100 | data$Na > 160) & !is.na(data$Na)
print(paste0("Outliers in Na ", sum(indexes)))
data[indexes, "Na"] <- NA

indexes <- (data$K < 2 | data$K > 7) & !is.na(data$K)
print(paste0("Outliers in K ", sum(indexes)))
data[indexes, "K"] <- NA

indexes <- (data$HCO3 < 5 | data$HCO3 > 40) & !is.na(data$HCO3)
print(paste0("Outliers in HCO3 ", sum(indexes)))
data[indexes, "HCO3"] <- NA

indexes <- (data$Hb < 5| data$Hb > 20) & !is.na(data$Hb)
print(paste0("Outliers in Hb ", sum(indexes)))
data[indexes, "Hb"] <- NA

indexes <- (data$Hto < 15| data$Hto > 60) & !is.na(data$Hto)
print(paste0("Outliers in Hto ", sum(indexes)))
data[indexes, "Hto"] <- NA

indexes <- (data$Ferritina < 10 | data$Ferritina > 400) & !is.na(data$Ferritina)
print(paste0("Outliers in Ferritina ", sum(indexes)))
data[indexes, "Ferritina"] <- NA

indexes <- (data$Indice_transferrina < 5 | data$Indice_transferrina > 40) & !is.na(data$Indice_transferrina)
print(paste0("Outliers in Indice_transferrina ", sum(indexes)))
data[indexes, "Indice_transferrina"] <- NA

indexes <- (data$Ca < 6 | data$Ca > 12) & !is.na(data$Ca)
print(paste0("Outliers in Ca ", sum(indexes)))
data[indexes, "Ca"] <- NA

indexes <- (data$P < 2 | data$P > 12) & !is.na(data$P)
print(paste0("Outliers in P ", sum(indexes)))
data[indexes, "P"] <- NA

indexes <- (data$PTH < 10 | data$PTH > 1000) & !is.na(data$PTH)
print(paste0("Outliers in PTH ", sum(indexes)))
data[indexes, "PTH"] <- NA

# Eliminamos todos los registros relacionados que tienen Metodo_PTH = BioPTH por ser muy pocos.
data[data$Metodo_PTH == "BioPTH" & !is.na(data$Metodo_PTH), "PTH"] <- NA

indexes <- data$X25.OH < 5 & !is.na(data$X25.OH)
print(paste0("Outliers in 25-OH ", sum(indexes)))
data[indexes, "X25.OH"] <- NA

# Eliminar variables redundantes
########################################
########################################

# Perimetro_cefalico, tiene en total 7274 valores perdidos, lo que representa el 95% del total de datos.
data$Perimetro_cefalico <- NULL

# Ya saber el metodo PTH no hace falta porque en el campo PTH solo se recoge cuando el método era Intacta
data$Metodo_PTH <- NULL

# Al completarse IECA y ARA a partir de este, no tiene logica que se siga considerando ya que IECA y ARA son los unicos Antiproteinuricos
data$Antiproteinuricos <- NULL

# Al completarse Quelantes_calcio y Quelantes_libre_calcio a partir de este, no tiene logica que se siga considerando
data$Quelantes_fosforo <- NULL

# Al completarse Vitamina_D3 y otras a partir de este, no tiene logica que se siga considerando
data$Toma_Vitamina_D <- NULL

# Al completarse hipotensor_IECA_ARA de este, no tiene logica que se siga considerando
data$Hipotensores <- NULL

#Salvar el dataset con valores perdidos
write.csv(data, file= "datos-visitas-tratamientos-clean-with-missing.csv", row.names = F, na = "")