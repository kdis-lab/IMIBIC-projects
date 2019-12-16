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

if(visualize_missing){
  #Visualizing missing data
  #########################################
  #########################################
  
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
data[indexes, c("Peso", "SC", "IMC")] <- NA  # En este caso el SC e IMC tambien se ponen igual a NA porque este se calcula a partir del peso

indexes <- (data$Talla < 45 | data$Talla > 200) & !is.na(data$Talla)
print(paste0("Outliers in Talla ", sum(indexes)))
# En este caso el SC y IMC tambien se ponen igual a NA porque este se calcula a partir de la talla. También se pone el FG en NA ya que este se calcula a partir de la Cr y Talla
data[indexes, c("Talla", "SC", "IMC", "Filtrado_glomerular")] <- NA

#No se filtrara el SC ya que este depende de peso y talla, y ya estos dos se filtran anteriormente
#No se filtrara el IMC ya que este depende de peso y talla, y ya estos dos se filtran anteriormente

indexes <- (data$Perimetro_cefalico < 30 | data
            $Perimetro_cefalico > 60) & !is.na(data$Perimetro_cefalico)
print(paste0("Outliers in Perimetro_cefalico ", sum(indexes)))
data[indexes, "Perimetro_cefalico"] <- NA

indexes <- (data$PAS < 60 | data$PAS > 200) & !is.na(data$PAS)
print(paste0("Outliers in PAS ", sum(indexes)))
data[indexes, "PAS"] <- NA

indexes <- (data$PAD < 30 | data$PAD > 120) & !is.na(data$PAD)
print(paste0("Outliers in PAD ", sum(indexes)))
data[indexes, "PAD"] <- NA

# Los valores normales de Urea se extrajeron a partir de los dados para el BUN, que fueron 5, 150[]
#No se filtrara el BUN ya que este depende de Urea, y ya esta se filtra anteriormente
indexes <- (data$Urea < 10.714 | data$Urea > 321.42) & !is.na(data$Urea)
print(paste0("Outliers in Urea ", sum(indexes)))
data[indexes, c("Urea", "BUN")] <- NA # Se pone admas el BUN como NAN porque este se calcula a partir de la Urea

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

indexes <- (data$Cr < 0.3 | data$Cr > 20) & !is.na(data$Cr)
print(paste0("Outliers in Cr ", sum(indexes)))
data[indexes, c("Cr", "Filtrado_glomerular")] <- NA #  Aqui ya se elimina el filtrado glomerular en los casos que tienen valor Infinito, esto es debido a que la Cr es 0

# Se eliminan los registros que contengan un FG > de 90 ya que estos no deberían estar en el sistema.
indexes <- (data$Filtrado_glomerular > 90) & !is.na(data$Filtrado_glomerular)
print(paste0("Outliers in Filtrado_glomerular ", sum(indexes)))
data <- data[!indexes, ]

# Eliminar variables
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

#Eliminar pacientes que tienen solo una visita, o que quedaron con una sola visita tras eliminar registros. Estos pacientes no entraran en el analisis, ni en la construccion del modelo
########################################
########################################
CountId <- data.frame(table(data$Id))

id_equal1 <- as.numeric(as.character(CountId[CountId$Freq == 1, "Var1"]))

data <- data[!(data$Id %in% id_equal1),]

#Salvar el dataset con valores perdidos
write.csv(data, file= "datos-visitas-tratamientos-clean-with-missing.csv", row.names = F, na = "")

# Salvar el dataset que tiene los id y la cantidad de visitas por cada paciente

CountId <- data.frame(table(data$Id))

CountId <- data.frame("Id"= as.numeric(as.character(CountId$Var1)), "Visits"= CountId$Freq)
           
write.csv(CountId, file= "datos-id-visitas.csv", row.names = F, na = "")
