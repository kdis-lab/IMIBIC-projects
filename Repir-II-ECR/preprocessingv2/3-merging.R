source("utils.R")

demograficos <- read.csv("datos-demograficos-clean-with-missing.csv", na.strings = "")
visitas_tratamientos <- read.csv("datos-visitas-tratamientos-clean-with-missing.csv", na.strings = "")

mergedDataset <- merge(demograficos, visitas_tratamientos, by = "Id")

mergedDataset$Fecha_diagnostico_ERC <- as.Date(mergedDataset$Fecha_diagnostico_ERC)
mergedDataset$Fecha_visita <- as.Date(mergedDataset$Fecha_visita)

#Ordenar por id y fecha de visita
mergedDataset <- mergedDataset[order(mergedDataset$Id, mergedDataset$Fecha_visita), ]

# Creamos una variable tiempo para representar el tiempo de la visita hasta el tiempo de inicio de estudio
mergedDataset$Tiempo_evolucion <- round(get_age(mergedDataset$Fecha_diagnostico, mergedDataset$Fecha_visita, dec = T), 2)
mergedDataset$Edad_visita <- round(get_age(mergedDataset$Fecha_nacimiento, mergedDataset$Fecha_visita, dec = T), 2)

uni <- unique(mergedDataset[,c("Id", "Fecha_visita")])

if(nrow(uni) < nrow(mergedDataset)){
    print("Patients with repeated visits")
}
  
# Para dejar la variable Filtrado_glomerular como ultima
mergedDataset <- mergedDataset[, c(1:60, 62, 63, 61)]

write.csv(mergedDataset, file= "datos-demograficos-visitas-tratamientos-clean-with-missing.csv", row.names = F, na = "")