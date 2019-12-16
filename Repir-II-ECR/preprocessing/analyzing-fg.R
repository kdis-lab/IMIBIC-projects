source("utils.R")

data <- read.csv("redcap_data_FILGLOFINAL.csv", na.strings = "")

sum(data$fg_auto_2013 > 90, na.rm = T)

sum(data$fg_auto > 90, na.rm = T)

data$fecha_visita <- as.Date(data$fecha_visita)

valores_conflicto <- !is.na(data$fg_auto) & !is.na(data$fg_auto_2013) & data$fg_auto < 90 & data$fg_auto_2013 > 90

# Existirian 1081 casos en los cuales segun la antigua formula ocurria una recuperacion de la funcion renal y segun la nueva formula que utiliza a partir del 2013 no habrian recuperado esa funcion renal.
sum(valores_conflicto)

#Salvar el nuevo dataset
write.csv(dataTemp, file= "/media/ogreyesp/DATA/workspace/IMIBIC/datasets/Proyecto-REPIR-II/datos-cambios-clean.csv", row.names = F, na = "")
