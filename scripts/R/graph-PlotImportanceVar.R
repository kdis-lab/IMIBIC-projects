library(ggplot2)

data <- read.csv("/home/ogreyesp/Desktop/sortedfeatures.csv", sep = "\t")


data <- data[1:20,]

df <- data.frame(var = data$Var, imp=1-data$Importance)
#La siguiente linea es para cortar los nombres; si no utilizaria la de antes
#df <- data.frame(var = str_split_fixed(data$Var, "_", 2)[,1], imp = df$imp)

p <- ggplot(df, aes(x = reorder(var, -imp), y = imp, fill=-imp)) + geom_bar(stat = "identity") +
  scale_color_grey() + theme_classic() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position="none") + labs(x="Factores", y="Importancia")

# legend.position="none" es para que no aparezca la legenda en la grafica

p
