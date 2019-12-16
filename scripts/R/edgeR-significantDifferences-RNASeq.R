rm(list = ls())
library(edgeR)

data <- read.csv("NormalvsTumor-transformed.csv", sep = "\t", na.strings = "")

condicion <- factor(c(rep("N", 19), rep("T", 19)))

dge <- DGEList(data, group = condicion)
dge.n <- calcNormFactors(dge, method = "TMM") # Normalización por medias ajustadas de M-Valores

dge.c <- estimateCommonDisp(dge.n) # Estimamos la dispersión común
dge.t <- estimateTagwiseDisp(dge.c) # Y la de cada gen
et.c <- exactTest(dge.c)
et.t <- exactTest(dge.t)

topTags(et.c) # Top genes dispersión común
topTags(et.t) # Top genes dispersión distinta para cada gen

# Determinamos los genes significativos

de.c <- decideTestsDGE(et.c, adjust.method = "BH", p.value = .05)
de.t <- decideTestsDGE(et.t, adjust.method = "BH", p.value = .05)

summary(de.t) # Resumen de los genes
summary(de.c)

# Genes up y down para cada clase
# 1 Up, 0 No significativo, -1 Down
# Dispersión común

# Upregulados

upre.c <- which(de.c@.Data == 1)
upre.c <- et.c[upre.c,]
upre.c <- rownames(topTags(upre.c, n = 10)) # 100 genes TOP up

# Downregulados

downre.c = which(de.c@.Data == -1)
downre.c = et.c[downre.c,]
downre.c = rownames(topTags(downre.c, n = 10)) # 100 genes TOP down

# Cada gen tiene su dispersión

# Upregulados

upre.t = which(de.t@.Data == 1)
upre.t = et.t[upre.t,]
upre.t = rownames(topTags(upre.t, n = 50)) # 50 genes TOP up

# Downregulados

downre.t = which(de.t@.Data == -1)
downre.t = et.t[downre.t,]
downre.t = rownames(topTags(downre.t, n = 50)) # 50 genes TOP up

# Hasta aquí hemos obtenido los nombres de los distintos genes up y down
# por los métodos de dispersión común y entre genes.
# Agrupamos los genes (Grupos de genes con expresión diferencial significativa)

genes.c = c(upre.c, downre.c) # Dispersión común a todos los genes
genes.t = c(upre.t, downre.t) # Dispersión por gen
