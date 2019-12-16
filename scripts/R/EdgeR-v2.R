# Para usar edgeR es preferible utilizar los conteos puros, sin embargo podemos pasarle conteos RSEM,
# pero sin normalizar, aunque siempre serán mejor los puros.

rm(list = ls())
pacman::p_load("edgeR", "SummarizedExperiment")

setwd("/home/antonio/Escritorio/Antonio/Bioinformatica/Cordoba/Oscar/paraAntonio/riñon_carcinoma")
data <- read.csv("kirc_gene_read_paired-normal-tumor.txt", sep = "\t")
head(data)

a <- colnames(data)
condicion <- factor(substr(a, nchar(a), nchar(a)))

dge = DGEList(data, group = condicion)

# Filtro para aquellos genes que tengan al menos cpm >= 1 en la mitad de las muestras (72 por condicion/2 = 36) 
# CPM is basically depth-normalized counts
# There is no purpose in analysing genes that are not expressed in either experimental condition.  We
# consider a gene to be expressed at a reasonable level in a sample if it has at least two counts for
# each million mapped reads in that sample
keep <- rowSums(cpm(dge)>1) >= 36 

dge <- dge[keep,]
dge$samples$lib.size <- colSums(dge$counts) # Recalculo el tamaño de libreria

# Otra forma de recalcular el tamaño de librería es poner dge <- dge[keep, keep.lib.sizes = FALSE] en la línea 21 y omitir el código de la 22

dge.n <- calcNormFactors(dge, method = "TMM")
dge.c <- estimateCommonDisp(dge.n)
dge.t <- estimateTagwiseDisp(dge.c)
et.c <- exactTest(dge.c)
et.t <- exactTest(dge.t)

topTags(et.c)
topTags(et.t)

# Determinamos los genes significativos

de.c <- decideTestsDGE(et.c, adjust.method = "BH", p.value = .05)
de.t <- decideTestsDGE(et.t, adjust.method = "BH", p.value = .05)

summary(de.t) # Resumen de los genes
summary(de.c)

# Genes up y down para cada clase
# Dispersión común

# Upregulados

upre.c <- which(de.c@.Data == 1)
upre.c <- et.c[upre.c,]
upre.c <- rownames(topTags(upre.c, n = 50))

# Downregulados

downre.c = which(de.c@.Data == -1)
downre.c = et.c[downre.c,]
downre.c = rownames(topTags(downre.c, n = 50))

# Cada gen tiene su dispersión

# Upregulados

upre.t = which(de.t@.Data == 1)
upre.t = et.t[upre.t,]
upre.t = rownames(topTags(upre.t, n = 50)) 

# Downregulados

downre.t = which(de.t@.Data == -1)
downre.t = et.t[downre.t,]
downre.t = rownames(topTags(downre.t, n = 50)) 

# Hasta aquí hemos obtenido los nombres de los distintos genes up y down
# por los métodos de dispersión común y entre genes.
# Agrupamos los genes (Grupos de genes con expresión diferencial significativa)

genes.c = c(upre.c, downre.c) # Dispersión común
genes.t = c(upre.t, downre.t) # Dispersión por gen, vamos a usar estos genes para Emerging Patterns.
genes.t

save(genes.t, file = "Top100genessig1.rda") # Guardamos los genes significativos 