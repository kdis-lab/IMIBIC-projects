library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(Factoshiny)
library(missMDA)

dataset <- read.csv("parafinas-withoutMissingValues-preproc.csv", na.strings = "", row.names = 1)

#PCA with supplementary variables
res <- PCA(dataset,
           scale.unit = FALSE,
           quali.sup = c(ncol(dataset)),
           graph = FALSE)

Investigate(res, document = "word_document", file = "Investigate-LinfoAnt.Rmd")

summary(res, nbelements = Inf)

#Decription of the dimensios
dimdesc(res)

#screeplot
fviz_screeplot(res, addlabels = TRUE)

# Graph of variables
fviz_pca_var(res, geom = c("point", "text"), repel = TRUE, col.var = "contrib", labelsize = 4)

# Contributions of variables to PC1
fviz_contrib(res, choice = "var", axes = 2, top = 10)

# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
fviz_pca_ind(res, col.ind = "contrib",
             repel = TRUE # Avoid text overlapping (slow if many points)
             )

# Biplot of individuals and variables
fviz_pca_biplot(res, repel = TRUE)

plot(res, cex=0.5, habillage= "Diabetes", invisible = "quali", axes=c(1,2))
plot(res, cex=0.5, choix="var", title="Variable PCA graph", axes=c(1,2), autoLab = "yes")

plotellipses(res, cex = 0.5, keepvar=5)
