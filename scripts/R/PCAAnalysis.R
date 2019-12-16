library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(Factoshiny)
library(missMDA)

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/enfermedades-autoinmunes/v3/")

dataset <- read.csv("Linfo-preproc.csv", na.strings = "")

hasMixedData <- TRUE
hasMissingData <- FALSE

# For handling missing data
################################

if(hasMissingData){
#missMDA is based in a iterative regularized PCA use to estimate missing data. It is a good alternative to other statistical 
#techniques for working with missing data

  if(!hasMixedData){
    # This function finds the optimal number of components (dimensions) to use when inputing missing data
    nb <- estim_ncpPCA(dataset[,1:20], ncp.min=0, ncp.max=5, method.cv="Kfold", nbsim=100, scale = TRUE)
    #impute the table
    comp <- imputePCA(dataset[,1:20], ncp = nb$ncp, scale = TRUE)
  } else{
    # This function finds the optimal number of components (dimensions) to use when inputing missing data
    nb <- estim_ncpFAMD(dataset, ncp.min=0, ncp.max=5, method.cv="Kfold", nbsim=100)  
    #impute the table
    comp <- imputeFAMD(dataset, ncp = nb$ncp)
  }
}

#PCA with supplementary variables
res <- PCA(#comp$completeObs,
           dataset,
           scale.unit = TRUE,
           quanti.sup = 35,
           quali.sup = c(36:44),
           graph = FALSE)

Investigate(res, document = "word_document", file = "Investigate-Mono.Rmd")

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
