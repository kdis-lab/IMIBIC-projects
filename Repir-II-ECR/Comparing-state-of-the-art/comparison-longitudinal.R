library(boostmtree)
library(htree)
library(REEMtree)
library(lme4)
library(caret)

#source("utils.R")

# Funcion que transforma el dataset. Se anade timestep columnas del siguiente filtrado glomerular
transform_dataset <- function(dataset, timestep=1){
  
  new.dataset <- data.frame(matrix(ncol = ncol(dataset) + timestep, nrow = 0))
  
  colnames(new.dataset) <- c(colnames(dataset), paste0("fg", c(1:timestep)))
  
  ids <- unique(dataset$Id)
  
  for(id in ids){
    
    # Extract subset associated for id
    rows <- dataset[dataset$Id==id,]
    
    if((nrow(rows)-timestep) >=1){
    
      for(row_index in 1:(nrow(rows)-timestep)){
        
        begin_slice <- row_index + 1
        end_slice <- begin_slice + timestep - 1
          
        new.row <-c(rows[row_index,], rows[begin_slice: end_slice , "Filtrado_glomerular"])
        
        names(new.row) <- colnames(new.dataset)
        
        new.dataset <- rbind(new.dataset, new.row)  
        
      }
    }
  }
  
  return(new.dataset)
}

split_train_test <- function(dataset, nvisits){
  
  train <- data.frame(matrix(ncol = ncol(dataset), nrow = 0))
  test <- data.frame(matrix(ncol = ncol(dataset), nrow = 0))
  
  colnames(train) <- colnames(dataset)
  colnames(test) <- colnames(dataset)
  
  ids <- unique(dataset$Id)
  
  # For each patient
  for(id in ids){
    
    # Extract subset associated for id
    
    rows <- dataset[dataset$Id==id,]
    
    nvisitsT <- nvisits
    
    if (nrow(rows) < nvisits){
      
      nvisitsT <- nrow(rows)
    }
    
    rows_training <- rows[1: nvisitsT,]
    train <- rbind(train, rows_training)
    
    if(nvisitsT < nrow(rows)){
      
      # Select the next visit
      rows_test <- rows[nvisitsT + 1, ]
      test <- rbind(test, rows_test)
    }
  }
  
  return(list("train"= train, "test" = test))
}

# SE TRATA DE PREDECIR EL SIGUIENTE VALOR DE FG. POR ESO ANTES DE TODO EL DATASET HA DE TRANSFORMARSE PARA QUE 
# POR CADA REGISTRO SE TENGA EL SIGUIENTE VALOR DE FG Y NO EL ALTUAL, YA QUE EL ACTUAL SE PUEDE CALCULAR DIRECTAMENTE A PARTIR DE LAS COVARIABLES

# Se siguio el proceso de CV explicado aqui, llamado forward chaining
#https://towardsdatascience.com/time-series-nested-cross-validation-76adba623eb9

# YA EN ESTE DATASET NO HAY NINGUN PACIENTE CON 1 SOLA VISITA
dataset <- read.csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-mean-average_before_after_values-allnumerics.csv", header = T, na.strings = "")

nmax <- max(table(dataset$Id))

original_numeric <- read.csv("datasets/original_columns_numeric.csv", header = T, na.strings = "")

results = list(BOOSTMTREE=c(), HTREE = c(), RFOREST= c(), REEMTREE= c(), LME= c())

for(timestep in 1: (nmax-2)){
  
  datasetT <- transform_dataset(dataset, timestep = timestep)
  original_numericT <- c(as.character(original_numeric$Column), paste0("fg", c(1:timestep)))
  
  nmaxT <- max(table(datasetT$Id))
  
  for(nvisit in 1:(nmaxT-1)){
    
    data.split <- split_train_test(datasetT, nvisit)
    
    dta_train <-data.split[["train"]]
    dta_test <-data.split[["test"]]
    
    preProcValues <- preProcess(dta_train[,original_numericT], method = c("center", "scale"))
    dta_train[,original_numericT] <- predict(preProcValues, dta_train[,original_numericT])
    dta_test[,original_numericT] <- predict(preProcValues, dta_test[,original_numericT])
    
    if(nvisit > 1){
      
      # LME
      
      form <- as.formula(paste0(paste0("fg", timestep, " ~ "), 
                                paste(colnames(dta_train)[c(2:ncol(dta_train)-1)], collapse = " + "), " + (1|Id)"))
      
      modelp <- lmer(form, data = dta_train)
      
      p <- predict(modelp, newdata = dta_test[, 1:(ncol(dta_train)-1)], allow.new.levels = TRUE)
      
      results$LME <- c(results$LME , RMSE(p, dta_test[,ncol(dta_test)]))
    }
    
    # REEMTREE
    form <- as.formula(paste0(paste0("fg", timestep, " ~ "), paste(colnames(dta_train)[c(2:51, 53:(ncol(dta_train)-1))], collapse = " + ")))
    
    modelp <- REEMtree(form, data= dta_train, random= ~ 1 + Tiempo_evolucion|Id)
    
    p <- predict(modelp, dta_test, EstimateRandomEffects = FALSE, id = "Id")
    
    results$REEMTREE <- c(results$REEMTREE , RMSE(p, dta_test[, ncol(dta_test)]))
    
    dta_train.features <- dta_train[, c(2:51, 53:(ncol(dta_train)-1))]
    dta_train.time <- dta_train$Tiempo_evolucion
    dta_train.id <- dta_train$Id
    dta_train.y <- dta_train[, ncol(dta_train)]
    
    dta_test.features <- dta_test[, c(2:51, 53:(ncol(dta_test)-1))]
    dta_test.time <- dta_test$Tiempo_evolucion
    dta_test.id <- dta_test$Id
    dta_test.y <- dta_test[,ncol(dta_test)]
    
    # BOOSTMTREE
    modelp <- boostmtree(x= dta_train.features, tm = dta_train.time, id = dta_train.id, y = dta_train.y, M = 20)
    
    p <-predict(modelp, x = dta_test.features, id= dta_test.id, tm= dta_test.time, y= dta_test.y)
    
    p <-unlist(p$mu)
    
    results$BOOSTMTREE <- c(results$BOOSTMTREE, RMSE(p,dta_test.y))
    
    # HTREE
    dta_train.features <- dta_train[, c(2:51, 53:ncol(dta_train))]
    modelp <- htb(x=dta_train.features, id= dta_train.id, time= dta_train.time, yindx= paste0("fg", timestep), lambda=.1, nsplit=3, ntrees=100)
    
    dta_test.features <- dta_test[, c(2:51, 53:ncol(dta_test))]
    p <- predict_htb(modelp, x= dta_test.features, type = "response", time= dta_test.time, id = dta_test.id)
    
    results$HTREE <- c(results$HTREE, RMSE(p, dta_test.y))
    
    # RFOREST
    modelp <- hrf(x=dta_train.features, id= dta_train.id, time= dta_train.time, yindx= paste0("fg", timestep))
    
    p <- predict_hrf(modelp, x = dta_test.features, time= dta_test.time, id = dta_test.id)
    
    results$RFOREST <- c(results$RFOREST , RMSE(p, dta_test.y))
  }
}

print("Results")
print(results)

#print(paste0("Mean: ", mean(results$BOOSTMTREE)))
#print(paste0("Std: ", sd(results$BOOSTMTREE)))

#print("***********************")

#print("HTREE")
#print(paste0("Mean: ", mean(results$HTREE)))
#print(paste0("Std: ", sd(results$HTREE)))

#print("***********************")

#print("RFOREST")
#print(paste0("Mean: ", mean(results$RFOREST)))
#print(paste0("Std: ", sd(results$RFOREST)))

#print("***********************")

#print("REEMTREE")
#print(paste0("Mean: ", mean(results$REEMTREE)))
#print(paste0("Std: ", sd(results$REEMTREE)))

#print("***********************")

#print("LME")
#print(paste0("Mean: ", mean(results$LME)))
#print(paste0("Std: ", sd(results$LME)))