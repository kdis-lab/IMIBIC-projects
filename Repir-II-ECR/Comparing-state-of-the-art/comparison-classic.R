library(caret)

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

#packages to install: elasticnet, e1071, mboost, randomForest, xgboost, RSNNS, kernlab

# SE TRATA DE PREDECIR EL SIGUIENTE VALOR DE FG. POR ESO ANTES DE TODO EL DATASET HA DE TRANSFORMARSE PARA QUE 
# POR CADA REGISTRO SE TENGA EL SIGUIENTE VALOR DE FG Y NO EL ALTUAL, YA QUE EL ACTUAL SE PUEDE CALCULAR DIRECTAMENTE A PARTIR DE LAS COVARIABLES

# Se siguio el proceso de CV explicado aqui, llamado forward chaining
#https://towardsdatascience.com/time-series-nested-cross-validation-76adba623eb9

# YA EN ESTE DATASET NO HAY NINGUN PACIENTE CON 1 SOLA VISITA

# LASSO, Bagged CART, Boosted Generalized Linear Model, Random Forest, eXtreme Gradient Boosting, k-Nearest Neighbors, Multi-Layer Perceptron, Support Vector Machines with Linear Kernel
traversals <- c("lasso", "treebag", "glmboost", "rf", "xgbLinear", "knn", "mlp", "svmLinear")

results <- list(lasso=c(), treebag = c(), glmboost = c(), rf= c(), xgbLinear= c(), knn= c(), mlp= c(), svmLinear= c())

# YA EN ESTE DATASET NO HAY NINGUN PACIENTE CON 1 SOLA VISITA
dataset <- read.csv("datasets/datos-demograficos-visitas-tratamientos-missing-imputation-mean-average_before_after_values-allnumerics.csv", header = T, na.strings = "")

nmax <- max(table(dataset$Id))

original_numeric <- read.csv("datasets/original_columns_numeric.csv", header = T, na.strings = "")

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
  
    dta_train$Id <- NULL
    dta_test$Id <- NULL
    
    form <- as.formula(paste0("fg", timestep, " ~ ."))
    
    for(alg in traversals){
      
      # The id att is removed
      modelFit <- train(form, data = dta_train[,],
                      method= alg,
                      maximize = FALSE)
    
      p <- predict(modelFit, newdata = dta_test[,-ncol(dta_test)])
    
      results[[alg]] <- c(results[[alg]], RMSE(p, dta_test[, ncol(dta_test)]))
     }
  }
}

#for(alg in traversals){
 
#  print(alg)
#  print(paste0("Mean: ", mean(results[[alg]])))
#  print(paste0("Std: ", sd(results[[alg]])))
  
#  print("***********************") 
  
#}

print("RESULTS")
print(results)





