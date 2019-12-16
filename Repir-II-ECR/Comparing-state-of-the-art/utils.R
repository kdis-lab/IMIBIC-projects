# Funcion que transforma el dataset. Se anade una nueva columna del siguiente filtrado glomerular
transform_dataset <- function(dataset){
  
  new.dataset <- data.frame(matrix(ncol = ncol(dataset) + 1, nrow = 0))
  
  colnames(new.dataset) <- c(colnames(dataset), "next_Filtrado_glomerular")
  
  ids <- unique(dataset$Id)
  
  for(id in ids){
    
    # Extract subset associated for id
    rows <- dataset[dataset$Id==id,]
    
    for(row_index in 1:(nrow(rows)-1)){
      
      new.row <-c(rows[row_index,], rows[row_index+1, "Filtrado_glomerular"]) # se adiciona el siguiente filtrado
      names(new.row) <- colnames(new.dataset)
      
      new.dataset <- rbind(new.dataset, new.row)  
      
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