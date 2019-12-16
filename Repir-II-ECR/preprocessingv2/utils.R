library(lubridate)

# For a given continuous variable, outliers are those observations that lie 
# outside 1.5 *IQR, where IQR, the 'Inter Quartile Range' is the difference between
# 75th and 25th quartiles. The outliers are removed with the median value of the group that they belong.

remove_outliers <- function(varT, mydata, coef=1.5){
  
  vAll <- mydata[,varT]
  
  quantilesG <- quantile(vAll, probs=c(0.25,0.75), na.rm = TRUE)
    
  IQRG <- quantilesG[2]-quantilesG[1]
    
  indexes <- (vAll < (quantilesG[1]-coef*IQRG) | vAll > (quantilesG[2]+coef*IQRG))
    
  indexes[is.na(indexes)] <- FALSE
    
  print(paste("Ouliers of var ", varT, ": ", sum(indexes), sep = ""))
  
  print(vAll[indexes])
    
  #Assigned the outliers to NA
  mydata[indexes, varT] <- NA
  
  return (mydata)
}

#' Get age
#' 
#' Returns age, decimal or not, from single value or vector of strings
#' or dates, compared to a reference date defaulting to now. Note that
#' default is NOT the rounded value of decimal age.
#' @param from_date vector or single value of dates or characters
#' @param to_date date when age is to be computed
#' @param dec return decimal age or not
#' @examples
#' get_age("2000-01-01")
#' get_age(lubridate::as_date("2000-01-01"))
#' get_age("2000-01-01","2015-06-15")
#' get_age("2000-01-01",dec = TRUE)
#' get_age(c("2000-01-01","2003-04-12"))
#' get_age(c("2000-01-01","2003-04-12"),dec = TRUE)
get_age <- function(from_date, to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  age
}

mode <- function(x, na.rm= T){
  ta = table(x)
  tam = max(ta)
  
  if(is.numeric(x))
    mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  
  return(mod)
}

# This imputation methods for longitudinal dara assume that the missing data can be estimated from data that are available for each individual.
##################################################
##################################################
##################################################

# It assigns the mean/median/mode value taking into consideration the previous values before the missing value
# data: the dataset
# var: the name of the colum
# By default mean=T, the function acts as previous row mean method. If mean=F, then the function performs the method previous row median
# By default longitudinal = F, the function performs a global imputation
average_previous_values <- function(data, var, meanFunction = T, longitudinal = F){
  
  num <- sum(is.na(data[, var]))
  
  print(paste0("Number of missing values before imputation: ", num))
  
  if(num == 0) # Nothing to do
  { print("Nothing to do")
    return(NULL)}
  
  func <- mean
  
  if(!meanFunction){
    func <- median
  }
  
  if(is.factor(data[,  var])){
    func <- mode
  }
  
  if(!longitudinal){
    data[is.na(data[, var]), var] <- func(data[, var], na.rm = T)
  }
  else{
  
    uniqueId <- unique(data[is.na(data[, var]), "Id"])
    
    # Es posible que para un mismo paciente haya varios valores perdidos
    for(id in uniqueId){
      
      subdata <- data[data$Id == id, var]
      
      # Extreme case. All patient's values are NA.
      if(all(is.na(subdata))){
        
        data[data$Id == id, var] <- func(data[, var], na.rm= T)
        next #Jump to next patient
      }
      
      knownIndexes <- which(!is.na(subdata))
      
      for(i in which(is.na(subdata))){
        
        population <- subdata[1:i]
        
        # Si el primer valor es missing, entonces ...
        if(i==1){
          # The next observation carried backward (NOCB) assigns the next known score after the “missing” one.
          subdata[1] <- subdata[knownIndexes[1]]
          next
        }
        
        valueT <- func(population, na.rm= T)
        
        if(length(valueT) > 1){
          
          # En el caso de que valueT tenga mas de un valor, esto puede ocurrir solamente con la moda
          # se va asignar la ultima moda conocida antes del missing
          for(v in rev(population)){
            if(v %in% valueT){
              valueT <- v
              break()
            }
          }
        }
          
        # Se calcula la media o moda de valores en los registros previos al missing value
        subdata[i] <- valueT
      }
      
      # update the dataset
      data[data$Id == id, var] <- subdata
    }
  }
  
  suma <- sum(is.na(data[, var]))
  print(paste0("Number of missing values after imputation: ", suma ))
  
  if(suma !=0)
  { 
    stop("The imputation was not completed.")
  }
  
  return(data)
}

# It assignas the last observation carried forward (locf), i.e. assigns the previous known score to the “missing value.”
# data: the dataset
# var: the name of the column
# mean: whether the mean is used for the extreme cases or not
# By default longitudinal = F, the function performs a global imputation
locf <- function(data, var, meanFunction = T, longitudinal = F){
  
  num <- sum(is.na(data[, var]))
  
  print(paste0("Number of missing values before imputation: ", num))
  
  if(num == 0) # Nothing to do
  { print("Nothing to do")
    return(NULL)}
  
  func <- mean
  
  if(!meanFunction){
    func <- median
  }
  
  if(is.factor(data[,  var])){
    func <- mode
  }
  
  if(!longitudinal){
    data[is.na(data[, var]), var] <- func(data[, var], na.rm = T)
  }
  else{
    uniqueId <- unique(data[is.na(data[, var]), "Id"])
    
    # Es posible que para un mismo paciente haya varios valores perdidos
    for(id in uniqueId){
      
      subdata <- data[data$Id == id, var]
      
      # Extreme case. All patient's values are NA.
      if(all(is.na(subdata))){
        
        data[data$Id == id, var] <- func(data[, var], na.rm= T)
        next #Jump to next patient
      }
      
      for(i in which(is.na(subdata))){
        
        knownIndexes <- which(!is.na(subdata))
        
        # Si la primer visita es missing, entonces ...
        if(i==1){
          # The next observation carried backward (NOCB) assigns the next known score after the “missing” one.
          subdata[1] <- subdata[knownIndexes[1]]
          next
        }
        
        lowerIndexes <- which(i > knownIndexes)
        subdata[i] <- subdata[knownIndexes[lowerIndexes[length(lowerIndexes)]]] # take the last observation before the missing
      }
      
      # update the dataset
      data[data$Id == id, var] <- subdata
    }
  }
  
  suma <- sum(is.na(data[, var]))
  print(paste0("Number of missing values after imputation: ", suma ))
  
  if(suma !=0)
  { 
    stop("The imputation was not completed.")
  }
  
  return(data)
}

# It assigns the median/mean/mode value of all patient's values to the missing value
# data: the dataset
# var: the name of the colum
# By default mean=T, the function acts as previous row mean method. If mean=F, then the function performs the method previous row median
# By default longitudinal = F, the function performs a global imputation
average_before_after_values <- function(data, var, meanFunction = T, longitudinal = F){
  
  num <- sum(is.na(data[, var]))
  
  print(paste0("Number of missing values before imputation: ", num))
  
  if(num == 0) # Nothing to do
  { print("Nothing to do")
    return(NULL)}
  
  func <- mean
  
  if(!meanFunction){
    func <- median
  }
  
  if(is.factor(data[,  var])){
    func <- mode
  }
  
  if(!longitudinal){
    data[is.na(data[, var]), var] <- func(data[, var], na.rm = T)
  }
  else{
    uniqueId <- unique(data[is.na(data[, var]), "Id"])
    
    # Es posible que para un mismo paciente haya varios valores perdidos
    for(id in uniqueId){
      
      population <- subdata <- data[data$Id == id, var]
      
      # Extreme case. All patient's values are NA.
      if(all(is.na(subdata))){
        population <- data[, var]
      }
      
      valueT <- func(population, na.rm= T)
      
      if(length(valueT) > 1){
      
        knownIndexes <- which(population %in% valueT)
        
        # En el caso de que valueT tenga mas de un valor, esto puede ocurrir solamente con la moda
        # se va asignar la ultima moda conocida antes del missing
        for(index in which(is.na(population))){
          
          lowerIndexes <- which(index > knownIndexes)
          higherIndexes <- which(index < knownIndexes)
          
          # By default, the last mode value before the missing data is assigned
          if(length(lowerIndexes) > 0)
            subdata[index]  <- subdata[knownIndexes[lowerIndexes[length(lowerIndexes)]]]
          else
            subdata[index]  <- subdata[knownIndexes[higherIndexes[1]]]
        }
        
      }
      else{
        subdata[is.na(subdata)] <- valueT
      }
      
      data[data$Id == id, var] <- subdata
    }
  }
  
  suma <- sum(is.na(data[, var]))
  print(paste0("Number of missing values after imputation: ", suma ))
  
  if(suma !=0)
  { 
    stop("The imputation was not completed.")
  }
  
  return(data)
}

# It assignas the next observation carried backward (NOCB) to the missing value.
# data: the dataset
# var: the name of the colum
# mean: whether the mean is used for the extreme cases or not
# By default longitudinal = F, the function performs a global imputation
nocb <- function(data, var, meanFunction = T, longitudinal = F){
  
  num <- sum(is.na(data[, var]))
  
  print(paste0("Number of missing values before imputation: ", num))
  
  if(num == 0) # Nothing to do
  { print("Nothing to do")
    return(NULL)}
  
  func <- mean
  
  if(!meanFunction){
    func <- median
  }
  
  if(is.factor(data[,  var])){
    func <- mode
  }
  
  if(!longitudinal){
    data[is.na(data[, var]), var] <- func(data[, var], na.rm = T)
  }
  else{
  
    uniqueId <- unique(data[is.na(data[, var]), "Id"])
    
    # Es posible que para un mismo paciente haya varios valores perdidos
    for(id in uniqueId){
      
      subdata <- data[data$Id == id, var]
      
      # Extreme case. All patient's values are NA.
      if(all(is.na(subdata))){
        
        data[data$Id == id, var] <- func(data[, var], na.rm= T)
        
        next #Jump to next patient
      }
      
      knownIndexes <- which(!is.na(subdata))
      
      for(i in which(is.na(subdata))){
        
        higherIndexes <- which(i < knownIndexes)
        
        # May be the rest of next elements are missing, and therefore, the method would fail. In that case, the method assigns the last known value
        if(length(higherIndexes)==0){
          lowerIndexes <- which(i > knownIndexes)
          subdata[i] <- subdata[knownIndexes[lowerIndexes[length(lowerIndexes)]]]
          next
        }
        
        subdata[i] <-  subdata[knownIndexes[higherIndexes[1]]] # Take the last known value after the missing
      }
      
      # update the dataset
      data[data$Id == id, var] <- subdata
    }
  }
  
  suma <- sum(is.na(data[, var]))
  print(paste0("Number of missing values after imputation: ", suma ))
  
  if(suma !=0)
  { 
    stop("The imputation was not completed.")
  }
  
  return(data)
}

# It assigns the median/mean/mode value of the last and next known observations to the missing value
# data: the dataset
# var: the name of the colum
# By default mean=T. In the case of mean=F, then the function performs the median
# By default longitudinal = F, the function performs a global imputation
average_last_next_values <- function(data, var, meanFunction = T, longitudinal = F){
  
  num <- sum(is.na(data[, var]))
  
  print(paste0("Number of missing values before imputation: ", num))
  
  if(num == 0) # Nothing to do
  { print("Nothing to do")
    return(NULL)}
  
  func <- mean
  
  if(!meanFunction){
    func <- median
  }
  
  if(is.factor(data[,  var])){
    func <- mode
  }
  
  if(!longitudinal){
    data[is.na(data[, var]), var] <- func(data[, var], na.rm = T)
  }
  else{
    
    uniqueId <- unique(data[is.na(data[, var]), "Id"])
    
    # Es posible que para un mismo paciente haya varios valores perdidos
    for(id in uniqueId){
      
      subdata <- data[data$Id == id, var]
      
      knownIndixes <- which(!is.na(subdata))
      
      # Extreme case. All patient's values are NA.
      if(length(knownIndixes)==0){
        
        data[data$Id == id, var]  <- func(data[, var], na.rm= T)
        next #Jump to next patient
      }
      
      for(i in which(is.na(subdata))){
        
        lowerIndixes <-  which(i > knownIndixes)
        higherIndixes <- which(i < knownIndixes)
        
        indexes <- c()
        
        if(length(lowerIndixes) > 0){
          lastValueIndex <- knownIndixes[lowerIndixes[length(lowerIndixes)]]
          indexes <- c(indexes, lastValueIndex)
        }
        
        if(length(higherIndixes) > 0){
          nextValueIndex <- knownIndixes[higherIndixes[1]]
          indexes <- c(indexes, nextValueIndex)
        }
        
        valueT <- func(subdata[indexes], na.rm=T)
        
        if(length(valueT) > 1){
        
          # Esto es un caso extremo que puede ocurrir cuando se usa la moda.
          # Si ocurre esto siempre se intentara de asignar el ultimo valor, sino se puede el siguiente valor conocido
          
          valueT <- subdata[indexes[1]]
        }
        
        subdata[i] <- valueT
      }
      
      # Update the dataset
      data[data$Id == id, var] <- subdata
    }
  }
  
  suma <- sum(is.na(data[, var]))
  print(paste0("Number of missing values after imputation: ", suma ))
  
  if(suma !=0)
  { 
    stop("The imputation was not completed.")
  }
  
  return(data)
}