library(car)
library(caret)
library(polycor)
library(vcd)
library(vcdExtra)
library(corrgram)
library(rms)
library(lsr)

significanceLevel <- 0.10

setwd("/media/ogreyesp/DATA/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/ictus/")

mydata <- read.csv("Ictus-preproc-withoutOutliers.csv", sep = ",", na.strings = "")

predictors <- read.csv("results/predictors.csv", header = FALSE)

idx <- match(c(as.character(predictors[,1]), "Class"), colnames(mydata))

mydata <- mydata[, idx]

output <- "results/Correlations/"

newData <- data.frame("Type" = character(0),
  "Comparison" = character(0),
  "Correlation" = numeric(0), "p-value" = numeric(0))

# Test the existence of relathionships between variables
cNames <- colnames(mydata)
ncolumns <- ncol(mydata)

for(i in 1: (ncolumns-1)){
  
  firstCol <- cNames[i]
  
  for(j in (i+1):ncolumns) {
    
    secondCol <- cNames[j]
    
    register <- FALSE
   
    #scatter plots
    if((class(mydata[,firstCol])=="integer" || class(mydata[,firstCol])=="numeric") 
       && (class(mydata[,secondCol])=="integer" || class(mydata[,secondCol])=="numeric")){
      
      type <- "Numericvs.Numeric"
      
      # Using parametric assumptions (Pearson, dividing the coefficient by its standard error, giving a value that follow
      # a t-distribution) or when data violate parametric assumptions using Spearman rank coefficient.
      
      test <-cor.test(mydata[,firstCol], mydata[,secondCol])
      
      #the following guidelines have been proposed:
                                 #Coefficient, r
      #Strength of Association 	Positive 	Negative
      #Small                	.1 to .3 	-0.1 to -0.3
      #Medium 	              .3 to .5 	-0.3 to -0.5
      #Large 	                .5 to 1.0  -0.5 to -1.0

      #register the comparison
      corre <- test$estimate
      p.value <- test$p.value
      
      if(abs(corre) > 0.1 && p.value <= significanceLevel){
        register <- TRUE
      }
    }
    
    if(class(mydata[,firstCol])=="factor" && class(mydata[,secondCol])=="factor"){
      
      type <- "Factorvs.Factor"

      test <- assocstats(table(mydata[,firstCol],mydata[,secondCol]))
      
      test <- summary(test)
      
      corre <- test$object$cramer
      p.value <- test$summary$p.value
      
      if(abs(corre) > 0.1 && p.value <= significanceLevel){
        register <- TRUE
      }
    }
    
    #NumericvFactor
    if((class(mydata[,firstCol])=="factor" && class(mydata[,secondCol])=="integer") 
       || (class(mydata[,firstCol])=="integer" && class(mydata[,secondCol])=="factor")
       || (class(mydata[,firstCol])=="numeric" && class(mydata[,secondCol])=="factor")
       || (class(mydata[,firstCol])=="factor" && class(mydata[,secondCol])=="numeric"))
      {
      
        type <- "Numericvs.Factor"
      
        theFactor <- ifelse(class(mydata[,firstCol])=="factor", firstCol, secondCol)
        
        theNumeric <- ifelse(class(mydata[,firstCol])=="integer" || class(mydata[,firstCol])=="numeric", firstCol, secondCol)
        
        levelsVar <- levels(mydata[,theFactor])
        
        #The strength of correlation between a categorical variable (dichotomous) and an continuous variable can be computed using point biserial correlation.
        if(length(levelsVar)==2){
          
          dichotomousVar <- c(rep(0,nrow(mydata)))
          
          dichotomousVar[mydata[,theFactor] == levelsVar[2]] <- 1
          
          test <- cor.test(mydata[,theNumeric], dichotomousVar)
          
          corre <- test$estimate
          p.value<-test$p.value
          
          if(abs(corre) > 0.1 && p.value <= significanceLevel){
            register <- TRUE
          }
        
        } else {
        
          #If your categorical variable has more than two measures, eta correlation is suggested.    
          formulaT <- as.formula(paste(theNumeric,"~",theFactor,sep = ""))
          
          # It ranges between 0..1. A rule of thumb (Cohen):
          # 0.02 -small
          # 0.13 -medium
          # 0.26 -large
          
          #One way anova
          anovaTest <- aov(formula = formulaT, data= mydata)
          
          corre <- etaSquared(anovaTest)[1][1]
          #p-value of the anova test
          p.value <- unlist(summary(anova1))[["Pr(>F)1"]]
          
          if(abs(corre) > 0.02 && p.value <= significanceLevel){
            register <- TRUE
          }
        }
    }
    
    #register the correlation
    if(register){
      
      newData <-
        rbind(
          newData,
          data.frame(
            "Type" = type,
            "Comparison" = paste(firstCol,"-",secondCol,sep = ""),
            "Correlation" = corre, "p-value"= p.value))
    }
  }
}

# Sort by Type and statistic
newData <- newData[order(newData$Type, -newData$Correlation, newData$p.value), ]

write.table(newData, file = paste(output,"Correlations.csv",sep = ""),
  quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)
