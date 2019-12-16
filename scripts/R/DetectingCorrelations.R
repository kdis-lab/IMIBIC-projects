library(car)
library(caret)
library(polycor)
library(vcd)
library(vcdExtra)
library(corrgram)
library(rms)
library(lsr)

#For more details of measures of associations consult 
#http://dwoll.de/rexrepos/posts/associationOrder.html
#https://www.andrews.edu/~calkins/math/edrm611/edrm13.htm

significanceLevel <- 0.10
plotCorre <- TRUE

setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/v4/")

mydata <- read.csv("factores-clinicos-withoutOutliersv2.csv", sep = ",", na.strings = "")

output <- "results/Correlations/"

newData <- data.frame("Type" = character(0),
  "Comparison" = character(0),
  "Correlation" = numeric(0), "p-value" = numeric(0))

#Computes a heterogenous correlation matrix, consisting of Pearson product-moment correlations 
#between numeric variables, polyserial correlations between numeric and ordinal variables, and 
#polychoric correlations between ordinal variables.
#results <-hetcor(mydata)

# very important
#https://statistics.laerd.com/statistical-guides/pearson-correlation-coefficient-statistical-guide.php

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

      if(plotCorre && register){
          
           formulaT <- as.formula(paste(firstCol," ~ ", secondCol, sep =""))
              
           scatterplot(formulaT, data=mydata,
                        xlab=secondCol, ylab=firstCol)
              
           dev.copy(png, paste(output, firstCol,"vs",secondCol,".png",sep = ""), width=8, height=5, units="in", res=150)
           dev.off()
      }
    }
    
    if(class(mydata[,firstCol])=="factor" && class(mydata[,secondCol])=="factor"){
      
      type <- "Factorvs.Factor"
      
      # With a large sample size, even a small degree of association can show a significant Chi squared.
      # Instead, there are a variety of statistical measures of strength of association for contingency tables- similar
      # in spirit to r or r-squared for continuous variables.
      
      #https://www.spss-tutorials.com/cramers-v-what-and-why/
      
      #If both variables are dichotomous (resulting in a 2 by 2 table) use a phi coefficient, 
      #which is simply a Pearson correlation computed on dichotomous variables. In this case, phi coefficient and Crammer match.
      
      #If either of the nominal vars has more than two categories, the Crame's V is preffered
      
      # Cramer's V is the most popular of the chi-square-based measures of nominal association 
      #because it gives good norming from 0 to 1 regardless of table size
      
      #It generally has a maximum value of 1 when there is a very strong relationship
      #between two variables
      
      #In practice, you may find that a Cramer's V of .10 provides a good minimum threshold for suggesting there is a substantive relationship between two variables.
      
      test <- assocstats(table(mydata[,firstCol],mydata[,secondCol]))
      
      test <- summary(test)
      
      corre <- test$object$cramer
      p.value <- test$summary$p.value
      
      if(abs(corre) > 0.1 && p.value <= significanceLevel){
        register <- TRUE
      }
      
      if(plotCorre && register){
          # Mosaic plots provide an ideal method both for visualizing contingency tables and for visualizing the
          # fit- or more importantly- lack of fit of a loglinear model.
          # For a two-way table, mosaic() fits a model of independence
          #For n-way tables, mosaic() can fit any loglinear model
          
          #The colors represent the level of the residual for that cell / combination of levels. 
          #The legend is presented at the plot's right. More specifically, blue means there are 
          #more observations in that cell than would be expected under the null model 
          #(independence). Red means there are fewer observations than would have been 
          #expected. You can read this as showing you which cells are contributing to the 
          #significance of the chi-squared test result.
          
          #Si los residuos no son largos, yet the
          #association between the two variables is highly significant.
          
          formulaT <- as.formula(paste("~", firstCol,"+", secondCol,sep = ""))
          
          mosaic(formulaT, data= mydata, gp = shading_Friendly, split_vertical = TRUE, legend =TRUE, 
                 labeling=labeling_border(rot_labels = c(90, 0, 90, 0), 
            just_labels=c("left","left","right","right"),
            tl_varnames = FALSE,
            gp_labels = gpar(fontsize = 9)))
          
          dev.copy(png,paste(output, firstCol,"vs",secondCol,".png",sep = ""), width=10, height=10, units="in", res=150)
          dev.off()
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
          
          dichotomousVar[mydata[,theFactor]==levelsVar[2]] <- 1
          
          test <- cor.test(mydata[,theNumeric], dichotomousVar)
          
          corre <- test$estimate
          p.value<-test$p.value
          
          if(abs(corre) > 0.1 && p.value <= significanceLevel){
            register <- TRUE
          }
        
        } else {
        
          #If your categorical variable has more than two measures, eta correlation is suggested.    
          formulaT <- as.formula(paste(theNumeric,"~",theFactor,sep = ""))
          
          #Eta squared is a measured of effect size, it is analougous to r-squared. It represents the proportion of variance in 
          # Y explained by X. It can detect non-linear correlations
          
          # It ranges between 0..1. A rule of thumb (Cohen):
          # 0.02 -small
          # 0.13 -medium
          # 0.26 -large
          
          #One way anova
          anovaTest <- aov( formula = formulaT, data= mydata)
          
          corre <- etaSquared(anovaTest)[1][1]
          #p-value of the anova test
          p.value <- unlist(summary(anova1))[["Pr(>F)1"]]
          
          if(abs(corre) > 0.02 && p.value <= significanceLevel){
            register <- TRUE
          }
        }
        
        if(plotCorre && register){
          formulaT <- as.formula(paste(theFactor,"~",theNumeric,sep = ""))
          
          cdplot(formulaT, data = mydata)
          
          dev.copy(png,paste(output, firstCol,"vs",secondCol,".png",sep = ""), width=8, height=8, units="in", res=150)
          dev.off()
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

if(plotCorre){
  #Displaying a correlogram for the numeric variables
  corrgram(mydata, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie)
  
  dev.copy(png,paste(output, "Correlogram between numeric vars.png",sep = ""), width=8, height=8, units="in", res=200)
  dev.off()
}

# Sort by Type and statistic
newData <- newData[order(newData$Type, -newData$Correlation, newData$p.value), ]

write.table(newData, file = paste(output,"Correlations.csv",sep = ""),
  quote = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)