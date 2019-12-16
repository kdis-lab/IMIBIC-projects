  #https://www.r-bloggers.com/correlation-and-linear-regression/
  #looking at data relation is a sensible step to understand how your different variable interact together. 
  #Correlation look at trends shared between two variables, and regression look at causal relation between a 
  #predictor (independent variable) and a response (dependent) variable.
  
  #More information at http://www.statmethods.net/graphs/scatterplot.html
  library(car)
  
  # load the csv
  path <- "D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/v2/"
  
  #load the dataset
  dataset <-read.csv(paste(path,"ControlvsTumoral-withoutOutliers-preproc.csv", sep = ""), sep = ",")
  
  nuAtt <- ncol(dataset)-1
  
  colnames <- colnames(dataset)
  
  colClassName <- colnames[nuAtt+1]
  
  # Those pairs of variables that show a correlation statistically significantive are shown (at significance level of 0.05)
  
  for(i in 1:(nuAtt-1)){
    
    firstCol <- colnames[i]
    
    for(j in (i+1):nuAtt){
      
      SecondCol <- colnames[j]
      
      # Using parametric assumptions (Pearson, dividing the coefficient by its standard error, giving a value that follow
      # a t-distribution) or when data violate parametric assumptions using Spearman rank coefficient.
      
      corre <- cor(dataset[,firstCol],dataset[,SecondCol], method="spearman")
      
      #Only for higher correlations
      if(abs(corre)>=0.8 && !is.na(corre)){
        
        test <- cor.test(dataset[,firstCol],dataset[,SecondCol], method="spearman")
        
        if(test$p.value < 0.01){
          
          formula <- paste(firstCol," ~ ", SecondCol," | ", colClassName, sep ="")
          
          print(formula)
          print(test)
          print("///////////////////////")
          
          scatterplot(as.formula(formula), data=dataset,
                      xlab=SecondCol, ylab=firstCol)
          
          dev.copy(png,paste(path,"-",firstCol,"vs",SecondCol,".png",sep = ""),width=8,height=5,units="in",res=200)
          dev.off()
        }
      }
      
    }
  }
  