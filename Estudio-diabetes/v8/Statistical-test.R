#load the library
library(scmamp)

# set whether the measure is maximal or minimal
maximize <- TRUE;

# set the significance level
alpha <- 0.05;

#load the csv file
rd <- read.csv("cv-results.csv", header = TRUE, sep = ";")

nAlgorithms <- ncol(rd)-1
nDatasets <- nrow(rd)

# upper case the first letter of dataset names
rd$Dataset <- paste(toupper(substring(rd$Dataset, 1, 1)), substring(rd$Dataset, 2), sep = "")

#sort the dataframe by dataset name
rd <- rd[order(rd$Dataset),]

rdm <- rd[, 2: (nAlgorithms+1)]

#create latex table
f <- if(maximize) max else min

boldT <- matrix(data=FALSE, nrow = nDatasets, ncol = (nAlgorithms+1))

for(i in 1:nDatasets){
  
  maxmin <- f(rdm[i,])
  
  boldT[i, 2:(nAlgorithms+1)] <- (rdm[i,] == maxmin)
  
}

boldT[is.na(boldT)] <- FALSE
writeTabular(table = rd, format = 'f', bold = boldT, hrule = 0, vrule = 0, align = 'c', print.row.names=FALSE)

#Friedman test. Multiple comparison
friedman <- friedmanTest(data=rdm,alpha=alpha)
friedman

#Friedman rankings
average.ranking <- colMeans(rankMatrix(rdm, decreasing = maximize))
average.ranking

#If there are sig. differences
if(friedman$p.value < alpha) {
  #############Post-hoc tests#################################
  
  #post-Hoc test
  res <- postHocTest(data = rdm, test = 'friedman', correct = 'bergmann', use.rank=TRUE)
  
  #table N vs N
  bold <- res$corrected.pval < alpha
  
  bold[is.na(bold)] <- FALSE
  
  writeTabular(table = res$corrected.pval, format = 'f', bold = bold, hrule = 0, vrule = 0) 
  
  setEPS()
  postscript("Bergmann-Hommel.eps", width = 6, height = 4)
  
  # plot
  plotRanking(pvalues = res$corrected.pval, summary=average.ranking, decreasing = FALSE, alpha=alpha)
  
  dev.off()
}