#load the library
library(scmamp)
rm(list = ls())

# set whether the measure is maximal or minimal
maximize <- T;

# set the significance level
alpha <- 0.05;

#load the csv file
rd <- read.csv("/home/ogreyesp/Desktop/pair.csv", sep = ",")

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

#Wilcoxon test. Comparison between two algorithms
wilcoxonSignedTest(x=round(rdm$InceptionV4_w, digits = 3), y= round(rdm$InceptionV4_n, digits = 3))

#Friedman test. Multiple comparison
friedman <- friedmanTest(data=rdm, alpha=alpha)
friedman

#Friedman rankings
average.ranking <- colMeans(rankMatrix(rdm, decreasing = maximize))
average.ranking

#Imandavenport test.
#imanDavenportTest(data = rdm)

par(mgp=c(0,0,0), tcl=-0.4, mar=c(0,0,0,0))
#plot.window(c(0,1),c(0,1), xaxs = "i", yaxs = "i")

#If there are sig. differences
if(friedman$p.value < alpha) {
  #############Post-hoc tests#################################
  
  #post-Hoc test
  res <- postHocTest(data = rdm, test = 'friedman', correct = 'shaffer', use.rank=TRUE)
  
  #table N vs N
  bold <- res$corrected.pval < alpha
  
  bold[is.na(bold)] <- FALSE
  
  writeTabular(table = res$corrected.pval, format = 'f', bold = bold, hrule = 0, vrule = 0)
  
  #drawAlgorithmGraph(pvalue.matrix = res$corrected.pval, mean.value = average.ranking, highlight = "min", node.width = 6)
  
  # plot
  plotRanking(pvalues = res$corrected.pval, summary=average.ranking, decreasing = FALSE, alpha=alpha)
}