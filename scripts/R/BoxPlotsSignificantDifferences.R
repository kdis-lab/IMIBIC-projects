library(ggplot2)
library(ggsignif)

# boxplot is a method for graphically depicting groups of numerical data 
# through their quartiles

# Box plots may also have lines extending vertically from the boxes (whiskers) 
# indicating variability outside the upper and lower quartiles

# Outliers may be plotted as individual points. Box plots are non-parametric: 
# they display variation in samples of a statistical population without making 
# any assumptions of the underlying statistical distribution

# The spacings between the different parts of the box indicate the degree of 
# dispersion (spread) and skewness in the data, and show outliers.

# The bottom and top of the box are always the first and third quartiles
# and the band inside the box is always the second quartile (the median)
# But the ends of the whiskers can represent several possible alternative values, 
#among them:

# - the minimum and maximum of all of the data
# - the lowest datum still within 1.5 IQR of the lower quartile, and the highest datum 
# still within 1.5 IQR of the upper quartile (often called the Tukey boxplot)
# IQR is equal to the difference between 75th and 25th percentiles. The IQR is the 1st
# quartile subtracted from the 3rd quartile; these quartiles can be clearly seen 
# on a box plot on the data. 
# It is a measure of variability, based on dividing a data set into quartiles. IQR = Q3 ???  Q1
# - one standard deviation above and below the mean of the data
# - the 9th percentile and the 91st percentile
# - the 2nd percentile and the 98th percentile.

# The interquartile range is often used to find outliers in data. Outliers here are 
# defined as observations that fall below Q1 ??? 1.5 IQR or above Q3 + 1.5 IQR. 
# In a boxplot, the highest and lowest occurring value within this limit are indicated 
# by whiskers of the box (frequently with an additional bar at the end of the whisker) 
# and any outliers as individual points.

# Any data not included between the whiskers should be plotted as an outlier with a dot, small circle, or star
# Some box plots include an additional character to represent the mean of the data

# On some box plots a crosshatch is placed on each whisker, before the end of the whisker.

# Rarely, box plots are presented with no whiskers at all.

# Because of this variability, it is appropriate to describe the convention
# being used for the whiskers and outliers in the caption for the plot

# The unusual percentiles 2%, 9%, 91%, 98% are sometimes? used for whisker cross-hatches
# and whisker ends to show the seven-number summary. If the data is normally distributed,
# the locations of the seven marks on the box plot will be equally spaced.

# load the csv
setwd("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/")

output <- "v3/results/statistical-tests/"

#load the dataset
dataset <-read.csv(paste("TCGA-2factores-withoutOutliers-preproc.csv", sep = ""), sep = ",", na.strings = "")
  
colnamesT <- colnames(dataset)[-ncol(dataset)]

classes <- levels(dataset[,ncol(dataset)])

colClassName <- colnames(dataset)[ncol(dataset)]

showOutliers <- TRUE

func<- geom_boxplot()

if(showOutliers==FALSE)
  func <- geom_boxplot(outlier.shape = NA)

#for each column
for(nameC in colnamesT){
  
  boxplotGraghic <- ggplot(dataset, aes_string(x=colClassName, y= nameC)) + 
    stat_boxplot(geom = "errorbar", width = 0.5) + func
  
  combinations <-combn(classes,2)
  
  l <- list()
  
  for(com in 1:ncol(combinations)){
    l<-c(l,list(combinations[,com]))
  }
  
  boxplotGraghic <- boxplotGraghic + geom_signif(comparisons = l, 
              map_signif_level=TRUE, test = "wilcox.test", na.rm = TRUE)

  ggsave(width = 5, height = 5, filename = paste(output,nameC,".png",sep = ""), bg = "transparent")
}
