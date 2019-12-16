library(caret)

set.seed(3527)
subjects <- c(c(1:20), c(1:20))

table(subjects)

folds <- groupKFold(subjects, k = 20)



