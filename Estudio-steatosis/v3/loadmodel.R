library(caret)

path <- "results/Curve3/rf/sanos-enfermos/"

modelFit <- readRDS(paste0(path, "modelfit-rf.rds"))

predictions <- modelFit$pred[modelFit$pred$mtry == modelFit$bestTune$mtry,]

predictions <- predictions[, c("pred", "obs", "E", "S")]

write.csv(predictions, file = paste0(path, "CV-residuals-complete.csv"), row.names = FALSE, quote = FALSE)