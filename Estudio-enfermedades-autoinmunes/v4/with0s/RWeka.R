library(RWeka)

hc <- make_Weka_clusterer("weka/clusterers/HierarchicalClusterer")

mydata <- read.csv("with0s/LinfoTrombosis-preproc.csv")

clust <- hc(x = mydata[-46], Weka_control(N=2,L='Ward'))

p <- predict(clust, newdata = NULL, type = c("class_ids"))

p <- predict(clust, newdata = mydata[-46], type = c("memberships"))

WOW(hc)
