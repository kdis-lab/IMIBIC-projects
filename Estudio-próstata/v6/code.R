barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("153", "163", "87", "158", "124", "148", "141", "81")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
wilks.p <-
c(Class = 0.00491744432706738)
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("153", "163", "87", "158", "124", "148", "141", "81")
hab <-
"Class"
plotellipses(res, axes = 1:2, invisible = 'quali', select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
c("KHDRBS1", "TIA1", "PTBP1", "RBM45", "SRSF9", "U2AF2", "SRSF5", 
"SRSF4", "SRSF10", "SF3B1", "SRSF2", "PRPF40A", "TRA2A", "RNU6", 
"MAGOH", "RBM3")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("Class N", "Class T")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = c('ind', 'ind.sup'), title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("153", "163", "87", "158", "124", "148", "141", "81")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
dimdesc(res, axes = 1:1)
