barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("s2", "s21", "s25", "s3", "s11", "s29", "s13", "s34", "s17", 
"s10", "s43", "s22")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
wilks.p <-
structure(0.00516083667453511, .Names = "Class")
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("s2", "s21", "s25", "s3", "s11", "s29", "s13", "s34", "s17", 
"s10", "s43", "s22")
hab <-
"Class"
plotellipses(res, axes = 1:2, invisible = 'quali', select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("Class N", "Class S")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = c('ind', 'ind.sup'), title = '', cex = cex)
drawn <-
c("s22", "s24", "s17", "s10", "s15", "s40", "s21", "s43", "s6", 
"s33")
plot.PCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = 'quali', title = '', cex = cex)
wilks.p <-
structure(0.257667208903072, .Names = "Class")
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("s22", "s24", "s17", "s10", "s15", "s40", "s21", "s43", "s6", 
"s33")
hab <-
"Class"
plotellipses(res, axes = 3:4, invisible = 'quali', select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
integer(0)
plot.PCA(res, select = drawn, axes = 3:4, choix = 'var', title = '', cex = cex)
drawn <-
"Class S"
plot.PCA(res, select = drawn, axes = 3:4, choix = 'ind', invisible = c('ind', 'ind.sup'), title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("s2", "s21", "s25", "s3", "s11", "s29", "s13", "s34", "s17", 
"s10", "s43", "s22")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
