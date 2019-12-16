barplot(res$eig[,2], names.arg = 1:nrow(res$eig))
drawn <-
c("28", "12", "46", "23", "31", "21", "43", "13", "25", "22", 
"37", "24", "30", "16", "49", "10", "19", "41", "17", "50")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = 'quali', title = '', cex = cex)
wilks.p <-
structure(c(0.137513632839709, 0.157425631906187, 0.724530771894072, 
0.88301618474201, 0.916441759316738), .Names = c("Dyslipidemia", 
"Hbpressure", "Diabetes", "PerineuralInv", "ProstExt"))
wilks.p
sample = sample(rownames(res$call$X), length(rownames(res$call$X)))
res$call$X = res$call$X[sample,]
res$ind$coord = res$ind$coord[sample[!sample %in% rownames(res$ind.sup$coord)],]
res$ind.sup$coord = res$ind.sup$coord[sample[sample %in% rownames(res$ind.sup$coord)],]
drawn <-
c("28", "12", "46", "23", "31", "21", "43", "13", "25", "22", 
"37", "24", "30", "16", "49", "10", "19", "41", "17", "50")
hab <-
"Dyslipidemia"
plotellipses(res, axes = 1:2, invisible = 'quali', select = drawn, keepvar = hab, title = '', cex = cex)
drawn <-
c("SF3B1", "SRSF3", "KHDRSB1", "NOVA1", "U2AF2", "SRRM1", "SF3B1tv1", 
"U4ATAC", "RBM22")
plot.PCA(res, select = drawn, axes = 1:2, choix = 'var', title = '', cex = cex)
drawn <-
c("Dyslipidemia_No", "Dyslipidemia_Si", "Hbpressure_Si", "Hbpressure_No"
)
plot.PCA(res, select = drawn, axes = 1:2, choix = 'ind', invisible = c('ind', 'ind.sup'), title = '', cex = cex)
res.hcpc = HCPC(res, nb.clust = -1, graph = FALSE)
drawn <-
c("28", "12", "46", "23", "31", "21", "43", "13", "25", "22", 
"37", "24", "30", "16", "49", "10", "19", "41", "17", "50")
plot.HCPC(res.hcpc, choice = 'map', draw.tree = FALSE, select = drawn, title = '')
dimdesc(res, axes = 1:2)
res.hcpc$desc.var
