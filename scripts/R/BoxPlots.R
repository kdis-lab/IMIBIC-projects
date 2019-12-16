#example data similar to your barplot
d <- data.frame(group=rep(c("control","group1","group2"),each=4),
                esker=c(1.6,1.4,1.8,1.5,2,1.8,1.6,1.4,2.3,2,1.7,1.4),
                se=rep(0.1,12),
                cond=rep(c("t1","t2","t3","t4"),3))
#dotplot - you need Hmisc library for version with error bars
library(Hmisc)
Dotplot(cond ~ Cbind(esker, esker+se, esker-se) | group, data=d, col=1, 
        layout=c(1,3), aspect="xy",
        par.settings = list(dot.line=list(lwd=0), plot.line=list(col=1)))


A = c(1, 5, 8, 17, 16, 3, 24, 19, 6) 
B = c(2, 16, 5, 7, 4, 7, 3)
C = c(1, 1, 3, 7, 9, 6, 10, 13) 
D = c(2, 15, 2, 9, 7) 
junk = list(g1=A, g2=B, g3=C, g4=D) 
boxplot(junk) 

summary(A)
summary(B)
summary(C)
summary(D)

dd <- data.frame(y=as.vector(unlist(junk)), 
                 g=rep(paste("g", 1:4, sep=""), unlist(lapply(junk, length))))

aov.res <- kruskal.test(y ~ g, data=dd)
alpha.level <- .05/nlevels(dd$g)  # Bonferroni correction, but use 
# whatever you want using p.adjust()

# generate all pairwise comparisons
idx <- combn(nlevels(dd$g), 2)

# compute p-values from Wilcoxon test for all comparisons
pval.res <- numeric(ncol(idx))
for (i in 1:ncol(idx))
  # test all group, pairwise
  pval.res[i] <- with(dd, wilcox.test(y[as.numeric(g)==idx[1,i]], 
                                      y[as.numeric(g)==idx[2,i]]))$p.value

# which groups are significantly different (arranged by column)
signif.pairs <- idx[,which(pval.res<alpha.level)]

boxplot(y ~ g, data=dd, ylim=c(min(dd$y)-1, max(dd$y)+1))
# use offset= to increment space between labels, thanks to vectorization
for (i in 1:ncol(signif.pairs))
  
  MyText = rep('',nlevels(ddg))for(iin1:ncol(signif.pairs))MyText[signif.pairs[,i]]=paste(MyText[signif.pairs[,i]],letters[i],sep='')text(c(1:nlevels(ddg)), rep(max(ddy)+1,nlevels(ddg)),MyText)
  
  text(signif.pairs[,i], max(dd$y)+1, letters[i], pos=4, offset=i*.8-1)
