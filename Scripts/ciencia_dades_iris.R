#rm(list=ls())

##-- Paquets -------------------------------------------------------------------
library(FactoMineR)

##-- Llegir dades --------------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/iris.csv')
# iris <- read.csv('../Dades/iris.csv')

##-- Analisi de components principals ------------------------------------------
(pca.iris <- PCA(iris,quali.sup=c(5: 5)))

##-- Clustering jerarquic ------------------------------------------------------
(hcpc.iris <- HCPC(pca.iris,nb.clust=-1)) 
table(hcpc.iris$data.clust[,ncol(hcpc.iris$data.clust)],iris$Species)

##-- K-means -------------------------------------------------------------------
iris2 <- iris[,1:4]
km.iris <- kmeans(iris2,centers=3,nstart=10)
table(km.iris$cluster,iris$Species)

# Representacio en components principals del K-means ---------------------------
pr.comp <- princomp(iris2)
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km.iris$cluster)
