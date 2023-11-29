rm(list=ls())

##-- Paquets -------------------------------------------------------------------
library(FactoMineR)

##-- Llegir dades --------------------------------------------------------------
# load(url("https://www-eio.upc.edu/teaching/pe/DADES/jetstream.Rdata"))
load("../dades/jetstream.Rdata")
dim(C)

##-- Analisi de components principals ------------------------------------------
pca = PCA(C[,-c(3:4)], quali.sup=1:2, quanti.sup=3)
