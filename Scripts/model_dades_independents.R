################################################################################
# Comparació mitjane samb dades indepndependents
# Notes de estudiants FIB, TELECOS (TEL) o INDUSTRIALS (IND)
################################################################################
rm(list=ls())

##-- Carregar paquets ----------------------------------------------------------
library(emmeans)

##-- Generar notes -------------------------------------------------------------
set.seed(12345)
n <- 10
FIB <- rnorm(n, 7.5, 1)
TEL <- rnorm(n, 6.5, 1)
IND <- rnorm(n, 7, 1)
d <- data.frame(x = factor(c(rep('FIB',n), rep('TEL',n), rep('IND',n))),
                y = c(FIB, TEL, IND))

##-- Model ---------------------------------------------------------------------
m <- lm(y~x, data = d)
summary(m)

##-- Interpretacio -------------------------------------------------------------
confint(m)             # Intervals de confianca de coeficients
(em <- emmeans(m, ~x)) # Mitjanes de cada grup
plot(em)               # Intervals de confianca de mitjanes de cada grup

##-- Premisses -----------------------------------------------------------------
##-- Normalitat
par(mfrow=c(1,3))

qqnorm(FIB)
qqline(FIB, col=2)

qqnorm(TEL)
qqline(TEL, col=2)

qqnorm(IND)
qqline(IND, col=2)

##-- Homoscedaticitat
par(mfrow=c(1,1))
boxplot(y~x, data = d)

