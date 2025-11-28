rm(list=ls())

##-- Paquets -------------------------------------------------------------------
library(PairedData)

##-- Data ----------------------------------------------------------------------
data(Shoulder)
summary(Shoulder)
View(Shoulder)

##-- Model ---------------------------------------------------------------------
d <- Shoulder
d$dif <- d$Right - d$Left
m <- lm(dif~1, data = d)
summary(m)

##-- IC95% ---------------------------------------------------------------------
confint(m)
t.test(d$Right, d$Left, paired = TRUE)

##-- Premisses -----------------------------------------------------------------
# Normalitat
qqnorm(d$dif)
qqline(d$dif, col=2)

# Efecte constant
x <- d$Right
y <- d$Left
p <- paired(x,y)
plot(p, type = 'BA')
