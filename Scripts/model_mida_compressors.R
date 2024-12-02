################################################################################
# Script per estudi tipus de models
# PE 2023-24
# 1. Estimar mitjana
# 2. Estimar diferencia de mitjanes aparellades
# 3. Estimar mitjanes segons categories de factors
# 4. Model lineal simple
# 5. Model lineal multiple
################################################################################

rm(list=ls) # Esborrar objectes en memoria

################################################################################
# Carregar llibreries
# mida: mida original del fitxer
# tar:  mida del fitxer despres de comprimir amb tar
# zip:  mida del fitxer despres de comprimir amb zip
# type: tipus de fitxer
################################################################################
# install.packages("emmeans")
# install.packages("PairedData")
library(emmeans)
library(PairedData)

################################################################################
# Llegir les dades
# mida: mida original del fitxer
# tar:  mida del fitxer despres de comprimir amb tar
# zip:  mida del fitxer despres de comprimir amb zip
# type: tipus de fitxer
################################################################################
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/mida_compressors.csv')
# d <- read.table('../Dades/mida_compressors.csv',sep=',',header=TRUE, stringsAsFactors = TRUE)
summary(d)
d[,1:3] <- d[,1:3]/1000 # Per tenir numeros mes petits

################################################################################
# Model per estimar una mitjana
################################################################################
##-- Model per estimar una mitjana
mod_1 <- lm(zip~1,data = d)
s <- summary(mod_1)
s

##-- IC95% de la mitjana a ma
m  <- mod_1$coefficients            # mitjana
se <- s$coefficients[,'Std. Error'] # error estandard
q  <- qt(0.975,mod_1$df)            # quantil
m - q * se
m + q * se

##-- IC95% amb funcio
confint(mod_1)

##-- Normalitat
qqnorm(d$zip)
qqline(d$zip)

##-- Logaritmes
qqnorm(log(d$zip))
qqline(log(d$zip))

################################################################################
# Model per estimar una diferencia de mitjanes aparellada
################################################################################
##-- Crear variable diferencia
d$dif <- d$zip - d$tar

##-- Model per estimar una diferencia de mitjanes aparellada
mod_2 <- lm(dif~1,data = d)
s <- summary(mod_2)
s

##-- IC95% de la diferencia de mitjanes a ma
m  <- mod_2$coefficients            # mitjana
se <- s$coefficients[,'Std. Error'] # error estandard
q  <- qt(0.975,mod_1$df)            # quantil
m - q * se
m + q * se

##-- IC95% de la diferencia de mitjanes amb funcio
confint(mod_2)

##-- Normalitat de la diferencia
qqnorm(d$dif)
qqline(d$dif)

##-- Bland-Altman
# Sense logs
x <- d$zip
y <- d$tar
p <- paired(x,y)
plot(p,type="BA")

# Amb logs --> Empitjora
x <- log(d$zip)
y <- log(d$tar)
p <- paired(x,y)
plot(p,type="BA")

################################################################################
# Model amb un factor
################################################################################
##-- Crear nova variable i descriptiva
d$rati <- d$zip/d$mida
summary(d$rati)
table(d$type)
with(d,tapply(rati,type,mean))

##-- Model amb un factor
mod_3 <- lm(rati~type,data = d)
s <- summary(mod_3)
s

##-- Validacio Premissa de normalitat (sobre els residus)
qqnorm(resid(mod_3))
qqline(resid(mod_3))

##-- Validacio Premissa de homocesdatiscitat
boxplot(rati~type,d)

##-- IC95% dels coeficients (NO de les mitjanes per grup)
confint(mod_3)

##-- IC95% de les mitjanes per grup (NO dels coeficients)
em <- emmeans(mod_3,~type)
em

##-- Comparatives 2 a 2 de les mitjanes per grup (independents)
plot(em)   # IC95% graficament
pairs(em)  # Diferencies de mitjanes


################################################################################
# Model lineal simple (MLS)
################################################################################
##-- Descriptiva
plot(zip~mida,data = d)

##-- Model lineal simple
mod_4 <- lm(zip~mida,data = d)
s <- summary(mod_4)
s

##-- Validacio Premisses (MLS)
par(mfrow=c(2,2))
plot(mod_4,ask=FALSE)

##-- Transformacio logaritmica amb diverses possibilitats
par(mfrow=c(1,3))
plot(log(zip) ~ mida,     data = d)
plot(zip      ~ log(mida),data = d)
plot(log(zip) ~ log(mida),data = d)
d$logzip  <- log(d$zip)
d$logmida <- log(d$mida)

##-- Model amb logs
mod_5 <- lm(logzip~logmida,data = d)
s <- summary(mod_5)
s

##-- Validacio Premisses
par(mfrow=c(2,2))
plot(mod_5,ask=FALSE)

################################################################################
# Model lineal multiple (MLM)
################################################################################
##-- Model lineal multiple
mod_6 <- lm(zip~mida+type,data = d)
s <- summary(mod_6)
s

##-- Validacio Premisses mod_6 (MLM)
par(mfrow=c(2,2))
plot(mod_6,ask=FALSE)

##-- Model amb interaccio
mod_7 <- lm(zip ~ mida*type,data = d)
s <- summary(mod_7)
s

##-- Validacio Premisses mod_7 (MLM)
par(mfrow=c(2,2))
plot(mod_7,ask=FALSE)

##-- Model amb interaccio i logs
mod_8 <- lm(logzip ~ logmida*type,data = d)
s <- summary(mod_8)
s

##-- Validacio Premisses mod_8 (MLM)
par(mfrow=c(2,2))
plot(mod_8,ask=FALSE)
