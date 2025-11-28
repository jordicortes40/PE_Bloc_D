rm(list=ls())

##-- Paquets -------------------------------------------------------------------
library(emmeans)

##-- Temps compressors ---------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/temps_compressors.csv')
# d <- read.csv('../Dades/temps_compressors.csv')

mod <- lm(temps~1,d)
summary(mod)

# Premissa normalitat
qqnorm(d$temps)
qqline(d$temps)

##-- Algoritme Dijkstra --------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/algoritme_dijkstra.csv')
# d <- read.csv('../Dades/algoritme_dijkstra.csv')
mod <- lm(lgt~as.factor(nodes),d)
summary(mod)

# Premissa normalitat
qqnorm(d$lgt)
qqline(d$lgt)

# Premissa homoscedasticitat
boxplot(lgt~nodes,d)

# IC95% per les mitjanes dels temps dels nodes
emmeans(mod,~nodes)

##-- Recorre arbres ------------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/recorre_arbres.csv')
# d <- read.csv('../Dades/recorre_arbres.csv')

##-- Model amb metode
mod1 <- lm(Temps ~ as.factor(metode),d)
summary(mod1)

# Premissa normalitat residus
qqnorm(resid(mod1))
qqline(resid(mod1))

# Premissa homoscedasticitat
boxplot(Temps~metode,d)

##-- Model amb nodes
mod2 <- lm(Temps~nodes,d)
summary(mod2)

plot(Temps~nodes,d)

# Premisses
par(mfrow=c(2,2))
plot(mod2,ask=FALSE)

##-- Model complert
mod3 <- lm(Temps ~ nodes + as.factor(metode),d)
summary(mod3)

# Premisses
par(mfrow=c(2,2))
plot(mod3,ask=FALSE)


##-- Benzina -------------------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/benzina_velocitat.csv')
# d <- read.csv('../Dades/benzina_velocitat.csv')

# Model
mod <- lm(fuel~speed,d)
summary(mod)

# Grafic
par(mfrow=c(1,1))
plot(fuel~speed,d, pch=19, col='blue')
abline(mod,lwd=2,col='red')

# Premisses
par(mfrow=c(2,2))
plot(mod,ask=FALSE)

##-- Cervesa alcohol -----------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/cervesa_alcohol.csv')
# d <- read.csv('../Dades/cervesa_alcohol.csv')

# Model
mod <- lm(alc~n.cerv,d)
summary(mod)

# Premisses
par(mfrow=c(2,2))
plot(mod,ask=FALSE)


##-- brillantor durada ---------------------------------------------------------
d <- read.csv('https://raw.githubusercontent.com/jordicortes40/PE_Bloc_D/main/Dades/brillantor_durada.csv')
# d <- read.csv('../Dades/brillantor_durada.csv')

# Model
mod <- lm(Durada~Brillantor,d)
summary(mod)

# Premisses
par(mfrow=c(2,2))
plot(mod,ask=FALSE)

# Grafic
par(mfrow=c(1,1))
plot(Durada~Brillantor,d, pch=19, col='blue')
abline(mod,lwd=2,col='red')



