
#_______________________________________________________________________________
##### Two-ways ANOVA
##### (p=1, g=2, b=2)

# In a small village in Switzerland there are two gas stations:
# one of Esso and one of Shell. Both sell either gasoline 95 octanes
# and 98 octanes. 
# A young statistician wants to find out which is the best gas station
# and the best gasoline to refuel his car, in order to maximize the 
# number of kilometers covered with a single refueling.
# km/l  : (18.7, 16.8, 20.1, 22.4, 14.0, 15.2, 22.0, 23.3)
# distr.: ('Esso','Esso','Esso','Esso','Shell','Shell','Shell','Shell')
# benz. : ('95','95','98','98','95','95','98','98')
# (a) Via a two-ways ANOVA identify which is the best station and the
#     best gasoline for the young statistician to refuel his car.
# (b) Is there an interaction between the gas station and the gasoline?

km          <- c(18.7, 16.8, 20.1, 22.4, 14.0, 15.2, 22.0, 23.3)
distr       <- factor(c('Esso','Esso','Esso','Esso','Shell','Shell','Shell','Shell'))
benz        <- factor(c('95','95','98','98','95','95','98','98'))
distr_benz  <- factor(c('Esso95','Esso95','Esso98','Esso98','Shell95','Shell95','Shell98','Shell98'))

g <- length(levels(distr))
b <- length(levels(benz))
n <- length(km)/(g*b)

M           <- mean(km)
Mdistr      <- tapply(km, distr, mean)
Mbenz       <- tapply(km, benz, mean)
Mdistr_benz <- tapply(km, distr_benz, mean)

x11()
par(mfrow=c(2,3), las=2)
barplot(rep(M,4), names.arg=levels(distr_benz), ylim=c(0,24), main='No factor')
barplot(rep(Mdistr,each=2), names.arg=levels(distr_benz), ylim=c(0,24), 
        col=rep(c('blue','red'),each=2), main='Only Fact. Stat.')
barplot(rep(Mbenz,times=2), names.arg=levels(distr_benz), ylim=c(0,24),
        col=rep(c('darkgreen','orange'),times=2), main='Only Fact. Gas')
barplot(c(Mdistr[1]+Mbenz[1]-M, Mdistr[1]+Mbenz[2]-M, Mdistr[2]+Mbenz[1]-M, 
          Mdistr[2]+Mbenz[2]-M), names.arg=levels(distr_benz), ylim=c(0,24), 
        col=rep(c('darkgreen','orange'),times=2), density=rep(10,4), angle=135, 
        main='Additive model Stat.+Gas')
barplot(c(Mdistr[1]+Mbenz[1]-M, Mdistr[1]+Mbenz[2]-M, Mdistr[2]+Mbenz[1]-M, 
          Mdistr[2]+Mbenz[2]-M), names.arg=levels(distr_benz), ylim=c(0,24), 
        col=rep(c('blue','red'),each=2), density=rep(10,4), add=T)
barplot(Mdistr_benz, names.arg=levels(distr_benz), ylim=c(0,24), 
        col=rainbow(5)[2:5], main='Model with Interact. Stat.+Gas.')
plot(distr_benz, km, col=rainbow(5)[2:5], ylim=c(0,24),xlab='')

#_______________________________________________________________________________
##### Two-ways ANOVA

### Model with interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk;
#i=1,2 (effect station), j=1,2 (effect gasoline)

fit.aov2.int <- aov(km ~ distr + benz + distr:benz)
summary.aov(fit.aov2.int) #no interactions

### Additive model: 
### X.ijk = mu + tau.i + beta.j + eps.ijk;
### i=1,2 (effect station), j=1,2 (effect gasoline)
fit.aov2.ad <- aov(km ~ distr + benz)
summary.aov(fit.aov2.ad) #gas no effect

### Reduced additive model (ANOVA one-way, b=2): 
### X.jk = mu + beta.j + eps.jk; 
###     j=1,2 (effect gasoline)
fit.aov1 <- aov(km ~ benz)
summary.aov(fit.aov1)

#even if we hadn't all the assumptions, now the model is reduced, so ok
Ps <- c(shapiro.test(km[ benz==levels(benz)[1] ])$p,
        shapiro.test(km[ benz==levels(benz)[2] ])$p)
Ps
bartlett.test(km, benz)

#_______________________________________________________________________________
##### Two-ways ANOVA: CI for the mean 

IC <- c(diff(Mbenz) - qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))), 
        diff(Mbenz) + qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))
names(IC) <- c('Inf', 'Sup')
IC 
