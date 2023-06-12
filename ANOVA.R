
library(MVN)
library(car)
library(heplots)

#_______________________________________________________________________________
##### ANOVA: FRAMEWORK
##### (p=1, g=6)

# Dataset Chicken Weights:
# Data from an experiment to measure and compare the effectiveness
# of various feed supplements on the growth rate of chickens. 
attach(chickwts)

### Model: weigth.ij = mu + tau.i + eps.ij; eps.ij ~ N(0,sigma^2)
### H0: tau.1 = tau.2 = tau.3 = tau.4 = tau.5 = tau.6 = 0, no effect of feeds
### H1: complementary to H0, at least one feed has effect

x11()
par(mfrow=c(1,2))
barplot(rep(mean(weight),6), names.arg=levels(feed), ylim=c(0, max(weight)),
        las=2, col='grey85', main='Model under H0')
barplot(tapply(weight, feed, mean), names.arg=levels(feed), ylim=c(0, max(weight)),
        las=2, col=rainbow(6), main='Model under a special case of
        H1 with 6 populations')

dev.off()

# This is a case of one-way ANOVA: one variable (weight) observed 
# over g=6 levels (feed)
n       <- length(feed)      # total number of obs.
ng      <- table(feed)       # number of obs. in each group
treat   <- levels(feed)      # levels of the treatment
g       <- length(treat)     # number of levels (i.e., of groups)

#_______________________________________________________________________________
##### ANOVA: ASSUMPTIONS
# 1) normality (univariate) in each group (6 tests)
Ps <- c(shapiro.test(weight[feed==treat[1]])$p,
        shapiro.test(weight[feed==treat[2]])$p,
        shapiro.test(weight[feed==treat[3]])$p,
        shapiro.test(weight[feed==treat[4]])$p,
        shapiro.test(weight[feed==treat[5]])$p,
        shapiro.test(weight[feed==treat[6]])$p) 
Ps

# 2) same covariance structure, H0: same variance
# WARNING: Test extremely sensitive to departures from normality (low robustness)

bartlett.test(weight, feed)

#_______________________________________________________________________________
##### ANOVA: GO

fit <- aov(weight ~ feed)
#p-value: [H0: tau_i = 0]
summary(fit)

### We reject the test, the treatment could have an effect 

#_______________________________________________________________________________
##### ANOVA: CHECK WHICH GROUP HAS IMPACT WITH BONFERRONI
k <- g*(g-1)/2
alpha= 0.05

Mediag  <- tapply(weight, feed, mean) # group-wise means
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)

# CI for all the differences
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * ( 1/ng[i] + 1/ng[j])))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt(S * (1/ng[i] + 1/ng[j])))))
  }}

x11()
par(mfrow=c(1,2))
plot(feed, weight, xlab='treat', ylab='weight', col = rainbow(6), las=2) #data

h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',xlab='pairs treat', ylab='Conf. Int. tau weight') #mean & bonferroni
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, ICrange[ind,1], col=rainbow(6)[j], pch=16); 
    points(h, ICrange[ind,2], col=rainbow(6)[i], pch=16); 
    h <- h+1
  }}
abline(h=0)
dev.off()

#_______________________________________________________________________________
##### ANOVA: CHECK WHICH GROUP HAS IMPACT WITH MULTIPLE UNIV. CI
Auni <- matrix(0,6,6)
for(i in 1:6) {
  for(j in i:6) {
    Auni[i,j] <- Mediag[i]-Mediag[j] + qt(1-alpha/2, n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  for(j in 1:i) {
    Auni[i,j] <- Mediag[j]-Mediag[i] - qt(1-alpha/2, n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] ) )}
  Auni[i,i]     <- 0
}

x11()
h <- 1
plot(c(1,g*(g-1)/2),range(Auni), pch='', xlab='pairs treat', 
     ylab='CI delta weight', main='Univariate Conf. Int.', col='grey55')
for(i in 1:5) {
  for(j in (i+1):6) {lines (c(h,h), c(Auni[i,j],Auni[j,i])); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, Auni[i,j], col=rainbow(6)[i], pch=16); 
    points(h, Auni[j,i], col=rainbow(6)[j], pch=16); 
    h <- h+1
  }}
abline(h=0)

