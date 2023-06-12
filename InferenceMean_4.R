#_______________________________________________________________________________
### TESTS AND CR FOR THE MEAN OF MULTI-GAUSSIAN
mu <- c(1, 0)
sig <- matrix(c(1, 1, 1, 2), nrow=2)
set.seed(123)
x <- rmvnorm(n=30, mean=mu, sigma=sig)
x <- data.frame(X.1=x[,1], X.2=x[,2])

n <- dim(x)[1]
p <- dim(x)[2]

x.mean   <- sapply(x, mean)
x.cov    <- cov(x)
x.invcov <- solve(x.cov)

#_______________________________________________________________________________
### Test on the mean of level alpha=1%, assume gaussianity and non n large

### H0: mu == mu0 vs H1: mu != mu0
### with mu0 = c(1, 0)
###-----------------------------------
mshapiro.test(t(x))

alpha <- 0.01
mu0 <- c(1,0)

# T2 Statistics
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
x.T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2 > cfr.fisher}
# (we reject for large values of the T2 statistics)

# Compute the p-value 
P <- 1 - pf(x.T2*(n-p) / ((n-1)*p), p, n-p)
P #we cannot reject H0 for any reasonable level (we would reject for a level alpha>81.7%)


#_______________________________________________________________________________
### Region of Rejection
X11()
plot(x, asp = 1)
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16) #centered in mu0

points(x.mean[1], x.mean[2], pch = 16, col ='red', cex = 1.5)
ellipse(x.mean, x.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1) #cenetered in sample mean

# Which relation between the two ellipses?
# - If the rejection region does NOT contain the sample mean (i.e., we
#   are in the acceptance region), then we cannot reject H0 
#   (i.e., if the sample mean falls within the ellipse we accept H0)
# - If the mean under H0 (mu0) is contained in the confidence region
#   of level 1-alpha, then we do not reject H0 at level alpha
# => the confidence region of level 1-alpha contains all the mu0
#    that we would accept at level alpha


#_______________________________________________________________________________
### Asymptotic test on the mean, just 30 datapoints

mu0   <- c(1,0)

x.T2A   <- n * (x.mean-mu0) %*%  x.invcov  %*% (x.mean-mu0)
cfr.chisq <- qchisq(1-alpha, p)
x.T2A < cfr.chisq # no statistical evidence to reject H0 at level alpha

# Compute the p-value
PA <- 1 - pchisq(x.T2A, p)
PA

X11()
par(mfrow=c(1,2))
plot(x, asp = 1,main='Comparison rejection regions')
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 1,
        center.pch = 4, center.cex=1.5, lwd=2)
ellipse(mu0, x.cov/n, sqrt(cfr.chisq), col = 'lightblue', lty = 1,
        center.pch = 4, center.cex=1.5, lwd=2)
points(mu0[1], mu0[2], pch = 4, cex = 1.5, lwd = 2, col ='lightblue')
legend('topleft', c('Exact', 'Asymptotic'), col=c('blue', 'lightblue'),
       lty=c(1), lwd=2)

plot(x, asp = 1,main='Comparison of confidence regions')
ellipse(x.mean, x.cov/n, sqrt(cfr.fisher), col = 'red', lty = 1, center.pch = 4,
        center.cex=1.5, lwd=2)
ellipse(x.mean, x.cov/n, sqrt(cfr.chisq), col = 'orange', lty = 1, center.pch = 4,
        center.cex=1.5, lwd=2)
points(x.mean[1], x.mean[2], pch = 4, cex = 1.5, lwd = 2, col ='orange')
legend('topleft', c('Exact', 'Asymptotic'), col=c('red','orange'),
       lty=c(1), lwd=2)

graphics.off()

#_______________________________________________________________________________
### SIMULTANEOUS CR
T2 <- cbind(inf = x.mean - sqrt(cfr.fisher*diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + sqrt(cfr.fisher*diag(x.cov)/n))
T2

X11()
par(mfrow=c(1,1))

plot(x, asp = 1,main='Confidence and rejection regions')
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16) #CR centered in mu0
points(x.mean[1], x.mean[2], pch = 16, col = 'red', cex=1.5)
ellipse(x.mean, shape=x.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, center.pch = 16) #CR centered in sample mean
rect(T2[1,1], T2[2,1], T2[1,3], T2[2,3], border='red', lwd=2) #simultaneous CR

#_______________________________________________________________________________
### ADD BONFERRONI

k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k), n-1)
Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bf

# we add the Bonferroni intervals to the plot
rect(Bf[1,1], Bf[2,1], Bf[1,3], Bf[2,3], border='orange', lwd=2)
legend('topleft', c('Rej. Reg.', 'Conf. Reg','T2-sim', 'Bonferroni'),
       col=c('blue','red','red','orange'), lty=c(2,2,1,1), lwd=2)

graphics.off()

#_______________________________________________________________________________
### ANOTHER EXAMPLE

stiff <- read.table('stiff.dat')
head(stiff)
dim(stiff)

n <- dim(stiff)[1]
p <- dim(stiff)[2]

X11()
plot(stiff,pch=19)

dev.off()

### Normality test
mvn(stiff)$multivariateNormality
mvn(stiff_wo_outliers)$multivariateNormality

n <- dim(stiff_wo_outliers)[1]
p <- dim(stiff_wo_outliers)[2]

### Test for the mean of level 5%
###--------------------------------
# 1) Formulate the test:
#    H0: mu == mu0 vs H1: mu != mu0
#    with mu0 = c(1850, 1750, 1500, 1700)

mu0   <- c(1850, 1750, 1500, 1700)
alpha <- 0.05

# 2) Compute the test statistics
x.mean   <- colMeans(stiff_wo_outliers)
x.cov    <- cov(stiff_wo_outliers)
x.invcov <- solve(x.cov)

x.T2     <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 

# 3a) Verify if the test statistics belongs to the rejection region
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
x.T2 < cfr.fisher # we accept H0 at 5%

# 3b) Compute the p-value
P <- 1 - pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P

### Confidence region for the mean of level 95%
###------------------------------------------------------------
# 1)  Identify the type of region of interest:
#     CR for the mean (ellipsoidal region) 
#     {m \in R^4 s.t. n * (x.mean-m)' %*% x.invcov %*% (x.mean-m) < cfr.fisher}
# 2)  Characterize the region: compute the center, direction of 
#     the principal axes, length of the axes

# Center:
x.mean

# Directions of the principal axes:
eigen(x.cov/n)$vectors

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values)

# Question: how to plot the confidence region? 
# (I don't know how to plot in R^4!)
# I can work with 'representations' of the confidence regions
# (e.g., projections along particular directions)

# We plot the projections of the ellipsoid in some directions 
# of interest (e.g. the X1,...,Xp coordinates)
# => We plot the simultaneous T2 confidence intervals in each
#    direction of interest (with global coverage alpha)

### Simultaneous T2 intervals on the components of the mean
### with global level 95%
###----------------------------------------------------

T2 <- cbind(inf = x.mean - sqrt(cfr.fisher*diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + sqrt(cfr.fisher*diag(x.cov)/n))
T2

X11()

# Plot of the simultaneous T2 intervals in the direction of X1,...,X4
matplot(1:4,1:4, pch='',ylim=range(stiff_wo_outliers), xlab='Variables', ylab='T2 for a component', 
        main='Simultaneous T2 conf. int. for the components')
for(i in 1:4) segments(i, T2[i,1], i, T2[i,3], lwd=3, col=i)
points(1:4, T2[,2], pch=16, col=1:4)

# Is mu0 inside the rectangular region?
# We add it to the plot
points(1:4, mu0, lwd=3, col='orange')

# Yes, it is, because it is inside all the T2-intervals,

### Bonferroni intervals on the components of the mean
### with global level 95%
###----------------------------------------------------
k <- p
cfr.t <- qt(1 - alpha/(k*2), n-1)

Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bf

# Let's do a plot
X11()
matplot(1:4, 1:4, pch='', ylim=range(stiff_wo_outliers), xlab='Variables',
        ylab='Confidence intervals along a component', main='Confidence intervals')

for(i in 1:4) segments(i, T2[i,1], i, T2[i,3], lwd=2, col='grey35', lty=3)
points(1:4, T2[,1], pch='-', col='grey35')
points(1:4, T2[,3], pch='-', col='grey35')

for(i in 1:4) segments(i, Bf[i,1], i, Bf[i,3], lwd=2, col=i)
points(1:4, Bf[,2], pch=16, col=1:4)
points(1:4, Bf[,1], pch='-', col=1:4)
points(1:4, Bf[,3], pch='-', col=1:4)

# Is mu0 inside the Bonferroni confidence region?
# we add it to the plot
points(1:4, mu0, lwd=3, col='orange')

# Yes, it is, because it belongs to all the intervals along the components

graphics.off()
