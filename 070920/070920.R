library(MASS)
library(car)
library(rgl)
library(glmnet)
library(sp)           ## Data management
library(lattice)      ## Data management
library(gstat)
library(fda) 
library(KernSmooth) 
library(rgl)
library(fields)
remove(list = setdiff(ls(), lsf.str()))
load("~/Documents/WeBeep Sync/APPLIED STATISTICS/Labs/Lab 5/mcshapiro.test.RData")
setwd("~/data_hdd2/MEGA/AppStat Exams (in English)/Exams (in English)/070920")


# Problem 2

# To compare the performances of the led bulbs produced by two brands (Candle Inc. and Sunshine Corp.), brightness
# tests are performed on 50 led bulbs independently sampled from those produced by Candle Inc. and 50 led bulbs
# independently sampled from those produced by Sunshine Corp.
# For each led bulb, the brightness is measured by two light meters positioned at different distances from the led
# bulb: the first light meter is a few centimeters from the led bulb and the second is positioned at one meter from
# the led bulb. The measurements on different led bulbs are independent (while the measurements on the same led
#                                                                       bulb are not).
# Files candle.txt and sunshine.txt report the measurements obtained with the two light meters on each led
# bulb produced by Candle Inc. and Sunshine Corp. respectively.

# a) Perform a statistical test of level 95% to verify if the mean measurements on the led bulbs produced by the two
#     brands differ. State the model assumptions required to perform the test and verify them.
candles <- read.table("candle.txt")
sunshine <- read.table("sunshine.txt")

D <- data.frame(LM1 = candles[,1] - sunshine[,1], candles[,2] - sunshine[,2])
D
mcshapiro.test(D)

n <- dim(D)[1]
p <- dim(D)[2] 

# compute T2 

D.mean <- sapply(D, mean)
D.cov <- cov(D)
D.invcov <- solve(D.cov)
alpha <- .05
delta.0 <- c(0,0)

D.T2 <- n * (D.mean - delta.0) %*% D.invcov %*% (D.mean - delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher

# we reject the hypothesis

# b) Compute and report the p-value of the test at point (a).

pvalue <- 1-pf(D.T2*(n-p)/(p*(n-1)),p,n-p)
pvalue

# p-value is very low, under 0.05 so this confirms the rejection of the hypothesis

# c) Interpret the results of the test at point (a) through two Bonferroni intervals of global level 95% for appropriate
#   differences in the mean. Comment the result.

k <- 2
cfr.t <- qt(1 - alpha/(2*k), n - 1)
ic.bf.lm1 <- c(D.mean[1]-cfr.fisher*sqrt(D.cov[1,1]/n),
               D.mean[1], D.mean[1] + cfr.t*sqrt(D.cov[1,1]/n))
ic.bf.lm2 <- c(D.mean[2]-cfr.fisher*sqrt(D.cov[2,2]/n),
               D.mean[2], D.mean[2] + cfr.t*sqrt(D.cov[2,2]/n))
bf <- rbind(ic.bf.lm1, ic.bf.lm2)
dimnames(bf)[[2]] <- c('inf','center','sup')
bf

# Along these 2 directions we see that the intervals don't contain 
# the origin, again, confirming our hypothesis.

# d) Is there statistical evidence to state that, at level 95%, the decrease in brightness between the two measurements
#   of the led bulbs produced by Candle Inc. is in mean higher than the one of the led bulbs produced by Sunshine
#   Corp.?

# We must define a new difference vector and compare the two populations
# It will be called D as well
diff1 <- candles[,1] - candles[,2]
diff2 <- sunshine[,1] - sunshine[,2]
D <- data.frame(diff1, diff2)
D
mcshapiro.test(D)

# Comparison of the mean of two populations

candles.mean <- mean(diff1)
sunshine.mean <- mean(diff2)
candles.mean
sunshine.mean
candles.cov  <-  var(diff1)
sunshine.cov  <-  var(diff2)

n1 <- nrow(candles)
n2 <- nrow(sunshine)

Sp <- ((n1-1)*candles.cov + (n2-1)*sunshine.cov)/(n1+n2-2)
list(S1=candles.cov, S2=sunshine.cov, Spooled=Sp)
alpha   <- .05
delta.0 <- 0
Spinv   <- solve(Sp)


T2 <- n1*n2/(n1+n2) * (candles.mean-sunshine.mean-delta.0) %*% Spinv %*% (candles.mean-sunshine.mean-delta.0)
T2

p <- 1

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P

# Problem n.4
# To forecast the tidal currents in the port of Dublin, the vertical motion induced by the rise and fall of the tides is
# monitored. The file tide.txt reports the measurements of the sea level in the port of Dublin collected every half
# hour for one day. Since the time variation of the sea level is inherently a continuous process, consider a functional
# data analysis approach. It is known that the available measurements are affected by small errors.

tide <- read.table("tide.txt", sep =" ", header =T)
tide

# a) Perform a smoothing of the data using a B-spline basis of degree 3. Choose the number of basis functions using
# a generalized cross-validation criterion. Report the number of basis functions chosen, a plot of the B-spline
# basis system used and a plot of the smoothed data.
# We plot the data

NT <- dim(tide)[1]
abscissa <- 1:48
Xobs0 <- tide$level

# Seems to be periodic
# We plot the first and second derivative

plot(abscissa,Xobs0, type = "l")
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])
X11()
par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data",type="l")
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")

dev.off()
# Very noisy
# We perform smoothing with a b-spline basis
# Set parameters
m <- 4           # spline order 
degree <- m-1    # spline degree 

nbasis <- 5:47
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(range(abscissa), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}

X11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
# Through generalized cross validation it appears an optimal number for the basis
# is 12
dev.off()

# Our b-spline basis with 12 functions
Xsp <- smooth.basis(abscissa, Xobs0, basis)
basis <- create.bspline.basis(range(abscissa), 12, m)
plot(basis)

# Now we fit the model via least squares
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, Xobs0, intercept=FALSE)$coef
est_coef

Xsp0 <- basismat %*% est_coef

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)

# b) Compute approximate pointwise confidence intervals at the sampling times of the data and provide a plot.

#### Approximate pointwise confidence intervals ####
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[mat*(mat'mat)^{-1}(mat)']
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
# We get the number of basis
sum(diag(S))

df <- Xsp$df   #  the degrees of freedom in the smoothing curve  
df    

sigmahat <- sqrt(sum((Xsp0-Xobs0)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))

x11()
plot(abscissa,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(abscissa,lb,type="l",col="blue",lty="dashed")
points(abscissa,ub,type="l",col="blue",lty="dashed")

# These are the confidence intervals
cbind(lower_b = as.numeric(lb), upper_b = as.numeric(ub))

# c) To study the currents induced by the tide, it is important to have a good estimate of the velocity of the tide.
# Compute an approximation of the first derivative of the curve from the data and the first derivative of the
# smoothed curve obtained at point (a). Provide a plot to compare the two and comment on the result.

# We fit the first derivative to study the velocity of the tide

basismat1 <- eval.basis(abscissa, basis,Lfdobj=1)
Xsp1bis <- basismat1 %*% lsfit(basismat, Xobs0, intercept=FALSE)$coef


x11()
par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="green",lwd=2)
abline(v=basisbis$params,lty=2)

basismat2 <- eval.basis(abscissa, basis,Lfdobj=2)
Xsp2bis <- basismat2 %*% lsfit(basismat, Xobs0, intercept=FALSE)$coef

x11(width = 14)
par(mfrow=c(1,2))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)


# We see that the first derivative reaches the peaks and the valleys of the
# differences plot (effect of the smoothing performed initially), 
# also as we would expect it is periodic. So the sea level variation
# is periodic. 

# d) Would you suggest the use of a different basis system? Why?
#   
  



# ???????????????????????????

