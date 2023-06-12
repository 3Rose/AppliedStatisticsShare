setwd("C:/Users/Michele/Desktop/Applied Statistics Labs/Applied Stats Code/Functional")


noisycurve <- read.table("noisycurvebis.txt",header=T)
Xobs0 <- noisycurve$X0
abscissa <- noisycurve$Abscissa
NT <- length(abscissa) 


#### REGRESSION SPLINES METHOD 1 ####
# Load package fda
library(fda)

# Set parameters
m <- 5           # spline order 
degree <- m-1    # spline degree 

nbasis <- 9

# Create the basis
basis <- create.bspline.basis(rangeval=c(0,1), nbasis=nbasis, norder=m)
# If breaks are not provided, equally spaced knots are created
plot(basis)

# Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)
dim(basismat) # number of data x number of basis

# Fit via LS
est_coef = lsfit(basismat, Xobs0, intercept=FALSE)$coef

# Smooth Curve
Xsp0 <- basismat %*% est_coef

# to obtain the first derivative (argument Lfdobj=1)
basismat1<- eval.basis(abscissa, basis, Lfdobj=1)
Xsp1 <- basismat1 %*% est_coef

# to obtain the second derivative (argument Lfdobj=2)
basismat2<- eval.basis(abscissa, basis, Lfdobj=2)
Xsp2 <- basismat2 %*% est_coef

#PLOTS
par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
legend("topleft", legend = c("noisy data","estimated curve"), col = c("black", "orange","blue"), lwd = c(1,3,2))
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)
points(abscissa,Xsp2 ,type="l",col="blue",lwd=2)

dev.off()

#### REGRESSION SPLINES METHOD 2 ####
Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative
Xsp2bis <- eval.fd(abscissa, Xsp$fd, Lfd=2) # second derivative


#### OPTIMAL NUMBER OF BASIS ####
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,1), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)


#### OPTIMAL LAMBDA ####
lambda <- 10^seq(-12,-5,by = 0.5)
gcv <- numeric(length(lambda))
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=lambda[i])  
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}
par(mfrow=c(1,1))
plot(log10(lambda),gcv)
lambda[which.min(gcv)]

functionalParbest <- fdPar(fdobj=basis, Lfdobj=3, lambda=lambda[which.min(gcv)])  

Xssbest <- smooth.basis(abscissa, Xobs0, functionalParbest)
Xss0best <- eval.fd(abscissa, Xssbest$fd, Lfd=0)
Xss1best <- eval.fd(abscissa, Xssbest$fd, Lfd=1)
Xss2best <- eval.fd(abscissa, Xssbest$fd, Lfd=2)

gcvbest <- Xssbest$gcv  #  the value of the gcv statistic
gcvbest

par(mfrow=c(1,2))
plot(abscissa,Xobs0,xlab="t",ylab="Optimal Lambda")
points(abscissa,Xss0best ,type="l",col="blue",lwd=2)
plot(abscissa,Xobs0,xlab="t",ylab="Non_Optimal")
points(abscissa,Xsp0bis ,type="l",col="blue",lwd=2)


