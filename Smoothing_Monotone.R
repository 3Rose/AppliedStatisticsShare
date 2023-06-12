names(growth)
matplot(growth$age, growth$hgtf, type = "l")

# If we neglect considering that the curves must be monotone we have decreasing height
#### SMOOTHING for monotone curves ####
#---------------------------------------
age <- growth$age
nage <- length(age)
ageRng <- range(age)

# Let's consider only the first 5 girls 
hgtf <- growth$hgtf[,1:5]
ncasef <- dim(hgtf)[2]

#BASIS
norder <- 6
nbasis <- nage - 2 + norder 
wbasis <- create.bspline.basis(rangeval = ageRng, nbasis = nbasis, 
                               norder = norder, breaks = age)

#PENALIZE 3RD ORDER DERIVATIVE
Lfdobj <- 3          
lambda <- 10^(-0.5)  
cvecf <- matrix(0, nbasis, ncasef)
Wfd0 <- fd(coef = cvecf, basisobj = wbasis)
growfdPar <- fdPar(fdobj = Wfd0, Lfdobj = Lfdobj, lambda = lambda)

#MODEL FOR MONOTONE FUNCTIONS
growthMon <- smooth.monotone(argvals = age, y = hgtf, WfdParobj = growfdPar)


hgtfhatfd <- growthMon$yhatfd #ANDAMENTO ALTEZZA
velocfdUN <- deriv.fd(expr = hgtfhatfd, Lfdobj = 1) #VELOCITA' ALTEZZA
accelfdUN <- deriv.fd(expr = hgtfhatfd, Lfdobj = 2) #ACCELERAZIONE ALTEZZA


#PLOT
par(mfrow=c(2,2),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)
plot(hgtfhatfd, xlim=c(1,18), lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Growth (cm)") #OK,MONOTONE
plot(velocfdUN, xlim=c(1,18),  lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Velocity (cm/yr)")
plot(accelfdUN, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Acceleration (cm/yr/yr)")
