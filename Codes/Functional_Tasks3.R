# Load package fda
library(fda)
df = read.table('power.txt',head=TRUE)
head(df)

df <- df$power
abscissa <- 1:365
N <- length(abscissa)

#POINT A
nbasis <- 7:50
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(range(abscissa), nbasis[i])
  gcv[i] <- smooth.basis(abscissa, df, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

basis <- create.fourier.basis(range(abscissa),nbasis = 12)
plot(basis)

# Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)

# Fit via LS
est_coef = lsfit(basismat, df, intercept=FALSE)$coef

# Smooth Curve
sm <- basismat %*% est_coef

par(mfrow=c(1,1))
plot(abscissa,df,xlab="t",ylab="observed data")
points(abscissa,sm ,type="l",col="blue",lwd=2)

#POINT B
der1_act <- (df[3:N]-df[1:(N-2)])/(abscissa[3:N]-abscissa[1:(N-2)])

basismat1<- eval.basis(abscissa, basis, Lfdobj=1)
sm_der1 <- basismat1 %*% est_coef

x11()
par(mfrow=c(1,1))
plot(1:363,der1_act,xlab="t",ylab="observed data",type='l')
points(1:365,sm_der1,type="l",col="blue",lwd=2)

#POINT C
n_und <- 6 #not so much, do not want to show absurde curves

basis_und <- create.fourier.basis(range(abscissa),nbasis = 6)
basismat_und <- eval.basis(abscissa, basis_und)
est_coef_und = lsfit(basismat_und, df, intercept=FALSE)$coef
sm_und <- basismat_und %*% est_coef_und

par(mfrow=c(1,1))
plot(abscissa,df,xlab="t",ylab="observed data")
points(abscissa,sm ,type="l",col="blue",lwd=2)
points(abscissa,sm_und ,type="l",col="red",lwd=2)

#POINT D
n_over <- 24 #not so much, do not want to show absurde curves

basis_over <- create.fourier.basis(range(abscissa),nbasis = 24)
basismat_over <- eval.basis(abscissa, basis_over)
est_coef_over = lsfit(basismat_over, df, intercept=FALSE)$coef
sm_over <- basismat_over %*% est_coef_over

par(mfrow=c(1,1))
plot(abscissa,df,xlab="t",ylab="observed data")
points(abscissa,sm ,type="l",col="blue",lwd=2)
points(abscissa,sm_over ,type="l",col="red",lwd=2)

