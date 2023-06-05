power <- read.table("power.txt", sep =" ", head = T, stringsAsFactors = T)
power

NT <- dim(power)[1]
abscissa <- 1:365
Xobs0 <- power$power

# Seems to be periodic
# We plot the first and second derivative
dev.off()
plot(abscissa,Xobs0, type = "l")
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])

nbasis <- 1:100
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(range(abscissa), nbasis[i])
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
X11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
