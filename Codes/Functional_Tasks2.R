library(MASS)
library(car)
library(rgl)
library(fda)

df <- read.table('listening.txt',header=TRUE)

#POINT A
#constant data
abscissa <- 1:365
N <- length(abscissa)
m <- 3           # spline order 
degree <- m-1    # spline degree 
bre <- abscissa

basis <- create.bspline.basis(rangeval=range(abscissa), norder = m, breaks = bre)
data.fd <- Data2fd(y = t(df), argvals = abscissa, basisobj = basis, lambda=100) #curves as column!!!

plot(data.fd)

coef <- data.fd$coefs
coef[1:3,1] #required coef

#POINT B
pca <- pca.fd(data.fd,nharm=10,centerfns=TRUE)

pca$varprop #proportion of variance
sum(pca$varprop) #together 99.92%

x11() #screeplot
plot(cumsum(pca$values)/sum(pca$values),xlab='j',ylab='CPV',xlim=c(0,100))

#POINT C
pca$values/sum(pca$values) #i would exclude eigenv. < 5%
dim <- which(pca$values/sum(pca$values)<0.05)
dim <- dim[1]

par(mfrow=c(1,dim))
for(i in 1:dim){
  plot(pca$harmonics[i,],col=1)  
}

#POINT D
par(mfrow=c(1,dim))
mean <- mean.fd(data.fd)
for(i in 1:dim){
  plot(mean,lwd=2)
  lines(mean+pca$harmonics[i,]*sqrt(pca$values[i]), col=2)
  lines(mean-pca$harmonics[i,]*sqrt(pca$values[i]), col=3)  
}

#POINT E
par(mfrow=c(1,1))
plot(pca$scores[,1],pca$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
