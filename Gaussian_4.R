library(MVN) # Can need separate installation of "gsl" software library
library(car)
library(mvtnorm)
library(mvnormtest)

setwd("C:/Users/Michele/Desktop/Applied Statistics Labs/Applied Stats Code/Multivar Gaussian and Inference on the Mean")

#_______________________________________________________________________________
### TEST FOR MULTIVARIATE NORMALITY
mu <- c(1,2)
sig <- matrix(c(1,1,1,2), 2)
n   <-  150

set.seed(20230320)
X <- rmvnorm(n, mu, sig)

#_______________________________________________________________________________
### SHAPIRO MULTIV
# Multivariate version of the Shapiro-Wilk test / H0: NORMALITY
# To be used with sample sizes *between 3 and 5000*
# Takes a matrix where each observation is a column
mshapiro.test(t(X)) # Don't forget to transpose your matrix before

#_______________________________________________________________________________
### GRAPHICAL
X11()
par(mfrow=c(1,2))
qqnorm(X[,1], main='QQplot of X.1',xlab='theoretical quantiles', ylab='sample quantiles')
qqline(X[,1])
qqnorm(X[,2], main='QQplot of X.2',xlab='theoretical quantiles', ylab='sample quantiles')
qqline(X[,2])

#_______________________________________________________________________________
### OTHER TESTS
# Henze-Zirkler’s test
result <- mvn(data = X, mvnTest = "hz") # Test performed by default
result$multivariateNormality

# Royston’s test (Shapiro-Wilk test under the hood)
result <- mvn(data = X, mvnTest = "royston")
result$multivariateNormality

# with Q-Q plot of the squared mahalanobis distance over chi-square
X11()
result <- mvn(data = X, mvnTest = "royston", multivariatePlot = "qq")
result$multivariateNormality

#_______________________________________________________________________________
### EXAMPLE
stiff <- read.table('stiff.dat')

# univariate for the components
X11()
par(mfcol=c(2,4))

for(i in 1:4)
{
  hist(stiff[,i], prob=T, main=paste('Histogram of V', i, sep=''), xlab=paste('V', i, sep=''))
  lines(900:2800, dnorm(900:2800,mean(stiff[,i]),sd(stiff[,i])), col='blue', lty=2)
  qqnorm(stiff[,i], main=paste('QQplot of V', i, sep=''))
  qqline(stiff[,i])
  print(shapiro.test(stiff[,i])$p)
}

# multivariate
X11()
result <- mvn(data = stiff, multivariatePlot = "qq", covariance = FALSE)
result$multivariateNormality

### The data don't seem Gaussian. What can we do?
#_______________________________________________________________________________
### OUTLIER AND OUTLIER REMOVAL

### Let's try to identify and remove outliers:
M <- colMeans(stiff)
S <- cov(stiff)

d2 <- matrix(mahalanobis(stiff, M, S))

stiff_wo_outliers <- stiff[which(d2 <= 8), ]

result <- mvn(data = stiff_wo_outliers)
result$multivariateNormality

X11()
plot(stiff_wo_outliers, asp=1, pch=19)

# Normality of the components
X11()
par(mfcol=c(2,4))

for(i in 1:4)
{
  hist(stiff_wo_outliers[,i], prob=T, main=paste('Histogram of V', i, sep=''), xlab=paste('V', i, sep=''))
  lines(900:2800, dnorm(900:2800,mean(stiff_wo_outliers[,i]),sd(stiff_wo_outliers[,i])), col='blue', lty=2)
  qqnorm(stiff_wo_outliers[,i], main=paste('QQplot of V', i, sep=''))
  qqline(stiff_wo_outliers[,i])
  print(shapiro.test(stiff_wo_outliers[,i])$p)
}

#_______________________________________________________________________________
### BOX COX TRANSFORMATIONS

# BOX COX tranformation function
box_cox <- function(x,lambda)
{
  if(lambda!=0)
    return((x^lambda-1)/lambda)
  return(log(x))
}

x<-seq(0, 25, by=0.01)

X11()
plot(x, box_cox(x,0), col='gold', type='l', xlab='x', ylab=expression(x[lambda]),
     ylim=c(-20,30), xlim=c(-5,25), asp=1)
title(main='Box-Cox transformations')
curve(box_cox(x,-1),from=0,to=25,col='blue',add=TRUE)
curve(box_cox(x,1),from=0,to=25,col='red',add=TRUE)
curve(box_cox(x,2),from=0,to=25,col='springgreen',add=TRUE)
points(1,0,pch=19,col='black')
abline(v=0,lty=2,col='grey')
legend('topright',c(expression(paste(lambda,'=-1')),expression(paste(lambda,'=0')),
                    expression(paste(lambda,'=1')),expression(paste(lambda,'=2'))),
       col=c('blue','gold','red','springgreen'),lty=c(1,1,1,1,1))

# For lambda<1: observations <1 are "spread", observations >1 are "shrinked"
# For lambda>1: observations <1 are "shrinked", observations >1 are "spread"

graphics.off()



#_______________________________________________________________________________
### BOX COX TRANSFORMATIONS EXAMPLE
rm(list = ls())

b <- read.table('data_sim.txt')
mshapiro.test(t(b)) #not gaussian

### Bivariate Box-Cox transformation
lambda <- powerTransform(cbind(x,y)) #optimal lambda 
lambda #lambda[1] close to 1, lambda[2] close to 0, it is almost a log transform; 
 

# Compute the transformed data with optimal lambda (of the bivariate transf.)
# (command bcPower)
BC.x <- bcPower(x, lambda$lambda[1])
BC.y <- bcPower(y, lambda$lambda[2])

# Let's formulate an hypothesis of transformation:
# since we get lambda[1]~1 and lambda[2]~0, we could reasonably consider:
hyp.x <- x
hyp.y <- log(y)

# Univariate Shapiro-Wilk
shapiro.test(x)$p
shapiro.test(BC.x)$p
shapiro.test(hyp.x)$p

shapiro.test(y)$p
shapiro.test(BC.y)$p
shapiro.test(hyp.y)$p

# MC-Shapiro test (H0: multivariate gaussianity)
mshapiro.test(rbind(x,y))$p
mshapiro.test(rbind(BC.x,BC.y))$p
mshapiro.test(rbind(hyp.x,hyp.y))$p

