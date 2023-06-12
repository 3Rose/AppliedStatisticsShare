
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



#_______________________________________________________________________________
##### MANOVA: FRAMEWORK
##### (p=4, g=3)

### Variables: Sepal and Petal Length and Sepal and Petal Width of iris
### Groups: species (setosa, versicolor, virginica; g = 3)

attach(iris)

species.name <- factor(Species, labels=c('setosa','versicolor','virginica'))
iris4        <- iris[,1:4] # variables

### Data exploration
i1 <- which(species.name=='setosa')
i2 <- which(species.name=='versicolor')
i3 <- which(species.name=='virginica')

x11()
par(mfrow=c(1,3))
boxplot(iris4[i1,], main='SETOSA',     ylim=c(0,8), col = rainbow(4))
boxplot(iris4[i2,], main='VERSICOLOR', ylim=c(0,8), col = rainbow(4))
boxplot(iris4[i3,], main='VIRGINICA',  ylim=c(0,8), col = rainbow(4))

x11()
par(mfrow=c(1,4))
boxplot(iris4[,1]~species.name, main='Sepal Length', ylim=c(0,8), col = rainbow(3))
boxplot(iris4[,2]~species.name, main='Sepal Width', ylim=c(0,8), col = rainbow(3))
boxplot(iris4[,3]~species.name, main='Petal Length', ylim=c(0,8), col = rainbow(3))
boxplot(iris4[,4]~species.name, main='Petal Width', ylim=c(0,8), col = rainbow(3))

graphics.off()

### Model: X.ij = mu + tau.i + eps.ij
### H0: tau.1 = tau.2 = tau.3  = (0,0,0,0)', no effect
### H1: at least one tau has effect
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1 + n2 + n3

g  <- length(levels(species.name))
p  <- 4

#_______________________________________________________________________________
##### MANOVA: ASSUMPTIONS
# 1)  normality 
Ps <- NULL
for(i in 1:g) {
  mvn.test <- mvn(data = iris[get(paste('i',i, sep='')), 1:4])
  Ps <- c(Ps, mvn.test$multivariateNormality$`p value`)
}
Ps

# 2) same covariance structure 
# Box's M test for homogeneity of covariance 
# Should be done only if n_i > ~20 for all i, p < 5 and g < 5
# H0: same covariance
summary(boxM(iris4, species.name))
# Other way to compare: heatmap on the covariance matrices
x11()
par(mfrow=c(1,3))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))

dev.off()

#_______________________________________________________________________________
##### MANOVA: GO
fit <- manova(as.matrix(iris4) ~ species.name)
summary.manova(fit, test="Wilks")
# Exact tests for p<=2 or g<=3 already implemented in R

### Reject the test, species could have effect


#_______________________________________________________________________________
##### MANOVA: CHECK SINGULAR GROUP Via ANOVA: 
summary.aov(fit)

# Each of the 4 variables is significantly influenced by the  
# factor species.
# Note: this analysis does NOT say: 
#       a) which group differ
#       b) with respect to which variables the groups in (a) differ
# => As for the ANOVA, we build confidence intervals 

#_______________________________________________________________________________
##### MANOVA: CHECK WITH BONFERRONI: 
alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(iris4, mean)         # estimates mu
m1 <- sapply(iris4[i1,], mean)    # estimates mu.1 = mu + tau.1
m2 <- sapply(iris4[i2,], mean)    # estimates mu.2 = mu + tau.2
m3 <- sapply(iris4[i3,], mean)    # estimates mu.3 = mu + tau.3

inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
inf13 <- m1 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
sup13 <- m1 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
inf23 <- m2 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))
sup23 <- m2 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))

CI <- list(setosa_versicolor    = cbind(inf12, sup12),
           setosa_virginica     = cbind(inf13, sup13),
           versicolor_virginica = cbind(inf23, sup23))

x11()
par(mfrow=c(2,4))
boxplot(iris4[,1]~species.name, main='Sepal Length', ylim=c(0,8), col = rainbow(3))
boxplot(iris4[,2]~species.name, main='Sepal Width', ylim=c(0,8), col = rainbow(3))
boxplot(iris4[,3]~species.name, main='Petal Length', ylim=c(0,8), col = rainbow(3))
boxplot(iris4[,4]~species.name, main='Petal Width', ylim=c(0,8), col = rainbow(3))

mg <- rbind(m1,m2,m3)
sp.name <- c('Sepal Length','Sepal Width', 'Petal Length', 'Petal Width')
for(k in 1:4){
  plot(c(1,g*(g-1)/2),ylim=c(-4,4), xlim=c(1,3), pch='', 
       xlab='pairs treat', ylab=paste('CI tau',k), 
       main=paste('CI tau',sp.name[k]))
  lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
  points(1, mg[1,k]-mg[2,k], pch=16); 
  points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
  points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);  
  lines (c(2,2), c(CI[[2]][k,1],CI[[2]][k,2])); 
  points(2, mg[1,k]-mg[3,k], pch=16);
  points(2, CI[[2]][k,1], col=rainbow(g)[3], pch=16); 
  points(2, CI[[2]][k,2], col=rainbow(g)[1], pch=16);
  lines (c(3,3), c(CI[[3]][k,1],CI[[3]][k,2])); 
  points(3, mg[2,k]-mg[3,k], pch=16);
  points(3, CI[[3]][k,1], col=rainbow(g)[3], pch=16); 
  points(3, CI[[3]][k,2], col=rainbow(g)[2], pch=16);  
  abline(h=0)
}

dev.off()