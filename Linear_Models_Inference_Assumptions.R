#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression

library(MASS)
library(car)
library(rgl)

distance   <- cars$dist
speed1     <- cars$speed
speed2     <- cars$speed^2

fm <- lm(distance ~ speed1 + speed2)
summary(fm) 

#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression: REMARKABLE AMOUNTS

fitted(fm)        # y hat
residuals(fm)     # eps hat

coefficients(fm)  # beta_i
vcov(fm)          # cov(beta_i)

rstandard(fm) # standardized residuals: eps_j / sqrt(s^2*(1-h_ii))

sum(residuals(fm)^2)/fm$df  # s^2 estimate of sigma^2

#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression: INFERENCE FOR BETA

# H0: (beta1, beta2) == (0, 0) vs H1: (beta1, beta2) != (0, 0)
linearHypothesis(fm, rbind(c(0,1,0), c(0,0,1)), c(0,0))
# restricted model: H0 true
# low p-value, H0 to be rejected, at least one of params are useful

#CONFIDENCE REGION
p <- 2  # number of tested coefficients
r <- 2  # number of regressors

# center: point estimate
c(coefficients(fm)[2], coefficients(fm)[3])
# Direction of the axes?
eigen(vcov(fm)[2:3,2:3])$vectors

plot(coefficients(fm)[2], coefficients(fm)[3], xlim = c(-6,6), ylim = c(-6,6), asp=1, xlab='beta1', ylab='beta2')
ellipse(coefficients(fm)[2:3], vcov(fm)[2:3,2:3], sqrt(p*qf(1-0.05,p,n-(r+1))))
abline(v=0)
abline(h=0)

#BONFERRONI INTERVALS (level 95%)
Bf <- rbind(
  beta1=c(coefficients(fm)[2]-sqrt(vcov(fm)[2,2])*qt(1-0.05/(2*p), n-(r+1)),
          coefficients(fm)[2]+sqrt(vcov(fm)[2,2])*qt(1-0.05/(2*p), n-(r+1))),
  beta2=c(coefficients(fm)[3]-sqrt(vcov(fm)[3,3])*qt(1-0.05/(2*p), n-(r+1)),
          coefficients(fm)[3]+sqrt(vcov(fm)[3,3])*qt(1-0.05/(2*p), n-(r+1)))
)
Bf

#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression: CORRECTION
fm <- lm(distance ~ speed2)
summary(fm) #OK

#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression: ASSUMPTIONS
#Parameter estimation: 1) E(Eps) = 0  and  2) Var(Eps) = sigma^2 
hist(fm$residuals) 
mean(fm$residuals) #1) ok

plot(fm$residuals) #2) seems constant variance

## 3) gaussianity Eps ~ N(0, sigma^2)
shapiro.test(fm$residuals) #H0: gaussianity, so not gaussian

#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression: LOG TRANSFORM
distance_L   <- log(cars$dist)
speed2_L     <- log(cars$speed^2)

fm_L <- lm(distance_L ~ speed2_L)
summary(fm_L) #OK

hist(fm_L$residuals)
shapiro.test(fm_L$residuals) #H0: gaussianity, so gaussian
#______________________________________________________________________________________________________________________________________________________________
##### Linear Regression: PREDICT
Z0.new <- data.frame(speed1=10, speed2=10^2) #new data

# Conf. int. for the mean
Conf <- predict(fm, Z0.new, interval='confidence', level=1-0.05)  
Conf
# Pred. int. for a new obs
Pred <- predict(fm, Z0.new, interval='prediction', level=1-0.05)  
Pred

