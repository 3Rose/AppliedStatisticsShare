#_______________________________________________________________________________
##### RIDGE REGRESSION
library(car)
library(glmnet)
rm(list = ls())

n          <- dim(cars)[[1]]
distance   <- cars$dist
speed1     <- cars$speed
speed2     <- cars$speed^2

fm <- lm(distance ~ speed1 + speed2)
summary(fm) 

# Fix lambda
lambda <- .5
fit.ridge <- lm.ridge(distance ~ speed1 + speed2, lambda = lambda)
# R automatically centers X and y wrt their mean.

coef.ridge <- coef(fit.ridge)
yhat.lm <- cbind(rep(1,n), speed1, speed2)%*%coef(fm)  # LM fitted values
yhat.r <- cbind(rep(1,n), speed1, speed2)%*%coef.ridge # ridge fitted values

#PLOT RESULTS
plot(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',xlab='Speed')
points(speed1, distance, pch=1, cex=.8)
matlines(speed1, yhat.r, type='l', lty=1,col=grey.colors(length(lambda)), lwd=2)
legend("topleft",c("lm","ridge"),lty=c(4,1),col=c("black",grey.colors(length(lambda))),lwd=2)

#_______________________________________________________________________________
##### CHOICE OF LAMBDA
lambda.c <- seq(0,10,0.01)
fit.ridge <- lm.ridge(distance ~ speed1 + speed2, lambda = lambda.c)

lambda.opt <- lambda.c[which.min(fit.ridge$GCV)]
lambda.opt

#PLOT RESULTS
x11()
plot(speed1, distance, pch=1, cex=.8, ylab='Distance',
     xlab='Speed')
matlines(speed1, yhat.r, type='l', lty=1,
         col=grey.colors(length(lambda.c)))
lines(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',
      xlab='Speed')
lines(speed1, yhat.r[,which.min(fit.ridge$GCV)], type='l', lty=1, lwd=2,
      col=2, ylab='Distance', xlab='Speed')
legend("topleft", c('LM', 'Ridge opt.' ), lty=c(4,1), col=c(1,2), lwd=2)

dev.off()

#_______________________________________________________________________________
##### LASSO REGRESSION

# Build the matrix of predictors
x <- model.matrix(distance~speed1+speed2)[,-1]
# Build the vector of response
y <- distance

lambda.grid <- 10^seq(5,-3,length=100)

#_______________________________________________________________________________
##### LASSO REGRESSION: CHOICE OF LAMBDA
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:3,]

yhat.lm <- cbind(rep(1,n), speed1, speed2)%*%coef(fm)  # LM fitted values
yhat.l <- cbind(rep(1,n), speed1, speed2)%*%coef.lasso # ridge fitted values

#PLOT RESULTS
plot(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',xlab='Speed')
points(speed1, distance, pch=1, cex=.8)
matlines(speed1, yhat.l, type='l', lty=1,col=grey.colors(length(lambda)), lwd=2)
legend("topleft",c("lm","lasso"),lty=c(4,1),col=c("black",grey.colors(length(lambda))),lwd=2)
