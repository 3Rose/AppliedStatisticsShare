library(MASS)
library(car)
library(rgl)


n          <- dim(cars)[[1]]
distance   <- cars$dist
speed1     <- cars$speed
speed2     <- cars$speed^2

fm <- lm(distance ~ speed1 + speed2)
summary(fm) 

#_______________________________________________________________________________
##### Colinearity Detection

vif(fm) # Problem when VIF exceeds 10 (or 5 sometimes)

#_______________________________________________________________________________
##### PCA REGRESSION: REMOVE COLLINEARITY
speed.pc <- princomp(cbind(speed1,speed2), scores=TRUE)
summary(speed.pc)

speed.pc$load #DISTRIBUTER LOADINGS ACROSS PCs

sp1.pc <- speed.pc$scores[,1] #points in new space, orthogonal axes!!
sp2.pc <- speed.pc$scores[,2]

fm.pc <- lm(distance ~ sp1.pc + sp2.pc)

summary(fm.pc) 
vif(fm.pc)

#_______________________________________________________________________________
##### PCA REGRESSION: DIMENSIONALITY REDUCTION
fm.pc <- lm(distance ~ sp1.pc)
summary(fm.pc) 
