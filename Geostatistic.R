rm(list=ls())

library(sp)  
library(lattice)    
library(gstat)  

setwd("C:/Users/Michele/Desktop/Applied Statistics Labs/Applied Stats Code/Geostats")

## Functions for graphics 
v.f <- function(x, ...){100-cov.spatial(x, ...)}
v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}

## Estimating Spatial Correlation ##
##     Variogram Analysis         ##
##--------------------------------##
data(meuse) #observation for training
coordinates(meuse) <- c('x','y') #set coordinates

data(meuse.grid) #grid of entire zone
coordinates(meuse.grid) <- c('x','y')
meuse.grid <- as(meuse.grid, 'SpatialPixelsDataFrame')

data(meuse.riv) #river points
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
meuse.sr <- SpatialPolygons(meuse.lst)

v <- variogram(log(zinc) ~ 1, meuse)
plot(v,pch=19)

# list of parametric isotropic variogram models
vgm()

v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
v.fit
plot(v, v.fit, pch = 19)

# sum of squares error. final value of the minimum
attr(v.fit, 'SSErr')


##        Other Improvements      ##
##--------------------------------##
# how can we choose weights? argument fit.method in fit.variogram
# fit.method = 1 : w = N_j
# fit.method = 2 : w = N_j/gamma(h_j)^2
# fit.method = 6 : w = 1
# fit.method = 7 : w = N_j/h_j^2


## Stationary Univariate Spatial Prediction (Ordinary Kriging)
##-------------------------------------------------------------
g.tr <- gstat(formula = log(zinc) ~ 1, data = meuse, model = v.fit)
g.tr

#----------------
s0=data.frame(x=179180, y=330100) # query points
coordinates(s0)=c('x','y')

predict(g.tr, s0) #var.pred = predicted value // var.var = variance of error

#Estimate the mean (equal across location since stationarity)
predict(g.tr, s0, BLUE = TRUE) #this gives the estimate of the mean
predict(g.tr, meuse[1,], BLUE = TRUE) #equal

lz.ok <- predict(g.tr, meuse.grid, BLUE = FALSE) #prediction on the entire domain
spplot(lz.ok)

## Non-stationary Univariate Spatial Prediction (Universal Kriging)
##-----------------------------------------------------------------
#1111111111111111 fit the model from stationary variogram
meuse.gstat <- gstat(id = 'zinc', formula = log(zinc) ~ sqrt(dist),
                     data = meuse, nmax = 50, model=v.fit, set = list(gls=1))
meuse.gstat
#formula include regressors
#nmax=max iteration for iterative algo

#222222222222222 non stationary variogram estimated from gls residuals
v.gls<-variogram(meuse.gstat)

v.gls.fit <- fit.variogram(v.gls, vgm(1, "Sph", 800, 1))
plot(v.gls, v.gls.fit, pch = 19)

#333333333333333 get Universal Kriging Model
meuse.gstat <- gstat(id = 'zinc', formula = log(zinc) ~ sqrt(dist),
                     data = meuse, nmax = 50, model=v.gls.fit, set = list(gls=1))

#----------------

s0.vec <- as.vector(slot(s0,'coords')) #same query
# distance to the river: min(s0-each river point)
s0.dist <- min(rowSums(scale(meuse.riv,s0.vec)^2)) 
s0 <- as.data.frame(c(s0,s0.dist))
names(s0) <- c('x','y','dist')
coordinates(s0) <- c('x','y')
s0.new <- as(s0, 'SpatialPointsDataFrame')

predict(meuse.gstat, s0.new) #query

lz.uk <- predict(meuse.gstat, meuse.grid, BLUE=FALSE) #entire grid

lz.uk.BLUE <- predict(meuse.gstat, meuse.grid, BLUE=TRUE) #mean estimation, now makes sense

## Ordinary Kriging vs Universal Kriging
##-----------------------------------------------------------------
par(mfrow = c(1,1))
spplot(lz.ok[,1], main = 'Ordinary Kriging, gstat')
spplot(lz.uk[,1], main = 'Universal Kriging, gstat')
spplot(lz.uk.BLUE[,1], main = 'Universal Kriging, Mean, gstat')

## Relevance of the Mean Term: Mean vs Prediction, is mean important? Yes
##-----------------------------------------------------------------
plot(v$dist,v$gamma,xlab='distance',ylab='semivariance',pch=19,col='skyblue1',ylim=c(0,0.8))
points(v.gls$dist,v.gls$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue',ylim=c(0,0.8))

