library(sp)          
library(lattice)     
# library(geoR)         
library(gstat)  
library(nlme)

#POINT B
df = read.table('hotels.txt',head=TRUE)
df <- data.frame(cbind(df,dummy_wint))
attach(df)

dummy_wint = rep(0,length(df[,1]))
dummy_wint[df$winter=='yes'] <- 1
coordinates(df) <- c('x','y')

v <- variogram(price ~ distance+dummy_wint+dummy_wint:distance, df)
v.fit = fit.variogram(v, vgm(var(price), "Sph", 1000,1))                 
plot(v,v.fit)

g.tr <- gstat(formula = price ~ distance+dummy_wint+dummy_wint:distance, 
              data = df, model = v.fit)
#2) take whatever location
loc = c(df$x[1], df$y[1]) 
#3) a0 = y at log.chlorofill=0 in whatever location
s0 = data.frame(x = loc[1] , y = loc[2],distance=0,dummy_wint=0)
coordinates(s0) = c('x','y')
a0 <- predict(g.tr, s0, BLUE = TRUE)
a0_n <- a0$var1.pred
a0_n #a0_no_winter = 220.5

s0 = data.frame(x = loc[1] , y = loc[2],distance=0,dummy_wint=1)
coordinates(s0) = c('x','y')
a0 <- predict(g.tr, s0, BLUE = TRUE)
a0_w <- a0$var1.pred
a0_w
#a0_winter = 447.3

s0 = data.frame(x = loc[1] , y = loc[2],distance=1,dummy_wint=0)
coordinates(s0) = c('x','y')
a1 <- predict(g.tr, s0, BLUE = TRUE)
a1_n <- a1$var1.pred - a0_n
a1_n #a1_no_winter = -0.009

s0 = data.frame(x = loc[1] , y = loc[2],distance=1,dummy_wint=1)
coordinates(s0) = c('x','y')
a1 <- predict(g.tr, s0, BLUE = TRUE)
a1_w <- a1$var1.pred - a0_w
a1_w #a1_winter = -0.111

#POINT C
#method b
loc = data.frame(cbind(x=342399.74,y=5072272.75)) 
x0 = data.frame(cbind(x=342362.58,y=5072518.24)) 
d = dist(rbind(loc,x0),method='euclidean')
d = d[1]
s0 = data.frame(x = loc[1] , y = loc[2],distance=d,dummy_wint=1)
coordinates(s0) = c('x','y')
pred <- predict(g.tr, s0, BLUE = TRUE)
pred <- pred$var1.pred
pred #430.03

