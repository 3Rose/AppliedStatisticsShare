library(sp)          
library(lattice)     
# library(geoR)         
library(gstat)  
library(nlme)

df = read.table('hotels.txt',head=TRUE)
head(df)

attach(df)

#POINT A
#MODEL WITH ONLY INTERCEPT
#1) estimate Kriging Stationary Model
coordinates(df) <- c('x','y')
v <- variogram(price ~ 1, df)
v.fit = fit.variogram(v, vgm(var(price), "Sph", 1000,nugget = 10))
plot(v,v.fit) #(X)
g.tr <- gstat(formula = price ~ 1, data = df, model = v.fit)
#2) take whatever location
loc = c(df$x[1], df$y[1]) 
#3) a0 = y at log.chlorofill=0 in whatever location
s0 = data.frame(x = loc[1] , y = loc[2])
coordinates(s0) = c('x','y')
a0 <- predict(g.tr, s0, BLUE = TRUE)
a0 <- a0$var1.pred
a0

