library(sp)          
library(lattice)     
# library(geoR)         
library(gstat)  
library(nlme)
df = read.table('walesharks.txt',head=TRUE)
head(df)

df$sights <- log(df$sights)
attach(df)

#POINT A
#1) estimate Kriging Non-Stationary Model
coordinates(df) <- c('x','y')
v <- variogram(sights ~ log.chlorofill, df)
v.fit = fit.variogram(v, vgm(0.5 , "Exp", 110000))   
g.tr <- gstat(formula = sights ~ log.chlorofill, data = df, model = v.fit)
#2) take whatever location
loc = c(df$x[1], df$y[1]) 
#3) a0 = y at log.chlorofill=0 in whatever location
s0 = data.frame(x = loc[1] , y = loc[2], log.chlorofill=0)
coordinates(s0) = c('x','y')
a0 <- predict(g.tr, s0, BLUE = TRUE)
a0 <- a0$var1.pred
a0
#4) a1 = y -a0 at log.chlorofill=1 in whatever location
s0 = data.frame(x = loc[1] , y = loc[2], log.chlorofill=1)
coordinates(s0) = c('x','y')
a1 <- predict(g.tr, s0, BLUE = TRUE)
a1 <- a1$var1.pred - a0
a1
#5) residuals 
res <- sights-a0-a1
head(res)

