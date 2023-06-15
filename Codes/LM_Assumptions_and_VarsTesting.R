#POINT B 19_01_22
library(car)
df = read.table('tattoo.txt',head=TRUE)
head(df)

attach(df)

n <- nrow(df)
dummy_hand = rep(0,n)
dummy_hand[which(method == 'handmade')] = 1

fit <- lm(price ~ dimension + ncolors + dummy_hand + dimension:dummy_hand + ncolors:dummy_hand, data = df)
#assumptions
shapiro.test(fit$residuals) #ok

l1 = lm(df$price[df$method=='handmade'] ~ df$dimension[df$method=='handmade'] + 
          df$ncolors[df$method=='handmade'] )
l2 = lm(df$price[df$method=='machine'] ~ df$dimension[df$method=='machine'] + 
          df$ncolors[df$method=='machine'] )

var.test(l1$residuals,l2$residuals) #ok only at 1% -> I accept it

#test
C = rbind(c(0,0,0,1,0,0), c(0,0,0,0,1,0), c(0,0,0,0,0,1)) #dummy and interactions
mu0 = c(0,0,0)
linearHypothesis(fit, C, mu0) #significant

C = rbind(c(0,0,1,0,0,0), c(0,0,0,0,0,1)) #color and its interaction
mu0 = c(0,0)
linearHypothesis(fit, C, mu0) #significant
