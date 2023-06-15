
#MANOVA
fit <- manova(as.matrix(df[,1:2])~as.matrix(df[,3]))
fit
summary(fit) #yes, membership has effect

fit$coefficients
fit$effects
fit$residuals

#MANOVA ASSUMPTIONS
g1 <- df[df$V3=='1',1:2]
g2 <- df[df$V3=='2',1:2]
library(mvnormtest)
mshapiro.test(t(g1))
mshapiro.test(t(g2)) #accept H0 of gaussianity

S1 = cor(g1)
S2 = cor(g2)

x11()
par(mfrow=c(1,g))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))


#BONFERRONI 4 DIFFERENCE BTWN MEANS
alpha <- 0.1
n1 <- length(g1[,1])
n2 <- length(g2[,1])
g <- 2
p <- length(g1[1,])

Spool <- ((n1-1)*cov(g1)+(n2-1)*cov(g2))/(n1+n2-2) 
S <- cov(df[,1:2])
mean_a <- sapply(g1,mean)
mean_p <- sapply(g2,mean)

a <- c(1,-1) #differences btwn means
k <- length(a) #number of lin.comb.
inf <- mean_a-mean_p - sqrt(t(a)%*%Spool%*%a*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2)
sup <- mean_a-mean_p + sqrt(t(a)%*%Spool%*%a*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2)

CI <- list(g12=cbind(inf, mean_a-mean_p,sup))
CI