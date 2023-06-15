library(rgl)
df = read.table('beans.txt',head=TRUE)
head(df)

df_num <- df[,1:8]
head(df_num)

#POINT A: PCA, SCALED OR NOT?
pc <- princomp(df_num, scores=T)
pc #see the PCs
summary(pc) #see the PCs + Cumulative

#lot of vars do not take place in PCs 
# + unbalanced PCs (first cover 99.9%) -> scale

pc.s <- princomp(scale(df_num), scores=T)
pc.s 
summary(pc.s) #way more balanced

#POINT B: PCs and ROTATED DATASET
#1)
load <- pc.s$loadings
load 

x11()
par(mfrow = c(3,1)) #loads first 3 PCs
for(i in 1:3) barplot(load[,i], ylim = c(-1, 1))

#2)
#plot data in rotated space (scores) wrt variety
scores.dataset <- pc.s$scores


x11()
plot(scores.dataset[which(df$Type == 'cannellini'),1], 
     scores.dataset[which(df$Type == 'cannellini'),2],col= 'red', xlim=c(-5,5))
points(scores.dataset[which(df$Type == 'adzuki'),1:2], col='green', xlim=c(-5,5))
points(scores.dataset[which(df$Type == 'black-eyed'),1:2], col='blue', xlim=c(-5,5))

#POINT C: MEAN OF ROTATED DATASET IN 2D
df2 = data.frame(scores.dataset[which(df$Type == 'cannellini'),1:2])

n <- length(df2[,1])
p <- length(df2[1,])

x.mean <- colMeans(df2) #center
x.cov <- cov(df2)
eigen(x.cov/n)$vectors #directions

alpha = 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values) 

library(car)
x11()
plot(df2, asp = 1)
ellipse(x.mean, x.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)
