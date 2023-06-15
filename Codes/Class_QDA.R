df <- read.table('musicCountry.txt',header=TRUE)
head(df)

library(MASS)
#POINT A 16_06_22


gr <- factor(df$release.country, levels=c('Germany', 'US'))
g <- length(levels(gr))

idx1 <- which(gr == 'Germany')
idx2 <- which(gr == 'US')

df_G <- df[idx1,1:2]
df_U <- df[idx2,1:2]

library(mvnormtest)
#ASSUMPTIONS
#supervised, LDA or QDA? In any case X|group as gaussian 

mshapiro.test(t(df_G))$p
mshapiro.test(t(df_U))$p #acceptable at 5%

bartlett.test(df[,1:2],gr) #H0 homosk, not acceptable -> QDA

#CLASSIFICATION
pr = c(0.1 , 0.9) #prior probabilities
qda <- qda(df[,1:2], gr, prior = pr)
qda

qda$means

#PLOTS
x11()
#points of dataset
plot(df[,1:2], main='Partition', xlab='V1', ylab='V2', pch=20)
points(df[idx1,1:2], col='red', pch=20)   
points(df[idx2,1:2], col='blue', pch=20)
legend("topright", legend=levels(gr), fill=c('red','blue'), cex=.7)
#centers
points(qda$means, pch=4,col=c('red','blue') , lwd=2, cex=2)
#input points for classified regions
x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(price=x, average.length=y)
#classified regions
z  <- predict(qda, xy)$post 
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1] 
#plot regions
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
