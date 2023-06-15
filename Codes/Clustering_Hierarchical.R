setwd('C:/Users/Michele/Desktop/Codes')
df = read.table("pinnanobilis.txt", header = T)

head(df)

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

#POINT A HIERARCHICAL CLUSTERING
#clustering
d <- dist(df, method='euclidean')
clust <- hclust(d, method='complete')

x11() #identify best k qualitatively
par(mfrow=c(1,4))
plot(clust, main='euclidean-complete, k=2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust, k=2)
plot(clust, main='euclidean-complete, k=3', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust, k=3)
plot(clust, main='euclidean-complete, k=4', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust, k=4)
plot(clust, main='euclidean-complete, k=5', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clust, k=5)

#okkam razor + cluster 2 in k=2,3,4 / 3 in k=5 very small -> k=2

cluster <- cutree(clust, k=2) 
cluster

label <- rep(1, nrow(df))
label[cluster=='2'] <- 2
label