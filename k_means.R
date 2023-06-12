

#_______________________________________________________________________________
##### K-means method

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)


Q <- cbind(quakes[,1:2], depth = -quakes[,3]/100)

d <- dist(Q) #D matrix euclidean distance

result.k <- kmeans(Q, centers=2) # Centers: fixed number of clusters

result.k$cluster      # labels of clusters
result.k$centers      # centers of the clusters
result.k$totss        # tot. sum of squares
result.k$withinss     # sum of squares within clusters
result.k$tot.withinss # sum(sum of squares within cluster)
result.k$betweenss    # sum of squares between clusters
result.k$size         # dimension of the clusters


#_______________________________________________________________________________
##### K-means method: CLUSTER VISUALIZATION
x11()
plot(Q, col = result.k$cluster+1)

open3d()
plot3d(Q, size=3, col=result.k$cluster+1, aspect = F) 
points3d(result.k$centers,size=10)

#_______________________________________________________________________________
##### K-means method: CHOICE OF k
### evaluate the variability between groups vs the variability withing

b <- NULL
w <- NULL
for(k in 1:10){
  
  result.k <- kmeans(Q, k)
  w <- c(w, sum(result.k$wit))
  b <- c(b, result.k$bet)
  
}

x11()
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)


