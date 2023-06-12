### Hierarchical clustering

library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

#_______________________________________________________________________________
##### Hierarchical Clustering

species.name <- iris[,5]
iris4        <- iris[,1:4] #cut the labels

misc <- sample(150)
iris4 <- iris4[misc,] #permutation, the dataset is ordered in this case

iris.e <- dist(iris4, method='euclidean') #even with method=manhattan, canberra


iris.es <- hclust(iris.e, method='single')
iris.ea <- hclust(iris.e, method='average')
iris.ec <- hclust(iris.e, method='complete')

#_______________________________________________________________________________
##### Hierarchical Clustering: DENDOGRAM
iris.ec$height # distance at which we have aggregations


#DENDOGRAM: AGGREGATION STEPS
x11()
par(mfrow=c(1,3))
plot(iris.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(iris.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(iris.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

dev.off()

#DENDOGRAM: 2 CLUSTERS WITH rect.hclust(iris.es, k=2)
x11()
par(mfrow=c(1,3))
plot(iris.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.es, k=2)
plot(iris.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.ec, k=2)
plot(iris.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.ea, k=2)

dev.off()

#_______________________________________________________________________________
##### Hierarchical Clustering: THE CUT

#2 CLUSTERS:
cluster.ec <- cutree(iris.ec, k=2) # euclidean-complete:
cluster.ec

cluster.es <- cutree(iris.es, k=2) 
cluster.ea <- cutree(iris.ea, k=2) 

#_______________________________________________________________________________
##### Hierarchical Clustering: INTERPRETATION

table(label.true = species.name[misc], label.cluster = cluster.es)
table(label.true = species.name[misc], label.cluster = cluster.ec)
table(label.true = species.name[misc], label.cluster = cluster.ea)

x11()
plot(iris4, col=ifelse(cluster.es==1,'red','blue'), pch=19)
x11()
plot(iris4, col=ifelse(cluster.ec==1,'red','blue'), pch=19)
x11()
plot(iris4, col=ifelse(cluster.ea==1,'red','blue'), pch=19)

graphics.off()

#_______________________________________________________________________________
##### Hierarchical Clustering: COPHENETIC MATRIX
coph.es <- cophenetic(iris.es)
coph.ec <- cophenetic(iris.ec)
coph.ea <- cophenetic(iris.ea)

# compare with dissimilarity matrix (Euclidean distance)
x11()
layout(rbind(c(0,1,0),c(2,3,4)))
image(as.matrix(iris.e), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='Single', asp=1 )
image(as.matrix(coph.ec), main='Complete', asp=1 )
image(as.matrix(coph.ea), main='Average', asp=1 )

dev.off()

#_______________________________________________________________________________
##### Hierarchical Clustering: COPHENETIC COEFFICIENTS
es <- cor(iris.e, coph.es)
ec <- cor(iris.e, coph.ec)
ea <- cor(iris.e, coph.ea)

c("Eucl-Single"=es,"Eucl-Compl."=ec,"Eucl-Ave."=ea) #near 1 = good

#_______________________________________________________________________________
##### Example 3: earthquakes dataset 
#####-------------------------------
Q <- cbind(quakes[,1:2], depth = -quakes[,3]/100)

d <- dist(Q) #D matrix euclidean distance

clusts <- hclust(d, method='single')
clusta <- hclust(d, method='average')
clustc <- hclust(d, method='complete')
clustw <- hclust(d, method='ward.D2')


## Ward-Linkage (see J-W p. 692-693): 
# Ward considered hierarchical clustering procedures based on minimizing the
# 'loss of information' from joining two groups. This method is usually implemented
# with loss of information taken to be an increase in an error sum of squares 
# criterion, ESS. 

open3d()
# ward linkage
clusterw <- cutree(clustw, 3)
plot3d(Q, size=3, col=clusterw+1, aspect = F)