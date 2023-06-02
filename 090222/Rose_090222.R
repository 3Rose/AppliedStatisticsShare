#The file nutrients.txt reports the nutrients (energy, proteins, fats, carbohydrates, sugars and fibers) of 295
#different breakfast cereals. Each statistical unit corresponds to a different brand and kind of cereals.
#a) Decide whether it is appropriate to use the original variables or the standardized ones, and perform a Principal
#Component Analysis of the dataset. Report the loadings of the first principal component.
#b) Report a plot of the loadings of the first 3 principal components and interpret them.
#c) Report the biplot of the data along the first two principal components and describe what characterizes the
#cereals with positive scores for both the first and second components.
#d) Report the screeplot. Propose (with motivations) a dimensionality reduction of the dataset. Report the variance
#explained along the principal components retained.
#e) Project a new cereal with 400kcal, 9g of proteins, 5g of fats, 100g of carbohydrates, 30g of sugar and 4g of fiber,
#on the reduced space identified at point (d).

nutrients <- read.table('nutrients.txt', header = T)
#a) Decide whether it is appropriate to use the original variables or the standardized ones, and perform a Principal
#Component Analysis of the dataset. Report the loadings of the first principal component.
head(nutrients)
nutrients.list <- as.list(nutrients)
bartlett.test(nutrients.list)
boxplot(nutrients)
#varianze molto diverse => standardizzo
std.nutrients <- scale(nutrients)
pc.nutrients <- princomp(std.nutrients)
summary(pc.nutrients)
loadings <- pc.nutrients$loadings
load.pc1 <- loadings[,1]


#b) Report a plot of the loadings of the first 3 principal components and interpret them.
x11()
par(mfrow = c(1,3))
for(i in 1:3) barplot(loadings[,i], ylim = c(-1, 1), las = 2, main=paste("PC",i))



#c) Report the biplot of the data along the first two principal components and describe what characterizes the
#cereals with positive scores for both the first and second components.
pc.nutrients$scores
plot(pc.nutrients$scores[,1], pc.nutrients$scores[,2])
abline(h=0)
abline(v=0)
dev.off()
# missing of interpretation
#d) Report the screeplot. Propose (with motivations) a dimensionality reduction of the dataset. Report the variance
#explained along the principal components retained.
x11()
plot(cumsum(pc.nutrients$sde^2)/sum(pc.nutrients$sde^2), type='b', xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=0.9)
# after the third components we don't gain a lot in terms of variability
#explained so we can keep the first 3 components


#e) Project a new cereal with 400kcal, 9g of proteins, 5g of fats, 100g of carbohydrates, 30g of sugar and 4g of fiber,
#on the reduced space identified at point (d).

x.new <- c(400, 9, 5, 100, 30, 4)
pc1.new <- rbind(loadings[,1]) %*% cbind(x.new)
pc2.new <- rbind(loadings[,2]) %*% cbind(x.new)
pc3.new <- rbind(loadings[,3]) %*% cbind(x.new)



#The file streaming.txt contains the data for 225 users of a music streaming service. The variable minutes contains
#the average minutes per day in 2021 for each user, while the variables artists the average number of single artists
#listened per day.
#a) Perform a cluster analysis of the users by using a hierarchical clustering method (Euclidean distance and single
#                                                                                      linkage). Report the plot of the dendrogram and the number of clusters you deem appropriate to represent the
#data.
#b) Report the centroids of the clusters, their size and the cophenetic coefficient.
#c) Provide Bonferroni intervals (global level 95%) for the mean of minutes and artists, within each of the clusters
#identified at point (a). Introduce and verify the appropriate assumptions.

streaming <- read.table('streaming.txt', header = T)

head(streaming)

plot(streaming)
#a) Perform a cluster analysis of the users by using a hierarchical clustering method (Euclidean distance and single
#linkage). Report the plot of the dendrogram and the number of clusters you deem appropriate to represent the
#data.

dist <- dist(streaming, method = 'euclidian')
dendogram.s <- hclust(dist, method='single')
plot(dendogram.s)
rect.hclust(dendogram.s, k=2)
#seems to be 2 clusters


#b) Report the centroids of the clusters, their size and the cophenetic coefficient.
clusters <- cutree(dendogram.s, k=2)
clusters
clust1 <- streaming[which(clusters==1),]
clust2 <- streaming[which(clusters==2),]
m.1 <- colMeans(clust1)
m.2 <- colMeans(clust2)
n.1 <- dim(clust1)[1]
n.2 <- dim(clust2)[1]
cophenetic(dendogram.s)


#c) Provide Bonferroni intervals (global level 95%) for the mean of minutes and artists, within each of the clusters
#identified at point (a). Introduce and verify the appropriate assumptions.
library(MVN)
mvn(clust1) # normal
mvn(clust2) # not normal
#We don't really need the gaussianity assumption beacuse we have a lot of data in each 
# cluster 

#clust1
head(streaming)
alpha <- 0.05
cov.1 <- cov(clust1)
k <- 2
ci1 <- cbind(
  inf = m.1 - qt(1-alpha/(2*k),n.1-1) * sqrt( diag(cov.1) / n.1),
  mean = m.1,
  sup = m.1 + qt(1-alpha/(2*k),n.1-1) * sqrt( diag(cov.1) / n.1)
)

#clust2
cov.2 <- cov(clust2)
ci1 <- cbind(
  inf = m.2 - qt(1-alpha/(2*k),n.2-1) * sqrt( diag(cov.2) / n.2),
  mean = m.2,
  sup = m.2 + qt(1-alpha/(2*k),n.2-1) * sqrt( diag(cov.2) / n.2)
)



#The file wine.txt reports the data on the alcohol content in 179 bottles of wine. For the alcohol content consider
#a linear model, accounting for the sugar content of grapes, and for type of wine (‘Red’, ‘Rose’, ‘White’):
#  alcoholg = β0,g + β1,g · sugar + ϵ,
#with ϵ ∼ N (0, σ 2 ) and g the grouping structure induced by the type of wine.
#a) Estimate the parameters of the model ({β0,g , β1,g , σ}). Verify the model assumptions, reporting any plot you
#consider important.
#b) Perform two statistical tests – each at level 1% – to verify if
#- there is a significant dependence of the mean alcohol content on the type of wine;
#- there is a significant dependence of the mean alcohol content on the sugar content.
#c) Based on tests (b) or any other test deemed relevant, reduce the model and report the updated model
#parameters.
#d) Build a prediction interval at 99% for a new bottle of red wine made with grapes with 20 g of sugar.

wine <- read.table('wine.txt')
head(wine)
plot(wine[,-3], col=as.factor(wine[,3]))


#a) Estimate the parameters of the model ({β0,g , β1,g , σ}). Verify the model assumptions, reporting any plot you
#consider important.
#  alcoholg = β0,g + β1,g · sugar + ϵ,

#dummys:
#10 red
#01 Rose
#00 white
wine
which(wine$type == 'Red')
which(wine$type == 'Rose')
which(wine$type == 'White')
dim(wine)[1]
dRed <- rep(c(1,0), c(68,179-68))
dRose <- rep(c(0,1,0),c(68,119-69+1,179-119))
data <- cbind(wine[,1:2], dRed = dRed, dRose = dRose)
lm1 <- lm(alcohol ~ dRed + dRose + sugar + dRed:sugar + dRose:sugar, data)
summary(lm1)

#beta0.white = b0       beta1.white = b3
#beta0.red = b0+b1      beta1.red = b3+b4
#beta0.rose = b0+b2     beta1.rose = b3+b5
lm1$coefficients
sum(lm1$residuals^2)/lm1$df.residual
par(mfrow=c(2,2))
plot(lm1)
#il QQplot suggerisce la gaussianità dei dati 
#i residui stanno intorno a 0 e non mostrano pattern particolari => incorrelati
#gaussianità + incorrelazione => indipendenza


#b) Perform two statistical tests – each at level 1% – to verify if
#- there is a significant dependence of the mean alcohol content on the type of wine;
#- there is a significant dependence of the mean alcohol content on the sugar content.

library(car)
linearHypothesis(lm1, rbind(c(0,1,0,0,0,0),
                            c(0,0,1,0,0,0),
                            c(0,0,0,0,1,0),
                            c(0,0,0,0,0,1)), c(0,0,0,0))
#Problem: I don't know if I have to test for types and his interactions or 
#just type

#c) Based on tests (b) or any other test deemed relevant, reduce the model and report the updated model
#parameters.
summary(lm1)
lm2 <- lm(alcohol ~ dRose + sugar + dRed:sugar + dRose:sugar, data)
summary(lm2)
lm3 <- lm(alcohol ~ sugar + dRed:sugar + dRose:sugar, data)
summary(lm3)
#seems to be all relevant now

lm3$coefficients
sig <- sum(lm3$residuals^2)/lm3$df.residual


#d) Build a prediction interval at 99% for a new bottle of red wine made with grapes with 20 g of sugar.
x.new <- data.frame(sugar = 20, dRed = 1, dRose = 0)
y.new <- predict.lm(lm3, x.new, interval = 'prediction', level = 0.99)




#The file walesharks.txt collects the number y of sightings of wale sharks during January 2022 at 64 observatory
#points in the Indian Ocean. The dataset also reports the UTM coordinates si of those locations, and the Chlorophyll
#concentration x(si ) (mg/m3 ) measured at the same locations. Consider for the variable y(si ), i = 1, ...64, the
#following model
#log[y(si )] = a0 + a1 · log[x(si )] + δ(si ),
#with δ(si ) a stationary residual.
#a) Estimate via generalized least squares the parameters a0 , a1 of the model. Report the model estimated for
#δ(si ), and discuss the model assumptions (use an exponential model without nugget for δ(si )).
#b) Provide a kriging prediction log[y ∗ (s0 )] of the log-number of sightings at an observatory point located close
#to the island of Fenfushi (South Ari Atoll, Maldives) s0 = (253844.8, 385997.7). For this purpose, use a
#point prediction of the log-transformed Chlorophyll concentration log[x(s0 )] obtained through a spatially
#stationary model (use a spherical model without nugget for log[x(s)]; report the estimated model, and the
#                  point prediction log[x(si )]∗ ).
#c) Report the kriging variance σ 2 (s0 ) of the point prediction at point (b). Would you deem the variance σ 2 (s0 )
#to be fully representative of the uncertainty associated with the prediction y ∗ (s0 )?
