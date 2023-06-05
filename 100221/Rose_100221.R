library(car)
library(rgl)
library(mvtnorm)
library(MASS)
library(sp)           ## Data management
library(lattice)      ## Data management
library(gstat)

#Problem n.1
#The file shopping.txt contains information on the usage of an online store selling clothing in the last 24 months.
#For each month, it reports the number of accesses to the online store, the number of purchases of men’s clothing
#and the number of purchases of women’s clothing. Assuming each month to be independent of the others, answer
#the following questions.
#a) Build a confidence region (level 95%) for the mean of the vector whose components are the number of accesses
#to the online store, the number of purchases of men’s clothing and the number of purchases of women’s clothing.
#Characterize the region by reporting its mathematical expression, its center, the direction of the axes and the
#length of the semi-axes. Introduce and test the hypothesis of Gaussianity; identify and discuss possible issues
#(if any).
#b) Build four T
#2
#-simultaneous confidence intervals (level 95%) for: the mean number of accesses to the online
#store, the mean number of purchases of men’s clothing, the mean number of purchases of women’s clothing and
#the mean number of total purchases.
#c) Perform a test of level 95% to verify the hypothesis according to which, in mean, more than 20% of the accesses
#to the online store result in a purchase. Report the p-value of the test.
setwd('/home/rose/Desktop/exams/Exams windows/100221/')
shopping <- read.table('shopping.txt', header = T)
head(shopping)

#a) Build a confidence region (level 95%) for the mean of the vector whose components are the number of accesses
#to the online store, the number of purchases of men’s clothing and the number of purchases of women’s clothing.
#Characterize the region by reporting its mathematical expression, its center, the direction of the axes and the
#length of the semi-axes. Introduce and test the hypothesis of Gaussianity; identify and discuss possible issues
#(if any).
sample.mean <- colMeans(shopping)
sample.cov <- cov(shopping)
dim(shopping)
#Gaussianity
library(MVN)
mvn(shopping)
#if we take the level at which we reject the hypotesis of gaussianity
#10%  then we reject the hypotesis of gaussianity
#5% then we do not reject the hypotesis of gaussianity
#I have choose to take the level 5% so we do not reject the null hypotesis

#confidence region
p <- dim(shopping)[2]
n <- dim(shopping)[1]
alpha <- 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

#confidence region "pseudo code"
# eta in R^p such that:
# n*(sample.mean - eta)' %*% solve(sample.cov) %*% (sample.mean - eta) <= cfr.fisher


#b) Build four T
#2
#-simultaneous confidence intervals (level 95%) for: the mean number of accesses to the online
#store, the mean number of purchases of men’s clothing, the mean number of purchases of women’s clothing and
#the mean number of total purchases.

sci <- cbind(inf = sample.mean - sqrt(cfr.fisher*diag(sample.cov)/n),
             center = sample.mean, 
             sup = sample.mean + sqrt(cfr.fisher*diag(sample.cov)/n))

a <- rbind(0,1,1)

sci.lc <- c(
      inf = t(a) %*% sample.mean - sqrt(cfr.fisher * t(a) %*% sample.cov %*% a/n),
      center = sample.mean[2]+sample.mean[3],
      sup = sample.mean[2]+sample.mean[3] + sqrt(cfr.fisher * t(a) %*% sample.cov %*% a/n)
)


#c) Perform a test of level 95% to verify the hypothesis according to which, in mean, more than 20% of the accesses
#to the online store result in a purchase. Report the p-value of the test.

prop.shopping <- (shopping$men  + shopping$women)/shopping$accesses
shapiro.test(prop.shopping)
#normally ditributed
alpha <- 0.05
t.test(prop.shopping, mu = 0.20, alternative = 'greater', conf.level = 1-alpha)






#Problem n.2
#In a study to develop an automated vision system able to distinguish between three species of rice, 60 images of
#rice grains were processed to obtain two morphological features for each grain of rice: the major axis length and
#the eccentricity. The data are reported in the file rice.txt.
#a) Use a hierarchical clustering method (Euclidean distance and complete linkage) to identify the three species of
#rice. Provide the plot of the dendrogram. Report the number of data within each cluster, the mean within the
#clusters and a plot of the results.
#b) Evaluate the performances of the clustering method at point (a) and identify the possible issues (if any). Propose,
#if needed, an alternative clustering method to cluster the data (report the number of data within each cluster,
#                                                                 the mean within the clusters and a plot of the results).
#c) Provide Bonferroni intervals (global level 95%) for the means and the variances of the major axis length of the
#rice grains within each of the three clusters identified. Introduce and verify the appropriate assumptions.

rice <- read.table('rice.txt', header = T)
head(rice)

plot(rice)

#a) Use a hierarchical clustering method (Euclidean distance and complete linkage) to identify the three species of
#rice. Provide the plot of the dendrogram. Report the number of data within each cluster, the mean within the
#clusters and a plot of the results.

dist.rice <- dist(rice,'euclidian')
dendogram <- hclust(dist.rice, 'complete' )

plot(dendogram, labels = FALSE)
rect.hclust(dendogram, k=3)

clusters.rice <- cutree(dendogram, k=3)
clust1 <- rice[which(clusters.rice == 1),]
clust2 <- rice[which(clusters.rice == 2),]
clust3 <- rice[which(clusters.rice == 3),]
dim(clust1)[1]
dim(clust2)[1]
dim(clust3)[1]
colMeans(clust1)
colMeans(clust2)
colMeans(clust3)

plot(rice, col = clusters.rice+1, pch=19)
points(colMeans(clust1)[1], colMeans(clust1)[2], pch = '+', cex = 2, col = 'red')
points(colMeans(clust2)[1], colMeans(clust2)[2], pch = '+', cex = 2, col = 'green')
points(colMeans(clust3)[1], colMeans(clust3)[2], pch = '+', cex = 2, col = 'blue')

#b) Evaluate the performances of the clustering method at point (a) and identify the possible issues (if any). Propose,
#if needed, an alternative clustering method to cluster the data (report the number of data within each cluster,
#                                                                 the mean within the clusters and a plot of the results).

#In the complete linkage clustering we see some data that could be missclusterized because
#the upper long cloud of points is splitted in red and green it may be better to
#see that cloud as a unique cluster


dendogram2 <- hclust(dist.rice, 'single')
plot(dendogram2, labels = FALSE)
clusters2 <- cutree(dendogram2, k=3)
plot(rice, col = clusters2+1, pch=19)

dendogram3 <- hclust(dist.rice, 'ward.D2')
clusters3 <- cutree(dendogram3, k=3)
plot(rice, col = clusters3+1, pch=19)

#the single linkage one seems to be better
dim(rice[which(clusters2 == 1),])[1]
dim(rice[which(clusters2 == 2),])[1]
dim(rice[which(clusters2 == 3),])[1]

colMeans(rice[which(clusters2 == 1),])
colMeans(rice[which(clusters2 == 2),])
colMeans(rice[which(clusters2 == 3),])

plot(rice, col = clusters2+1, pch=19)
points(colMeans(rice[which(clusters2 == 1),])[1], colMeans(rice[which(clusters2 == 1),])[2], 
       pch='+', cex=2, col='red')
points(colMeans(rice[which(clusters2 == 2),])[1], colMeans(rice[which(clusters2 == 2),])[2], 
       pch='+', cex=2, col='green')
points(colMeans(rice[which(clusters2 == 3),])[1], colMeans(rice[which(clusters2 == 3),])[2], 
       pch='+', cex=2, col='blue')

#c) Provide Bonferroni intervals (global level 95%) for the means and the variances of the major axis length of the
#rice grains within each of the three clusters identified. Introduce and verify the appropriate assumptions.
library(MVN)
k <- 12
c1 <- rice[which(clusters2 == 1),]
c2 <- rice[which(clusters2 == 2),]
c3 <- rice[which(clusters2 == 3),]
mvn(c1)
mvn(c2)
mvn(c3)



alpha <- 0.05
#intervals for mean1
a <- c(1,0)
bci.m1.1 <- c(
  inf = rbind(a)%*%colMeans(c1) - qt(1-alpha/(2*k),dim(c1)[1]-1) * sqrt( (rbind(a)%*%cov(c1)%*%cbind(a)) / dim(c1)[1]),
  center = rbind(a)%*%colMeans(c1),
  sup = rbind(a)%*%colMeans(c1) + qt(1-alpha/(2*k),dim(c1)[1]-1) * sqrt( (rbind(a)%*%cov(c1)%*%cbind(a)) / dim(c1)[1])
)

a <- c(0,1)
bci.m1.2 <- c(
  inf = rbind(a)%*%colMeans(c1) - qt(1-alpha/(2*k),dim(c1)[1]-1) * sqrt( (rbind(a)%*%cov(c1)%*%cbind(a)) / dim(c1)[1]),
  center = rbind(a)%*%colMeans(c1),
  sup = rbind(a)%*%colMeans(c1) + qt(1-alpha/(2*k),dim(c1)[1]-1) * sqrt( (rbind(a)%*%cov(c1)%*%cbind(a)) / dim(c1)[1])
)


#intervals for mean2
a <- c(1,0)
bci.m2.1 <- c(
  inf = rbind(a)%*%colMeans(c2) - qt(1-alpha/(2*k),dim(c2)[1]-1) * sqrt( (rbind(a)%*%cov(c2)%*%cbind(a)) / dim(c2)[1]),
  center = rbind(a)%*%colMeans(c2),
  sup = rbind(a)%*%colMeans(c2) + qt(1-alpha/(2*k),dim(c2)[1]-1) * sqrt( (rbind(a)%*%cov(c2)%*%cbind(a)) / dim(c2)[1])
)

a <- c(0,1)
bci.m2.2 <- c(
  inf = rbind(a)%*%colMeans(c2) - qt(1-alpha/(2*k),dim(c2)[1]-1) * sqrt( (rbind(a)%*%cov(c2)%*%cbind(a)) / dim(c2)[1]),
  center = rbind(a)%*%colMeans(c2),
  sup = rbind(a)%*%colMeans(c2) + qt(1-alpha/(2*k),dim(c2)[1]-1) * sqrt( (rbind(a)%*%cov(c2)%*%cbind(a)) / dim(c2)[1])
)

#intervals for mean3

a <- c(1,0)
bci.m3.1 <- c(
  inf = rbind(a)%*%colMeans(c3) - qt(1-alpha/(2*k),dim(c3)[1]-1) * sqrt( (rbind(a)%*%cov(c3)%*%cbind(a)) / dim(c3)[1]),
  center = rbind(a)%*%colMeans(c3),
  sup = rbind(a)%*%colMeans(c3) + qt(1-alpha/(2*k),dim(c3)[1]-1) * sqrt( (rbind(a)%*%cov(c3)%*%cbind(a)) / dim(c3)[1])
)

a <- c(0,1)
bci.m3.2 <- c(
  inf = rbind(a)%*%colMeans(c3) - qt(1-alpha/(2*k),dim(c3)[1]-1) * sqrt( (rbind(a)%*%cov(c3)%*%cbind(a)) / dim(c3)[1]),
  center = rbind(a)%*%colMeans(c3),
  sup = rbind(a)%*%colMeans(c3) + qt(1-alpha/(2*k),dim(c3)[1]-1) * sqrt( (rbind(a)%*%cov(c3)%*%cbind(a)) / dim(c3)[1])
)


#try alternative
t.test(c1[1], conf.level = 1-alpha/k)$conf.int
t.test(c1[2], conf.level = 1-alpha/k)$conf.int
#check
bci.m1.1
bci.m1.2
#they are the same ok!
t.test(c2[1], conf.level = 1-alpha/k)$conf.int
t.test(c2[2], conf.level = 1-alpha/k)$conf.int
t.test(c3[1], conf.level = 1-alpha/k)$conf.int
t.test(c3[2], conf.level = 1-alpha/k)$conf.int


#variances confidence intervals
ci.sig.1.1 <- c(
  ((dim(c1)[1]-1)*var(c1[1]))/qchisq(1-alpha/2*k, dim(c1)[1]-1),
  var(c1[1]),
  ((dim(c1)[1]-1)*var(c1[1]))/qchisq(alpha/2*k, dim(c1)[1]-1)
)

ci.sig.1.2 <- c(
  ((dim(c1)[1]-1)*var(c1[2]))/qchisq(1-alpha/2*k, dim(c1)[1]-1),
  var(c1[2]),
  ((dim(c1)[1]-1)*var(c1[2]))/qchisq(alpha/2*k, dim(c1)[1]-1)
)

ci.sig.2.1 <- c(
  ((dim(c2)[1]-1)*var(c2[1]))/qchisq(1-alpha/2*k, dim(c2)[1]-1),
  var(c2[1]),
  ((dim(c2)[1]-1)*var(c2[1]))/qchisq(alpha/2*k, dim(c2)[1]-1)
)
ci.sig.2.2 <- c(
  ((dim(c2)[1]-1)*var(c2[2]))/qchisq(1-alpha/2*k, dim(c2)[1]-1),
  var(c2[2]),
  ((dim(c2)[1]-1)*var(c2[2]))/qchisq(alpha/2*k, dim(c2)[1]-1)
)

ci.sig.3.1 <- c(
  ((dim(c3)[1]-1)*var(c3[1]))/qchisq(1-alpha/2*k, dim(c3)[1]-1),
  var(c3[1]),
  ((dim(c3)[1]-1)*var(c3[1]))/qchisq(alpha/2*k, dim(c3)[1]-1)
)

ci.sig.3.2 <- c(
  ((dim(c3)[2]-1)*var(c3[2]))/qchisq(1-alpha/2*k, dim(c3)[1]-1),
  var(c3[2]),
  ((dim(c3)[1]-1)*var(c3[2]))/qchisq(alpha/2*k, dim(c3)[1]-1)
)




#Problem n.3
#The file landslides.txt collects data on slow moving landslides. The dataset reports, for 80 monitored landslides,
#the average downslope displacement rate [mm/year], the annual precipitations [mm], the hardness of subsurface
#rocks [in Mohs’ scale], the quantity of coarse debris [g/cm3 ] and the quantity of fine debris [g/cm3 ].
#a) Formulate a linear regression model for the average downslope displacement rate, as a function of all the other
#variables. Report the model and its parametrization, together with the estimates of all its parameters. Verify
#the model assumptions.
#b) Based on appropriate test(s), reduce the model and update the model parameters.
#c) Using model (b), test the hypothesis according to which the effect on the displacement rate of the increase of 1
#g/cm3 of coarse debris is two times the effect of the increase of 1 g/cm3 of fine debris. Report the hypotheses
#and the p-value of the test performed. Possibly propose a new constrained model and estimate its parameters.
#d) A new landslide has been identified on a site with annual precipitations 700 mm, hardness of subsurface rocks
#5, quantity of coarse debris 10 g/cm3 and quantity of fine debris 8 g/cm3 . Using the last model, compute a
#pointwise estimate and a confidence interval of level 99% for the mean displacement rate of the new landslide.

landslides <- read.table('landslides.txt', header = T)
head(landslides)

#a) Formulate a linear regression model for the average downslope displacement rate, as a function of all the other
#variables. Report the model and its parametrization, together with the estimates of all its parameters. Verify
#the model assumptions.

rate.lm <- lm(rate ~ ., data = landslides)
summary(rate.lm)
coefs <- rate.lm$coefficients
sum(rate.lm$residuals^2)/rate.lm$df.residual

par(mfrow=c(2,2))
plot(rate.lm)
shapiro.test(rate.lm$residuals) #ok!

#b) Based on appropriate test(s), reduce the model and update the model parameters.
summary(rate.lm)
# we see that from the summary the only test for which the p-value is big (greater
#than 0.05) is the one related to the coefficent of the regressor hardness
#H0 is: that beta equals 0 => we cannot reject the null hypothesis at level 5 (or 10)
#so we can proceed removing that regressor
rate.lm2 <- lm(rate ~ rain + coarse + fine, landslides)
summary(rate.lm2)
#now all parameters seems to be significant
coefs <- rate.lm2$coefficients
sum(rate.lm2$residuals^2)/rate.lm2$df.residual


#c) Using model (b), test the hypothesis according to which the effect on the displacement rate of the increase of 1
#g/cm3 of coarse debris is two times the effect of the increase of 1 g/cm3 of fine debris. Report the hypotheses
#and the p-value of the test performed. Possibly propose a new constrained model and estimate its parameters.

#I have to test for B.course = 2* B.fine
hypothesis <- "coarse - 2 * fine = 0"
linearHypothesis(rate.lm2, hypothesis)
linearHypothesis(rate.lm2, hypothesis)$P
#non possiamo rigettare l'ipotesi nulla "coarse - 2 * fine = 0"
#per cui possiamo affermare che c'è evidenza statistica del fatto che
#the effect on the displacement rate of the increase of 1
#g/cm3 of coarse debris is two times the effect of the increase of 1 g/cm3 of fine debris

constrained_model <- update(rate.lm2, formula = rate ~ rain + coarse + I(2*fine))
constrained_model$coefficients
coefs
#d) A new landslide has been identified on a site with annual precipitations 700 mm, hardness of subsurface rocks
# 5, quantity of coarse debris 10 g/cm3 and quantity of fine debris 8 g/cm3 . Using the last model, compute a
#pointwise estimate and a confidence interval of level 99% for the mean displacement rate of the new landslide.

x.new <- data.frame(rain = 700, hardness = 5, coarse = 10, fine = 8)
predict(constrained_model, x.new)

predict(rate.lm2, x.new)
#(the result is the same)

prediction <- predict(constrained_model, x.new, interval = 'confidence', level = 0.99)



#Problem n.4
#The manager of the hotel Boule de Neige in Courmayeur is designing the pricing strategy for the carnival festivities
#2021. The file hotels.txt collects the prices per night y [e/night] in hotels in Courmayeur and neighboring
#villages, as observed for 55 hotels during 2019. The dataset also reports the UTM coordinates si of the hotels,
#whether the price refers to a day during the winter season or not (winter = yes or winter = no), and the
#distance of the considered hotel from the funicular connecting to the ski slopes, d(si ) = ksi − sf k, with sf =
#  (342362.58, 5072518.24). Consider for the price the following model
#y(si ) = a0,g + a1,g · d(si ) + δ(si ),
#with δ(si ) a stationary residual with spherical variogram with nugget, and g = 1, 2 the grouping induced by the
#variable winter (g = 1 for winter = yes, g = 2 otherwise).
#a) Assuming a1,g = 0 for g = 1, 2 and a0,1 = a0,2 = a0 , estimate the parameter a0 of the model via genera-
#  lized least squares. Report the point estimate of a0 and the model estimated for δ(si ); discuss the model
#assumptions.
#b) Assuming a1,g 6= 0, estimate the parameters a0,g , a1,g of the model via generalized least squares. Report the
#point estimate of a0,g , a1,g and the model estimated for δ(si ); discuss the model assumptions.
#c) Choose the best model between those estimated at points (a) and (b). Comment on your choice.
#d) Suggest to the hotel manager a pricing strategy for a stay of 4 nights at Boule de Neige in the period Feb.
#17th to Feb. 21st (winter = yes, s0 = (342399.74, 5072272.75)). Motivate your response and detail your
#assumptions.

hotels <- read.table('hotels.txt')
head(hotels)

#y(si ) = a0,g + a1,g · d(si ) + δ(si ),

#a) Assuming a1,g = 0 for g = 1, 2 and a0,1 = a0,2 = a0 , estimate the parameter a0 of the model via genera-
#  lized least squares. Report the point estimate of a0 and the model estimated for δ(si ); discuss the model
#assumptions.
dm.winter <- ifelse(hotels$winter == 'no', 0, 1)
hotels$winter <- dm.winter
hotels
coordinates(hotels) <- c('x','y')
variogram(price ~ 1, hotels)
plot(variogram(price ~ 1, hotels))
plot(variogram(price ~ 1, hotels), vgm(4000, 'Sph', 500, 1000))
v.fit <- fit.variogram(variogram(price ~ 1, hotels), vgm(4000, 'Sph', 500, 1000))
g.stat <- gstat(formula = price ~ 1, data = hotels, model = v.fit)
predict(g.stat, hotels[1,], BLUE = T)
predict(g.stat, hotels[23,], BLUE = T) #we predict all values with the same value
a0 <- predict(g.stat, hotels[1,], BLUE = T)$var1.pred
#Problem: I don't know how to estimate the model for δ(si).


#b) Assuming a1,g != 0, estimate the parameters a0,g , a1,g of the model via generalized least squares. Report the
#point estimate of a0,g , a1,g and the model estimated for δ(si ); discuss the model assumptions.

#y(si ) = a0,g + a1,g · d(si ) + δ(si ),

plot(variogram(price ~ winter + distance + I(winter*distance), hotels), cutoff=3000)
plot(variogram(price ~ winter + distance + I(winter*distance), hotels),
     vgm(1000, 'Sph',1500,200 ))
fit2 <- fit.variogram(variogram(price ~ winter + distance +I(winter*distance), hotels),
                      vgm(1000, 'Sph',1500,200 ))
g.tr <- gstat(formula = price ~ winter + distance + I(winter*distance), data = hotels, model = fit2)


#model: y(si) = b0 + b1 * winter + b2 * distance + b3 * winter * distance
# a0.0 = b0           a1.0 = b2
# a0.1 = b0 + b1      a1.1 = b2 + b3
hotels[1,]
hotels[2,]
# y.0 = b0 + b1 * distance

zetas <- predict(g.tr, hotels[1:2,], BLUE=T)$var1.pred
regressors <- cbind(c(1,1), hotels[1:2,]$distance)
#regressor %*% c(b0, b2) = zetas
B <- solve(regressors)%*%zetas
b0 <- B[1]
b2 <- B[2]

#now we search for b1 and b3
hotels[3,]
hotels[5,]
zetas2 <- c(predict(g.tr, hotels[3,], BLUE=T)$var1.pred,
            predict(g.tr, hotels[5,], BLUE=T)$var1.pred)
regressors2 <- cbind(c(1,1), c(hotels[3,]$distance,hotels[5,]$distance))
# a0.1 = b0 + b1      a1.1 = b2 + b3
#regressor2 %*% c(b0 + b1, b2 + b3) = zetas2
B2 <- solve(regressors2) %*% zetas2
b1 <- B2[1] - b0
b3 <- B2[2] - b2

B.final <- c(b0, b1, b2, b3)

#lets try
hotels[12,]
regressors.try <- c(1, 1, 2123.53, 1 * 2123.53)
regressors.try %*% B.final 
predict(g.tr, hotels[12,], BLUE = T)$var1.pred
#the result of the automatic prediction and the result obtained by the manual
#prediction (using estimated coefficents are quite the same. Good)


#c) Choose the best model between those estimated at points (a) and (b). Comment on your choice.
sum(predict(g.stat, hotels, BLUE = T)$var1.var) / dim(hotels)[1]
sum(predict(g.tr, hotels, BLUE = T)$var1.var) / dim(hotels)[1]

#it may be better the second beacuse we have less variability in the prediction?
#the prediction is unbiased in both the cases so we check for the
#variability?
#Problem: not shure about anything my bro



#d) Suggest to the hotel manager a pricing strategy for a stay of 4 nights at Boule de Neige in the period Feb.
#17th to Feb. 21st (winter = yes, s0 = (342399.74, 5072272.75)). Motivate your response and detail your
#assumptions.

#center (342362.58, 5072518.24)
dist <- sqrt((342399.74-342362.58)^2 + (5072272.75 - 5072518.24)^2)
s0.new <- data.frame(x = 342399.74, y = 5072272.75, winter = 1, distance = dist)
coordinates(s0.new) <- c('x','y')
s0.new <- as(s0.new, 'SpatialPointsDataFrame')
predict(g.tr, s0.new, BLUE =T)
# i Would suggest to set price equals to 429.9 but we have much more variability
#due to we are considering a new observation