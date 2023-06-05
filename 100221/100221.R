library(car)
library(rgl)
library(mvtnorm)
library(MASS)
library(sp)           ## Data management
library(lattice)      ## Data management
library(gstat)   
remove(list = setdiff(ls(), lsf.str()))
load("~/Documents/WeBeep Sync/APPLIED STATISTICS/Labs/Lab 5/mcshapiro.test.RData")
setwd("~/data_hdd2/MEGA/AppStat Exams (in English)/Exams (in English)/100221")

# Problem n.1
# The file shopping.txt contains information on the usage of an online store selling clothing in the last 24 months.
# For each month, it reports the number of accesses to the online store, the number of purchases of men’s clothing
# and the number of purchases of women’s clothing. Assuming each month to be independent of the others, answer
# the following questions.

shopping <- read.csv("shopping.txt", sep=" ")
shopping

# a) Build a confidence region (level 95%) for the mean of the vector whose components are the number of accesses
# to the online store, the number of purchases of men’s clothing and the number of purchases of women’s clothing.
# Characterize the region by reporting its mathematical expression, its center, the direction of the axes and the
# length of the semi-axes. Introduce and test the hypothesis of Gaussianity; identify and discuss possible issues
# (if any).

# mean <- c(mean[1], mean[2])
# eigenvalues <- c(eigen1, eigen2)
# (x-mean[1])/eigen1^2 + (y -mean[2])/eigen2^2 = 1
mcshapiro.test(shopping)
D <- shopping
# p-value above .05 I can't reject the gaussianity hypothesis
n <- dim(D)[1]  # 11
p <- dim(D)[2]  #  2

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)
alpha   <- .05

# Distributed as Fisher with p, n-p degrees of freedom
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

# Spectral decomposition of convariance matrix
decomp <- eigen(D.cov)
decomp

# Direction of axes, the eigenvectors 
decomp$vectors

# Center, the sample mean 
D.mean

# Radius of ellipse, square root of Fisher quantile
r <- sqrt(cfr.fisher)
r

#Length of semi-axes, proportional to eigenvalues
lengthSemiAxes <- r*sqrt(decomp$values)
lengthSemiAxes

# b) Build four T 2-simultaneous confidence intervals (level 95%) for: the mean number of accesses to the online
# store, the mean number of purchases of men’s clothing, the mean number of purchases of women’s clothing and
# the mean number of total purchases.
alpha <- 0.05
D[,4] <- D[,3] + D[,2]
D[,4]
n <- dim(D)[1]  # 11
p <- dim(D)[2]  #  2

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)

# We use the Fisher quantile since T2 follows that distribution
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

# Through the formula we compute the intervals

ic.t2.access <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
ic.t2.men <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )
ic.t2.women <- c( D.mean[3]-sqrt(cfr.fisher*D.cov[3,3]/n) , D.mean[3], D.mean[3]+sqrt(cfr.fisher*D.cov[3,3]/n) )
ic.t2.purchase <- c( D.mean[4]-sqrt(cfr.fisher*D.cov[4,4]/n) , D.mean[4], D.mean[4]+sqrt(cfr.fisher*D.cov[4,4]/n) )
ic.t2 <- rbind(ic.t2.access, ic.t2.men, ic.t2.women, ic.t2.purchase)
dimnames(ic.t2)[[2]] <- c('inf','center','sup')
ic.t2

# c) Perform a test of level 95% to verify the hypothesis according to which, in mean, more than 20% of the accesses
# to the online store result in a purchase. Report the p-value of the test.

# Consider the fourth column, the mean of that column must be greater than 0.2 times the mean of the first one
# Comparison of the means of two gaussian populations
# t-test from Stat 101
# H0: mu1 >= mu0*0.2 
# H0: - mu1 + mu0*0.2 <= 0
# H0: a'mu <= 0 vs H1: a'mu < 0 where a = c(0.2,-1)

a <- as.matrix(c(0.2,-1), 2,1)
delta.0 <- 0
accPurchase <- as.matrix(cbind(D[,1],D[,4]))
# Must perform a t-test
t.stat <- (mean(accPurchase %*% a) - delta.0) / sqrt( var(accPurchase %*% a) / n ) 
t.stat

P <- 1-pt(t.stat, n-1)
P

# We can't reject the hypothesis at level 95%


# Problem n.2
# In a study to develop an automated vision system able to distinguish between three species of rice, 60 images of
# rice grains were processed to obtain two morphological features for each grain of rice: the major axis length and
# the eccentricity. The data are reported in the file rice.txt.

rice <- read.csv("rice.txt", sep=" ", stringsAsFactors = TRUE)
rice

# a) Use a hierarchical clustering method (Euclidean distance and complete linkage) to identify the three species of
# rice. Provide the plot of the dendrogram. Report the number of data within each cluster, the mean within the
# clusters and a plot of the results.

plot(rice)

# We can clearly see 3 clusters, even though one of them is elongated 
dataset.d <- dist(rice, method='euclidean')

# Distance matrix plot
x11()
image(1:81,1:81,as.matrix(dataset.d), main='metrics: Euclidean', asp=1, xlab='duration', ylab='time')
dev.off()

# Applied clustering (single, average, complete) to distances
cluster.co <- hclust(dataset.d, method='complete')

# Dendogram of cluster
x11()
plot(cluster.co, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
dev.off()

# Cut the dendogram at k - clusters (just for the plot)
x11()
plot(cluster.co, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cluster.co, k=3)
dev.off()

# Cut the variable itself into k-clusters
cluster.co <- cutree(cluster.co, k=3) 

# Scatter plot w.r.t. clusters
x11()
plot(rice, col=ifelse(cluster.co==1,'red',ifelse(cluster.co==2,'blue','green')), pch=19)


# Number of data in each cluster
sum(cluster.co==1)
sum(cluster.co==2)
sum(cluster.co==3)

# Compute the means of each cluster
C <- as.data.frame(matrix(nrow=0,ncol=2))
for(l in 1:length(unique(cluster.co)))
  C <- rbind(C, colMeans(rice[which(cluster.co == l),]))
points(C, pch = 4, cex = 2, lwd = 2)
dev.off()

# These clusters don't really match what we see by eye. The elongated one is divided by two.

# b) Evaluate the performances of the clustering method at point (a) and identify the possible issues (if any). Propose,
# if needed, an alternative clustering method to cluster the data (report the number of data within each cluster,
#                                                                  the mean within the clusters and a plot of the results).

# The clusters don't match what we see by eye, the big cluster (elongated one) is divided in two. We can try average linkage


dataset.d <- dist(rice, method='euclidean')

# Distance matrix plot
x11()
image(1:81,1:81,as.matrix(dataset.d), main='metrics: Euclidean', asp=1, xlab='duration', ylab='time')
dev.off()

# Applied clustering (single, average, average) to distances
cluster.av <- hclust(dataset.d, method='average')

# Dendogram of cluster
x11()
plot(cluster.av, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
dev.off()

# Cut the dendogram at k - clusters (just for the plot)
x11()
plot(cluster.av, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cluster.av, k=3)
dev.off()

# Cut the variable itself into k-clusters
cluster.av <- cutree(cluster.av, k=3) 

# Scatter plot w.r.t. clusters
x11()
plot(rice, col=ifelse(cluster.av==1,'red',ifelse(cluster.av==2,'blue','green')), pch=19)


# Number of data in each cluster
sum(cluster.av==1)
sum(cluster.av==2)
sum(cluster.av==3)

# Compute the means of each cluster
C <- as.data.frame(matrix(nrow=0,ncol=2))
for(l in 1:length(unique(cluster.av)))
  C <- rbind(C, colMeans(rice[which(cluster.av == l),]))
points(C, pch = 4, cex = 2, lwd = 2)
dev.off()

# These 3 cluster are a great improvement, they match what we see by eye.

# c) Provide Bonferroni intervals (global level 95%) for the means and the variances of the major axis length of the
# rice grains within each of the three clusters identified. Introduce and verify the appropriate assumptions.

i1 <- which(cluster.av==1)
i2 <- which(cluster.av==2)
i3 <- which(cluster.av==3)

t1 <- rice[i1,]
t2 <- rice[i2,]
t3 <- rice[i3,]

P <- c(mcshapiro.test(t1)$p,
       mcshapiro.test(t2)$p,
       mcshapiro.test(t3)$p)
P

# The samples are gaussian

# We apply Bonferroni intervals (6 of them)
# For each type of rice we compute a confidence interval for the mean
# and the variance
k <- 6
alpha <- 0.05
for(i in 1:3) {
  # Sample mean, covariance
  n <- sum(cluster.av==i)
  x.mean   <- mean(rice[get(paste('i',i,sep='')),1])
  x.var    <- var(rice[get(paste('i',i,sep='')),1])
  cfr.t <- qt(1-alpha/(2*k),n-1)
  print(paste("Mean of cluster",i))
  ICMean <- cbind(inf = x.mean - cfr.t*sqrt(x.var/n),
                  center = x.mean, 
                  sup = x.mean + cfr.t*sqrt(x.var/n))
  # We perform a chi-square test on the variance
  ICVar <- cbind(inf=x.var*(n-1) / qchisq(1 - alpha/(2*k), n-1),
                 center=x.var,
                 sup=x.var*(n-1) / qchisq(alpha/(2*k), n-1))
  print(ICMean)
  print(paste("Variance of cluster",i))
  print(ICVar)
}


# Problem n.3
# The file landslides.txt collects data on slow moving landslides. The dataset reports, for 80 monitored landslides,
# the average downslope displacement rate [mm/year], the annual precipitations [mm], the hardness of subsurface
# rocks [in Mohs’ scale], the quantity of coarse debris [g/cm3] and the quantity of fine debris [g/cm3].
landslides <- read.table("landslides.txt", sep =" ", header=T)
View(landslides)
# a) Formulate a linear regression model for the average downslope displacement rate, as a function of all the other
# variables. Report the model and its parametrization, together with the estimates of all its parameters. Verify
# the model assumptions.
# Not knowing the physics of this I propose this straight forward linear model

# rate = beta0 + beta1 * rain + beta2 * hardness + beta3 * coarse + beta4 * fine + epsilon
# where epsilon ~ N(0,sigma^2) for some sigma

fit <- lm(rate ~ rain + hardness + coarse + fine, data = landslides)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

# We run some diagnostics on our model and nothing seems to be out of order.
shapiro.test(residuals(fit))
shapiro.test(rstudent(fit))
# The residuals appear to be gaussian at level 5% (or even higher)
# Our estimates of beta_i for i = 0,...,4
fit$coefficients


# b) Based on appropriate test(s), reduce the model and update the model parameters.

# Is there collinearity?

vif(fit)
pairs(landslides[2:5])
# From the variance inflation rate it doesn't appear to be the case, we perform 
# a test to see if all the columns are influential
summary(fit)

linearHypothesis(fit, rbind(c(0,0,1,0,0)),0)
# We take out the hardness
fit2 <- lm(rate ~ rain + coarse + fine, data = landslides)
summary(fit2)

# It appears this is as much as we can remove, none of the other

# c) Using model (b), test the hypothesis according to which the effect on the displacement rate of the increase of 1
# g/cm3 of coarse debris is two times the effect of the increase of 1 g/cm3 of fine debris. Report the hypotheses
# and the p-value of the test performed. Possibly propose a new constrained model and estimate its parameters.


# We use fit2 and test this linear combination
linearHypothesis(fit2, rbind(c(0,0,2,1)),0)
plot(x = landslides$fine, y = landslides$coarse)
# We stay with the original model
pairs(landslides)
dev.off()
# d) A new landslide has been identified on a site with annual precipitations 700 mm, hardness of subsurface rocks
# 5, quantity of coarse debris 10 g/cm3 and quantity of fine debris 8 g/cm3. Using the last model, compute a
# pointwise estimate and a confidence interval of level 99% for the mean displacement rate of the new landslid


Z0.new <- data.frame(rain = 700, coarse = 10, fine = 8)

# Conf. int. for the mean
Conf <- predict(fit2, Z0.new, interval='confidence', level=1-0.01)  
Conf

# An estimate of the mean is the fitted value 30.26575 
# We also see a 99% conf. interval (30.038, 30.49)

# Problem n. 4
# The manager of the hotel Boule de Neige in Courmayeur is designing the pricing strategy for the carnival festivities
# 2021. The file hotels.txt collects the prices per night y [e/night] in hotels in Courmayeur and neighboring
# villages, as observed for 55 hotels during 2019. The dataset also reports the UTM coordinates si of the hotels,
# whether the price refers to a day during the winter season or not (winter = yes or winter = no), and the
# distance of the considered hotel from the funicular connecting to the ski slopes, d(si ) = ksi − sf k, with sf =
#   (342362.58, 5072518.24). Consider for the price the following model
# y(si) = a0,g + a1,g · d(si ) + δ(si ),
# with δ(si ) a stationary residual with spherical variogram with nugget, and g = 1, 2 the grouping induced by the
# variable winter (g = 1 for winter = yes, g = 2 otherwise).
hotels <- read.table("hotels.txt", sep= " ")
hotels

g <- rep(0, nrow(hotels))
g [which(hotels$winter == "yes")] <- 1
g

coordinates(hotels) <- c('x','y')
hotels

# a) Assuming a1,g = 0 for g = 1, 2 and a0,1 = a0,2 = a0 , estimate the parameter a0 of the model via genera-
#   lized least squares. Report the point estimate of a0 and the model estimated for δ(si ); discuss the model
# assumptions.

# Empirical variogram
v <- variogram(price ~ 1, data = hotels)
plot(v)

# We generate a model as suggested in the text

v.fit1 <- fit.variogram(v, vgm(4000, "Sph", 1000, 500))
plot(v, v.fit1, pch = 3)
v.fit1

g.tr <- gstat(formula = price ~ 1, data = hotels, model = v.fit1)
predict(g.tr, hotels[1,], BLUE = TRUE)

# Estimates
# [generalized least squares trend estimation]
# coordinates var1.pred   var1.var
# 1 (344076.8, 5072878)  260.6138 0.01895275

# b) Assuming a1,g 6= 0, estimate the parameters a0,g , a1,g of the model via generalized least squares. Report the
# point estimate of a0,g , a1,g and the model estimated for δ(si ); discuss the model assumptions.
v <- variogram(price ~ 1 + price:g, data = hotels)
plot(v)
v.fit1 <- fit.variogram(v, vgm(1000, "Sph", 1000, 200))
plot(v, v.fit1, pch = 3)
v.fit1


# c) Choose the best model between those estimated at points (a) and (b). Comment on your choice.

# d) Suggest to the hotel manager a pricing strategy for a stay of 4 nights at Boule de Neige in the period Feb.
# 17th to Feb. 21st (winter = yes, s0 = (342399.74, 5072272.75)). Motivate your response and detail your
# assumptions.

