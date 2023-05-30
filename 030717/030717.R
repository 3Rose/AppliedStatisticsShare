remove(list=ls())
load("/home/rose/Desktop/exams/Exams (in English) - WINDOWS/Labs/Lab 5/mcshapiro.test.RData")
setwd("~/MEGA/AppStat Exams (in English)/Exams (in English) - WINDOWS/030717")
library(car)
library(rgl)
library(mvtnorm)
library(MASS)
# Problem n.1
# The file kimono.txt collects the value (kEuro) of 528 silk kimonos sold in Mitsukoshi
# department stores in Kyoto and Tokyo, both tailor-made and ready-to-wear.

# a) Formulate an ANOVA model for the value of a kimono as a function of the 
#    factors city (Kyoto or Tokyo) and type (tailor-made, ready-to-wear). 
#    Verify the assumptions of the model.

kimono <- read.csv("kimono.txt", sep=" ", stringsAsFactors = TRUE)
head(kimono)
attach(kimono)
# Two-way ANOVA we must partition the groups
# But first, find the levels (even though it's obvious)
g <- length(levels(city))
b <- length(levels(type))
n <- length(value)/(g*b) # Simply divided by the cardinality of all possible combinations
N <- n*g*b

# Does each partition follow a gaussian distribution?
P <- c(shapiro.test(value[which(city == levels(city)[1] & type == levels(type)[1])])$p,
       shapiro.test(value[which(city == levels(city)[1] & type == levels(type)[2])])$p,
       shapiro.test(value[which(city == levels(city)[2] & type == levels(type)[1])])$p,
       shapiro.test(value[which(city == levels(city)[2] & type == levels(type)[2])])$p
  
)
P
# We can assume normality at 0.05, what about the variance? Bartlett?
bartlett.test(value, city:type)
# H_0 can't be rejected at 0.05 significance
# We can propose this model for the value of a Kimono:
# X_ijk = mu + tau_i + beta_j + gamma_ij + eps_ijk
# i = 1,2 (effect city)
# j = 1,2 (effect of type of kimono)
# gamma_ij = effect of interaction between groups i and j
# eps_ijk = uncertainty in sample k


# b) Through appropriate statistical tests, propose a reduced model.

# We perform a two-way ANOVA
# First we test to see if there are significant interactions
# Test on the complete model 
# H_0: gamma_ij = 0 for all i,j vs H1: exists i,j s.t. gamma_ij != 0
fit <- aov(value ~ city + type + city:type)
summary.aov(fit)

# We can say that city doesn't have an impact on its own, let's reduce the model
# X_ijk = mu + beta_j + gamma_ij + eps_ijk

fit.notype <- aov(value ~ type + city:type)
summary.aov(fit.notype)

# When removed from the equation the interaction becomes insignificant
# Our model is reduced to 
# X_i = mu + tau_i + eps_ijk
fit.oneway <- aov(value ~ type)
summary.aov(fit.oneway)

# The type of kimono has an influence at 5% significance 

# c) Provide Bonferroni intervals (global level 95%) for the differences 
# between the mean value of kimonos belonging
# to the homogeneous groups identified by the model at point (b).
# Consider only the type
levels(type)
handmade <- value[which(type == "hand-made")]
readytouse <- value[which(type == "ready-to-use")]

# CI for the difference in the mean of the two groups
# If we had more than one group
g <- 2
k <- g*(g-1)/2
alpha <- 0.05

ng      <- table(type)  
ng
Mediag  <- tapply(value, type, mean)
Mediag
SSres <- sum(residuals(fit.oneway)^2)
n <- 528
S <- SSres/(n-g)

Mediag
var(handmade)
var(readytouse)

as.numeric(c(Mediag[1]-Mediag[2] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2] )),
             Mediag[1]-Mediag[2] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2] ))))

#in alternativa risultato identico con Spooled (che alla fine è la stessa cosa dato
# che le varianze nei gruppi sono omogenee)
#Spooled <- as.numeric(((ng[1]-1)*var(handmade)+(ng[2]-1)*var(readytouse))/(ng[1]+ng[2]-2))
#as.numeric(c(Mediag[1]-Mediag[2] - qt(1-alpha/(2), n-g) * sqrt( Spooled * ( 1/ng[1] + 1/ng[2] )),
#             Mediag[1]-Mediag[2] + qt(1-alpha/(2), n-g) * sqrt( Spooled * ( 1/ng[1] + 1/ng[2] ))))

# Ok, is this the same as looking at the difference of mean? I think so!

# Problem n.2
# Having picnics in parks is very common in Japan, especially for the traditional custom of Hanami (flower viewing)
# during the bloom of cherry blossoms. The file bento.txt contains the total amount [g] of rice, sashimi, vegetables
# and okashi (traditional sweets) in the bentō’s (packed lunches) of 32 volunteer families, consumed on March 26th
# 2017 (for Hanami) and on May 7th 2017 (normal season). Assuming independent the composition of bentō’s of
# different families and not independent that of the same family, answer the following question.

dataset <- read.table("bento.txt", sep=" ", stringsAsFactors = TRUE)
head(dataset)
# Paired comparisons

# a) Perform a statistical test to verify if there is evidence of an impact of Hanami on the mean amount of rice,
# sashimi, vegetables and okashi in families bentō’s. Verify the needed assumptions.
D <- data.frame(rice=dataset[,1]-dataset[,5], 
                sashimi=dataset[,2]-dataset[,6],
                vegetables=dataset[,3]-dataset[,7], 
                okashi=dataset[,4]-dataset[,8]) 
D
# Do we satisfy the gaussian assumption?
mcshapiro.test(D)
# We can assume gaussianity at almost any significance level
n <- dim(D)[1]  
p <- dim(D)[2]  
n
p

# Calculate the sample mean and covariance matrix
D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

# Is there a difference at level 5%?
# Check if the difference is in mean zero
alpha   <- .05
delta.0 <- c(0,0,0,0)

# Hotelling's T2
D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

# Test result
D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# P-value computation
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
D.mean
# We can reject at any level H_0: no difference on average
# => There is almost certainly a difference in the amount of consumption


# b) Provide four T 2 simultaneous confidence intervals (global confidence 95%) for the increase in the mean con-
#   sumption of rice, sashimi, vegetables and okashi in correspondence of the bloom of cherry blossoms. Comment
# the results.
# Simultatenous T2 intervals (at 95%)
IC.T2.rice <- c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2.sashimi <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )
IC.T2.vegetables <- c( D.mean[3]-sqrt(cfr.fisher*D.cov[3,3]/n) , D.mean[3], D.mean[3]+sqrt(cfr.fisher*D.cov[3,3]/n) )
IC.T2.okashi  <- c( D.mean[4]-sqrt(cfr.fisher*D.cov[4,4]/n) , D.mean[4], D.mean[4]+sqrt(cfr.fisher*D.cov[4,4]/n) )

T2 <- rbind(IC.T2.rice, IC.T2.sashimi, IC.T2.vegetables, IC.T2.okashi)
dimnames(T2)[[2]] <- c('inf','center','sup')
T2

# The simultaneous T2 intervals DON'T contain the origin, therefore we can safely
# say that there's a difference in mean.

# 
# Problem n.3
# The file geisha.txt collects data about Geisha hunting in Kyoto (i.e., tours finalized to spot a Geisha). The data
# report the duration (minutes) and starting time (in minutes after 16:00) of 130 trials (not all successful).
geisha <- read.table("geisha.txt", sep = " ", stringsAsFactors = TRUE)
head(geisha)
# a) Use a hierarchical clustering method based on Euclidean distance and single linkage to identify two groups of
# data (i.e., successful and unsuccessful tours). Report the centers of the clusters, the size of the clusters, the
# cophenetic coefficient and a qualitative plot of the results.

# The first thing I do is plot the data (two-dimensional)
X11()
plot(geisha)
dev.off()
# It appears there are two well defined (almost circular clusters)
# For the sake of argument we compute the distance matrix

geisha.d <- dist(geisha, method='euclidean')
# Can't make anything out of it, as we would expect
x11()
image(1:160,1:160,as.matrix(geisha.d), main='metrics: Euclidean', asp=1, xlab='duration', ylab='time')
dev.off()

# Single linkage (with euclidean distance)

cluster.sl <- hclust(geisha.d, method='single')

x11()
plot(cluster.sl, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
dev.off()

# It appears to suggests 2 clusters...
# Let's cut the dendogram
x11()
plot(cluster.sl, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cluster.sl, k=2)
dev.off()

# Let's choose 2 clusters
# Fix k=2 clusters:
cluster.sl <- cutree(cluster.sl, k=2) 

x11()
plot(geisha, col=ifelse(cluster.sl==1,'red','blue'), pch=19)
dev.off()

# Cophenetic similarity -> How similar must these objects be to fall in the same cluster?
# Cophenetic coefficient -> Considers the distance (whatever that may be) between points and
#                           the difference in height in the dendogram. Does the dendogram represent
#                           these distances well?
# Computationally it's just a ration between the mean of levels and the distances
# Let's recompute the "uncut" cluster and calculate the cophenetic matrix
cluster.sl <- hclust(geisha.d, method='single')
coph.sl <- cophenetic(cluster.sl)

# We expect the distance matrix to be similar to the cluster cophenetic matrix
# The cophenetic coefficient tells us how well they fit

# Simple image 
x11()
par(mfrow=c(1,2))
image(as.matrix(geisha.d), main='Euclidean', asp=1 )
image(as.matrix(coph.sl), main='Single', asp=1 )
dev.off()

es <- cor(geisha.d, coph.sl)
c("Eucl-Single"=es)

# The center of each cluster is their respective mean

# Computation of cluster centers
C <- as.data.frame(matrix(nrow=0,ncol=2))
cluster.sl <- hclust(geisha.d, method='single')
cluster.sl <- cutree(cluster.sl, k=2) 


for(l in 1:length(unique(cluster.sl)))
  C <- rbind(C, colMeans(geisha[which(cluster.sl == l),]))

# Draw center
x11()
plot(geisha, col=ifelse(cluster.sl==1,'red','blue'), pch=19)
points(C, pch = 4, cex = 2, lwd = 2)
dev.off()

# Number of elements in cluster 1
sum(cluster.sl == 1)
# Number of elements in cluster 2
sum(cluster.sl == 2)

# b) Evaluate the quality of the clustering at point (a) and, in case you deem it unsatisfactory, repeat the procedure
# with another linkage at your choice.

# From the scatterplot it is clear that the chaining effect of single linkage doesn't 
# form appropiate clusters (by eye we can see the second one is bigger). We can try average
# linkage

cluster.av <- hclust(geisha.d, method='average')
# The two clusters are more balanced
x11()
plot(cluster.av, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
dev.off()

# Choose 2 clusters
cluster.av <- cutree(cluster.av, k=2) 

# The result is much more balanced and we can clearly see the two clusters
# This also confirms what we see by eye
x11()
plot(geisha, col=ifelse(cluster.av==1,'red','blue'), pch=19)
dev.off()
# Number of elements in cluster 1
sum(cluster.av == 1)
# Number of elements in cluster 2
sum(cluster.av == 2)

# c) Identify the successful tours with the smaller group found with the clustering method at point (b). Having
# introduced and verified the needed assumptions, provide 4 Bonferroni intervals (global level 90%) for the dif-
#   ference in the mean characteristics of successful and unsuccessful tours, and for the mean characteristics of a
# successful tour

# Test if populations are gaussian
P <- c(mcshapiro.test(geisha[which(cluster.av == 1),])$p,
       mcshapiro.test(geisha[which(cluster.av == 2),])$p)
P

# Assuming equal variance
# Both are gaussian (at level 5%)
# Simple Bonferroni intervals for the difference
k <- 4
n <- dim(geisha)[1]
alpha <- 0.1
cfr.t <- qt(1-alpha/(2*k),n-1)
geisha_successful <- geisha[which(cluster.av == 1),]
x.mean   <- colMeans(geisha_successful)
x.cov    <- cov(geisha_successful)
x.invcov <- solve(x.cov)
Bfsuccessful <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
            sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bfsuccessful

# And for the mean of the difference
geisha_unsuccessful <- geisha[which(cluster.av == 2),]
n1 <- 75
n2 <- 85
p <- 2
t1 <- geisha_successful
t2 <- geisha_unsuccessful

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

cfr.t <- qt(1-alpha/(2*k),n1+n2-2)
Bf1 <- cbind(inf = (t1.mean[1]-t2.mean[1]) - cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)),
             sup = (t1.mean[1]-t2.mean[1]) + cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)))
Bf2 <- cbind(inf = (t1.mean[2]-t2.mean[2]) - cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)),
             sup = (t1.mean[2]-t2.mean[2]) + cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)))
Bfdiff <- rbind(Bf1, Bf2)
dimnames(Bfdiff)[[2]] <- c('inf','sup')    
Bfdiff
Bfsuccessful

# d) Comment the results at point (c) and suggest a successful strategy for Geisha hunting.

# The successful Geisha spottings last about 40 minutes more than successful ones and start 
# 15 minutes earlier than unsuccessful ones. So a good strategy is starting at around 16:45
# and search for a geisha for about an hour and a half.


# Problem n.4
# The file garden.txt collects the number of carps, maple trees, cherry trees and stones, and the extension [m2 ] of
# 156 of garden in the Kantō region of Japan. Experts believe that, to achieve an overall balance of elements, the
# Japanese gardens follow the model
garden <- read.table("garden.txt", sep = " ", header = T)
View(garden)
# E = β0 + β1 · x1 + β2 · x2 + β3 · x3 + β4 · x4 + ε,

# with E the extension of the garden, x1 , x2, x3 , x4 the number of carps, maple trees, cherry trees and stones
# respectively, and ε ∼ N (0, σ2 ).
colnames(garden)
# a) Estimate the 6 parameters of the model and verify the model assumptions. Evaluate the residuals of the model.
fit <- lm(extension ~ carps + maple + cherry + stones, data = garden)
summary(fit)

# The coefficients of the model
fit$coefficients

# The residuals appear to be gaussian
shapiro.test(fit$residuals)

#We run diagnostics
par(mfrow=c(2,2))
plot(fit)

# No strange behaviour

# b) Perform two statistical tests to verify if
# - there is statistical evidence of a dependence of the mean garden extension on the number of maple or cherry
# trees;

linearHypothesis(fit,rbind(c(0,0,1,0,0),
                           c(0,0,0,1,0)),c(0,0))

# - there is statistical evidence of a dependence of the mean garden extension on lake elements (stones, carps).
# We run a linear hypothesis test
linearHypothesis(fit,rbind(c(0,1,0,0,0),
                           c(0,0,0,0,1)),c(0,0))

# They do have an impact, so they both can't be zero

# c) Based on the results at point (b), comment on possible model weaknesses and, if needed, reduce the dimensio-
#   nality of the regressors. Comments the analysis and interpret the results.

# In b) we saw that there's no evidence to state that both can be zero. They do have an impact
# on garden extension, however, are they linearly independent? Can they individually be taken out?


# d) Update the estimates of the parameters using the analysis at point (c).
linearHypothesis(fit,rbind(c(0,0,0,1,0)),0)
linearHypothesis(fit,rbind(c(0,0,1,0,0)),0)

pairs(garden)
# Also from the plots we see a dependence between carps and stones. I can work
# with this reduced model.
# The weakness of the previous model was the collinearity 
# between some of the regressors. Which leads us to this new model:

# E = beta_0 + beta_1*x_1 + b_4*x_4 + epsilon
# epsilon ~ N(0,sigma^2)

# We took out the carps and cherry trees

fit2 <- lm(extension ~ stones + maple, data = garden)
X11()
par(mfrow = c(2,2))
plot(fit2)

# Nothing strange in the diagnostics, residuals are normal 
# and there's no relation between residuals and fitted values. Plus there are
# no samples that significantly condition the model (nothing beyond Cook's distance).


