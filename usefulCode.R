library(MVN)
library(mvtnorm)
library(MASS)
library(rgl)
library(car)
library(lattice)
library(sp)
library(gstat)
library(class)
library(fda)
library(fdakma)

library(fdacluster)
library(KernSmooth)
library(fields)
library(corrplot)
library(nlme)
library(nlmeU)
library(plot.matrix)
library(lme4)
library(insight)
library(glmnet)
library(e1071)

###DUMMYS###
#----------------------------------------------------------------------
#example of dummy building
d.red <- ifelse(wine$color=='red',1,0)
d.p <- ifelse(wine$region=='Piemonte',1,0)
d.t <- ifelse(wine$region=='Toscana',1,0)
#----------------------------------------------------------------------


###INFERENCE###
#----------------------------------------------------------------------

#1 univariate
#Assume gaussianity of data_samples

# automatic
t.test(data_samples, mu = mean.H0, alternative = 'two.sided', conf.level = 1-alpha)
#obs:
#'two.sided' -> H0: mu = mean.H0  vs  H1: mu != mean.H0
#'greater'   -> H0: mu < mean.H0  vs  H1: mu > mean.H0
#'less'      -> H0: mu > mean.H0  vs  H1: mu < mean.H0

# manual
tstat <- (sample.mean - mean.H0) / (sample.sd / sqrt(n))
cfr.t <- qt(1 - alpha/2, n-1)
abs(tstat) < cfr.t
pvalue  <- ifelse(tstat >= 0, (1 - pt(tstat, n-1))*2, pt(tstat, n-1)*2)

#1.2 confidence intervals
  #mean
t.test(data_samples, mu = mean.H0, alternative = 'two.sided', conf.level = 1-alpha)$conf.int
#or
ci <- c(
      sample.mean - qt(1 - alpha/2, n-1) * (sample.sd / sqrt(n)),
      sample.mean,
      sample.mean + qt(1 - alpha/2, n-1) * (sample.sd / sqrt(n))
)
  #variance
ci.var <- c(
          (n - 1) * sample.var / qchisq(1 - alpha/2, n-1),
          sample.var,
          (n - 1) * sample.var / qchisq(alpha/2, n-1)
)


#2 multivariate
# n large => use chi squared
# n small and gaussian samples => use Fisher

#manual
T2stat <- n * (sample.mean-mu0) %*% inv.sample.cov %*% (sample.mean-mu0) 
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p) # gaussian samples
cfr.chisq <- qchisq(1-alpha, p) # not gaussian samples but n large
pvalue <- 1 - pf(x.T2*(n-p) / ((n-1)*p), p, n-p)

#2.1 confidence region
ellipse(sample.mean, sample.cov/n, sqrt(cfr.fisher)) #or
ellipse(sample.mean, sample.cov/n, sqrt(cfr.chisq))
#center: sample.mean
#axis direction: eigenvectors
#semi-axis length: 
sqrt(eigen(sample.cov/n)$values) * sqrt(cfr.fisher)  # or cfr.chisq
#formula:
  #{eta in R^p : n*(sample.mean - eta)' %*% S^(-1) %*% (sample.mean - eta)  <= cfr.fisher}
  #or for n large
  #{eta in R^p : n*(sample.mean - eta)' %*% S^(-1) %*% (sample.mean - eta)  <= cfr.chisq}




#2.2 one-at-the-time confidence intervals
ci <- c(
      rbind(a)%*%sample.mean - qt(1-alpha/2,n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n),
      rbind(a)%*%sample.mean,
      rbind(a)%*%sample.mean + qt(1-alpha/2,n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n)
)
#for the component i
ci_i <- c(
      sample.mean[i] - qt(1-alpha/2,n-1) * sqrt( sample.cov[i,i] / n),
      sample.mean[i],
      sample.mean[i] + qt(1-alpha/2,n-1) * sqrt( sample.cov[i,i] / n)
)

#2.2 simultaneous confidence intervals
#compute for all components
sci <- cbind(inf = sample.mean - sqrt(cfr.fisher*diag(sample.cov)/n),
            center = sample.mean, 
            sup = sample.mean + sqrt(cfr.fisher*diag(sample.cov)/n))
#for one component (i)
sci_i <- c(
      sample.mean[i] - sqrt(cfr.fisher * sample.cov[i,i]/n),
      sample.mean[i],
      sample.mean[i] + sqrt(cfr.fisher * sample.cov[i,i]/n)
)
# note that cfr.fisher contains ((n-1)*p)/(n-p)

#2.3 Bonferroni confidence intervals
k <- k
bci <- c(
  rbind(a)%*%sample.mean - qt(1-alpha/(2*k),n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n),
  rbind(a)%*%sample.mean,
  rbind(a)%*%sample.mean + qt(1-alpha/(2*k),n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n)
)
#for the component i
ci_i <- c(
  sample.mean[i] - qt(1-alpha/(2*k),n-1) * sqrt( sample.cov[i,i] / n),
  sample.mean[i],
  sample.mean[i] + qt(1-alpha/(2*k),n-1) * sqrt( sample.cov[i,i] / n)
)


#2.4 region within 2 intervals
#having 2 intervals one for each of the 2 variables
segments(b.int1[1], b.int2[1], b.int1[2], b.int2[1])
segments(b.int1[1], b.int2[2], b.int1[2], b.int2[2])
segments(b.int1[1], b.int2[1], b.int1[1], b.int2[2])
segments(b.int1[2], b.int2[1], b.int1[2], b.int2[2])
#----------------------------------------------------------------------




###MANOVA###
#----------------------------------------------------------------------

#ASSUMPTIONS

#Observations:
#if we want to use manova/anova and we have more grouping factors (ex: g,b)
#we have to evaluate the assumptions on all the possible combinations

#1. test for gaussianity in each group

#2. test for same covariances in the groups


#ANOVA
data.aov <- aov(target ~ factor.dummys + dummy.interactions, data)

#Assumptions:
  #test for gaussianity in each group
  #example:
  shapiro.test(wine[which(wine$region=='Piemonte' & wine$color=='red'),][,1])$p
  
  #test for same covariances in the groups
  #compute variances
  #build a list of the groups
  #for univariate: bartlett.test(groups.list)
  #for univariate and 2 groups : var.test()
  
  #example
  groups <- list(wine[which(wine$region=='Piemonte' & wine$color=='red'),][,1],
                 wine[which(wine$region=='Piemonte' & wine$color=='white'),][,1],
                 wine[which(wine$region=='Toscana' & wine$color=='red'),][,1],
                 wine[which(wine$region=='Toscana' & wine$color=='white'),][,1],
                 wine[which(wine$region=='Veneto' & wine$color=='red'),][,1],
                 wine[which(wine$region=='Veneto' & wine$color=='white'),][,1])
  bartlett.test(groups)

#----------------------------------------------------------------------
  
###PCA (PRINCIPAL COMPONENTS ANALYSIS)###
#----------------------------------------------------------------------
#check the covariances
#if they are not comparable standardize the variables !!!
scale(dataset) #(optional)
  
princomp(dataset, )  
  
#plot loadings
par(mfcol=c(k,k))
for(i in 1:num_of_comp) barplot(pca.data$loadings[,i], ylim = c(-1, 1), main=paste("PC",i), las=2)

#scatterplot on first 2 compononents
plot(pca.data$scores[,1],pca.data$scores[,2], 'col= as.factor(data$Type)', pch=19)
abline(h=0)
abline(v=0)
#legend('topleft', levels(as.factor(data$Type)), col=c(1,2,3),
#       pch=16)

#----------------------------------------------------------------------



###LINEAR REGRESSION/MODELS###
#----------------------------------------------------------------------
#residuals and assumptions
par(mfrow=c(2,2))
plot(model)
shapiro.test(model$residuals)

#lasso
x <- model.matrix(model.formula, data)[,-1]
y <- data$target
lasso.mod <- glmnet(x,y,lambda=lambda) #alpha = 0
#coefficents
coef.lasso <- predict(lasso.mod, type = 'coefficients')

#chose lambda
lambda.grid <- seq(min, max, length = k)
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV
bestlam.lasso <- cv.lasso$lambda.min
plot(cv.lasso)

#prediction 
x.new <- data.frame(variables)
x.new <- as.matrix(x.new)
predict(lasso.mod2,newx=x.new)

#tests on coefficents
linearHypothesis(lm, rbind(c(0,0,0,0,0,0)), c(0))

#test if a regressor is two times more influential than an other
hypothesis <- "reeg1 - 2 * reg2 = 0"
linearHypothesis(lm, hypothesis)



#----------------------------------------------------------------------


###CLASSIFICATION###
#----------------------------------------------------------------------

#1 QDA
#Assumptions
#constant costs
#gaussianity within groups
mvn(data[which(data$factor == x),-factor])

#1.1 qda() and prediction of dataset
qda.data <- qda(data[,-factor], data[,factor])
Qda.data <- predict(qda.data, data[,-factor])

#1.1.1 separation lines
#example
x  <- seq(min(data[,1]), max(data[,1]), length=200)
y  <- seq(min(data[,2]), max(data[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)

z  <- predict(lda.iris, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2], z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1], z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
z3 <- z[,3] - pmax(z[,1], z[,2])  # P_3*f_3(x,y)-max{P_j*f_j(x,y)}

#plot
plot(data[,-factor], col=as.factor(data[,factor]), pch=19)
contour(x, y, matrix(z1, 200),levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200),levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200),levels=0, drawlabels=F, add=T)

#1.2 AER by crossvalidation leave one out
data.qdaCV <- qda(factor ~ quantitativedata, data = data, prior = c(p1, p2), CV = T)
data.qdaCV

table(class.true=data[,factor], class.assignedCV=data.qdaCV$class)
est_AER <- n12/150 * p1 + n21/150 * p2

#1.3 total costs
tot.costs <- n12/150 * p1 * c21 + n21/150 * p2 *c12



#3 knn (k nearest neighbour)

#k crossvalidation example
set.seed(19)

best.k = 0 #will save best k
best.err = 1 #will save best error
best.model = 0 #will save best model

for (k in 10:30) {
  knn = knn.cv(df[,1:2], cl = df[,3], k)  
  conf.matrix = table(df[,3], knn)
  
  miss <- (conf.matrix[1,2] + conf.matrix[2,1])/n
  
  if (miss < best.err){
    best.err = miss
    best.k = k
    best.model = knn
  }
}

best.model  
best.k       
best.err 

#knn PLOT example
x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid( x = x , y = y)
knn.grid <- knn(df[,1:2], test = xy, df[,3], best.k) #on xy data
z  <- as.numeric(knn.grid)

x11()
plot(df[,1:2], pch=20)
points(high, col=2, pch=20)
points(low, col=3, pch=20)
legend("topright", legend=levels(as.factor(df[,3])), fill=c(2,3,4))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)

#SVM (support vector machine)
#example
svmfit <- svm(as.factor(norm_abnorm) ~ incidence + tilt, data=ort , kernel ='linear', cost =0.1, scale =FALSE )
plot(svmfit , ort, col =c('salmon', 'light blue'), pch=19, asp=1)
predict(svmfit, x.new)


#----------------------------------------------------------------------






###CLUSTERING###
#----------------------------------------------------------------------

#distances and dendogram
dist.data <- dist(data,'euclidian')
dendogram <- hclust(dist.data, 'complete' )
#linkage types:
#'single'
#'average'
#'ward.D2'

#plot dendogram
plot(dendogram, labels = FALSE)
rect.hclust(dendogram, k=k)

#generate clusters
clusters.data <- cutree(dendogram, k=k)
clust1 <- data[which(clusters.data == 1),]
clust2 <- data[which(clusters.data == 2),]
clust3 <- data[which(clusters.data == 3),]
  #clusters parameters
  dim(clust1)[1]
  dim(clust2)[1]
  dim(clust3)[1]
  colMeans(clust1)
  colMeans(clust2)
  colMeans(clust3)
  
#plot clusters and means of clusters
plot(data, col = clusters.data+1, pch=19)
points(colMeans(clust1)[1], colMeans(clust1)[2], pch = '+', cex = 2, col = 'red')
points(colMeans(clust2)[1], colMeans(clust2)[2], pch = '+', cex = 2, col = 'green')
points(colMeans(clust3)[1], colMeans(clust3)[2], pch = '+', cex = 2, col = 'blue')


#----------------------------------------------------------------------

###FDA (FUNCTIONAL DATA ANALYSIS)###
#----------------------------------------------------------------------
matplot(data, type='l')
#NB the functional data are columns


#1 fourier basis for periodical events
basis <- create.fourier.basis(c(min, max), nbasis)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, functional.data, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef
#or
basis <- create.fourier.basis(c(1,365), nbasis)
Xsp0 <- Data2fd(argvals = abscissa, y=functional.data, basisobj = basis)
est_coef <- Xsp0$coefs

#1.1 plot
plot(abscissa,functional.datum,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
#or (using Data2fd)
plot(Xsp0, col='red', lwd=2)


#2 bspline 
basis <- create.bspline.basis(c(min, max), nbasis, norder)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, functional.data, intercept=FALSE)$coef
Xsp0 <- basismat %*% est_coef
#or
basis <- create.bspline.basis(c(1,365), nbasis, norder)
Xsp0 <- Data2fd(argvals = abscissa, y=functional.data, basisobj = basis)
est_coef <- Xsp0$coefs

#2.1 plot
plot(abscissa,functional.datum,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
#or (using Data2fd)
plot(Xsp0, col='red', lwd=2)


#3 nbasis crossvalidation
#3.1 fourier
nbasis <- min:max
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(c(min , max), nbasis[i])
  gcv[i] <- smooth.basis(argvals = abscissa, y = functional.data, basis)$gcv
}
plot(nbasis,gcv)
nbasis_min <- nbasis[which.min(gcv)]

#3.1 spline
nbasis <- min:max
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(min , max), nbasis[i])
  gcv[i] <- smooth.basis(argvals = abscissa, y = functional.data, basis)$gcv
}
plot(nbasis,gcv)
nbasis_min <- nbasis[which.min(gcv)]


#4 first and second derivatives of observed data
NT <- length(abscissa)
rappincX1.data <- (datum[3:NT]-datum[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincX2.data <- ((datum[3:NT]-datum[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-
                     (datum[2:(NT-1)]-datum[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])
#first and second derivative of smoothed data
basismat1 <- eval.basis(abscissa, basis,Lfdobj=1)
Xsp1 <- basismat1 %*% lsfit(basismat, functional.data, intercept=FALSE)$coef

basismat2 <- eval.basis(abscissa, basis,Lfdobj=2)
Xsp2 <- basismat2 %*% lsfit(basismat, functional.data, intercept=FALSE)$coef


#4.1 plotting derivatives
plot(abscissa[2:(NT-1)],rappincX1.data,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)

plot(abscissa[2:(NT-1)],rappincX2.data,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xsp2 ,type="l",col="blue",lwd=2)


#FPCA functional principal components analysis
#example
data_W.fd.1 <- Data2fd(y = t(temperature[,-366]),argvals = abscissa,basisobj = basis)
pca.temp <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)
par(mfrow=c(1,3))
plot(pca.temp$harmonics[1,])
abline(h=0)
plot(pca.temp$harmonics[2,])
abline(h=0)
plot(pca.temp$harmonics[3,])
abline(h=0)
plot(cumsum(pca.temp$values[1:13])/sum(pca.temp$values), ylim=c(0,1))

#fpca scores
plot(pca.temp$scores[,1:2], col=as.factor(temperature$country))
abline(h=0, v=0)
#fpca scores of the first pc as a function of abscissa
plot(abscissa, scores.PC1, type = 'l', xlab = 'day')


#----------------------------------------------------------------------

###GEOSTATISTICS / SPATIAL STATISTICS ###
#----------------------------------------------------------------------
#1 set coordinates
coordinates(data) <- c('x','y')

#2 variogram
vgram <- variogram(z ~ 1, data= data) #just the intercept (STATIONARY MODEL)
vgram <- variogram(z ~ features, data= data) #(NOT STATIONARY MODEL)
plot(vgram)
plot(vgram, vgm(psill, "Sph", range, nugget))
v.fit <- fit.variogram(vgram, vgm(psill, "Sph", range, nugget))

#3 parameter estimation 
#3.1 just intercept (stationary model) z = a0
g.tr <- gstat(formula = formula, data = colours, model = v.fit)
g.tr
a0 <- predict(g.tr, data[1,], BLUE = T)$var1.pred

#3.2 intercept and groups z = a0.g
#example
    #(g = 1 for yellow, g = 2 for orange, g = 3 for red)
    d.yel <- ifelse(colours$colour=='yellow', 1, 0)
    d.or <- ifelse(colours$colour=='orange', 1, 0)
    D <- data.frame(d.yel = d.yel, d.or = d.or)
    colours$colour <- D
    
    vgram2 <- variogram(revenue ~ colour.d.yel + colour.d.or , data= colours)
    plot(vgram2)
    plot(vgram2, vgm(6, "Sph", 2500))
    v.fit2 <- fit.variogram(vgram2, vgm(6, "Sph", 2500))
    
g.tr2 <- gstat(formula=revenue ~ colour.d.yel + colour.d.or , data= colours, model = v.fit2)
g.tr2
a0.y <- predict(g.tr2, colours[2,], BLUE = T)$var1.pred
a0.o <- predict(g.tr2, colours[3,], BLUE = T)$var1.pred
a0.r <- predict(g.tr2, colours[1,], BLUE = T)$var1.pred

#3.3 intercept and features
#example log[y(si)] = a0 + a1 · log[x(si)] + δ(si)
v <- variogram(log(sights) ~ log.chlorofill, walesharks)
v.fit <- fit.variogram(v, vgm(0.6,'Exp', 70000))
g.stat <- gstat(formula = log(sights) ~ log.chlorofill,
                data = walesharks, nmax = 50, model=v.fit)
zetas <- predict(g.stat, walesharks[1:2,], BLUE = TRUE)$var1.pred
regressors <- walesharks$log.chlorofill[1:2]
A <- rbind(c(1, regressors[1]), c(1, regressors[2]))
coefs <- solve(A)%*%zetas


###MATRIX OP ###
matrix.inv <- solve(matrix)
matrix_at_minus_1_half <- sqrtm(solve(matrix))


