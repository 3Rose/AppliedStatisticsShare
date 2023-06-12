
#_______________________________________________________________________________
##### Two-ways MANOVA: FRAMEWORK
##### (p=3, g=2, b=2)
###------------------------
# The optimum conditions for extruding plastic films have been 
# examined using a technique called Evolutionary Operation. In 
# the course of the study, three responses were measured:
# X.1 = Tear Resistance
# X.2 = Gloss
# X.3 = Opacity
# at two levels of the factors:
# factor.1 = Rate of Extrusion     (0=Low level, 1=High level)
# factor.2 = Amount of an Additive (0=Low level, 1=High level)
# The measurements were repeated n=5 times at each combination 
# of the factor levels.

plastic <- read.table('T6-4.dat',col.names=c('Ex','Ad','Tr','Gl','Op'))
plastic

Ex   <- factor(plastic$Ex, labels=c('L','H')) # Treat.1
Ad   <- factor(plastic$Ad, labels=c('L','H')) # Treat.2

ExAd <- Ex
levels(ExAd) <- c('LL','LH','HL','HH')
ExAd[Ex=='L' & Ad=='L'] <- 'LL'
ExAd[Ex=='L' & Ad=='H'] <- 'LH'
ExAd[Ex=='H' & Ad=='L'] <- 'HL'
ExAd[Ex=='H' & Ad=='H'] <- 'HH'

plastic3  <- plastic[,3:5]

#_______________________________________________________________________________
##### Two-ways MANOVA: ASSUMPTIONS
# 1) normality (multivariate) in each group (4 test)
Ps <- c(mvn(plastic3[ExAd==levels(ExAd)[1],])$multivariateNormality$`p value`,
        mvn(plastic3[ExAd==levels(ExAd)[2],])$multivariateNormality$`p value`,
        mvn(plastic3[ExAd==levels(ExAd)[3],])$multivariateNormality$`p value`,
        mvn(plastic3[ExAd==levels(ExAd)[4],])$multivariateNormality$`p value`)
Ps

# 2) homogeneity of the covariance (qualitatively)
S1 <-  cov(plastic3[ ExAd==levels(ExAd)[1],])
S2 <-  cov(plastic3[ ExAd==levels(ExAd)[2],])
S3 <-  cov(plastic3[ ExAd==levels(ExAd)[3],])
S4 <-  cov(plastic3[ ExAd==levels(ExAd)[4],])

x11()
par(mfrow=c(1,4))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))
image(S4, col=heat.colors(100),main='Cov. S4', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3,S4), (0:100)/100, na.rm=TRUE))

dev.off()

#_______________________________________________________________________________
##### Two-ways MANOVA: GO

### Model with interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N_p(0,Sigma), [p=3]
###     i=1,2 (effect Extrusion), j=1,2 (effect Additive),
###     X.ijs, mu, tau.i, beta.j, gamma.ij in R^3
fit <- manova( as.matrix(plastic3) ~ Ex + Ad + Ex:Ad)
summary.manova(fit, test="Wilks")

### Model without interaction (additive model): 
### X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N_p(0,Sigma), [p=3]
###     i=1,2 (effect Extrusion), j=1,2 (effect additive),
###     X.ijs, mu, tau.i, beta.j, in R^3
fit2<- manova( as.matrix(plastic3) ~ Ex + Ad)
summary.manova(fit2, test="Wilks")

# Both the treatments have a significant effect on the mean (but not
# their interaction, that we could remove)

#_______________________________________________________________________________
##### Two-ways MANOVA: INSPECTION SINGLE VARS, ANOVA
# Let's verify if this is true for all the variables through appropriate
# conf. int. and tests on the components:
summary.aov(fit2)

# Bonferroni
alpha <- 0.05
g <- 2
b <- 2
p <- 3
n <- 5
N <- n*g*b # 20

W <- summary.manova(fit2)$SS$Residuals

# how many comparisons?
k <- g*(g-1)/2*p + b*(b-1)/2*p
# because we have: g levels on the first treatment on p components
#                  b levels on the second treatment on p components
k

qT <- qt(1 - alpha / (2 * k), g*b*n-g-b+1)
# the degrees of freedom of the residuals on the additive model are
# g*b*n-g-b+1

mExL  <- sapply(plastic3[Ex=='L',],mean)
mExH  <- sapply(plastic3[Ex=='H',],mean)
infEx <- mExH-mExL - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )
supEx <- mExH-mExL + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )

mAdL  <- sapply(plastic3[Ad=='L',],mean)
mAdH  <- sapply(plastic3[Ad=='H',],mean)
infAd <- mAdH-mAdL - qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )
supAd <- mAdH-mAdL + qT * sqrt( diag(W)/(g*b*n-g-b+1) * (1/10+1/10) )

IC2   <- list(ExH_ExL=cbind(infEx, supEx), AdH_AdL=cbind(infAd, supAd))
IC2

