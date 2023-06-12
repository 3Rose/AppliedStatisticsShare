### LDA UNIVARIATE 2 CLASSES (ONLY INFG)
###----------------------------------
rm(list = ls())
setwd("C:/Users/Michele/Desktop/Applied Statistics Labs/Applied Stats Code/Classification")
cyto <- read.table('cytokines.txt', header=T)
attach(cyto)

# Group A: favorable clinical outcome (the treatment has effect)
A <- which(group == 'A')
# Group B: unfavorable clinical outcome (the treatment has no effect)
B <- which(group == 'B')

### Idea: we aim to find a "rule" to classify patients as Group A or Group B
### given the measurements of Inf-g (Interferon gamma) and IL-5 (Interleukin 5).

### LDA: ASSUMPTIONS
###----------------------------------
# Assumptions:
# 1) if L = i, X.i ~ N(mu.i, sigma.i^2), i = A,B
# 2) sigma.A = sigma.B
# 3) c(A|B) = c(B|A) (equal misclassification costs)

# 1) normality (univariate) within the groups
shapiro.test(cyto[A,1]) #ok
shapiro.test(cyto[B,1]) #ok


# 2) equal variance (4 univariate use var.test)
var.test(cyto[A,1], cyto[B,1]) #large p-value -> equal

### LDA: GO
###----------------------------------
library(MASS)
cyto.lda <- lda(group ~ Infg)
cyto.lda

### LDA: TESTING AND QUALITY
###----------------------------------
x <- data.frame(Infg = 0) #query
predict(cyto.lda, x)$class #predicted class
predict(cyto.lda, x)$posterior #posterior probs 4 every class
predict(cyto.lda, x)$x #Fisher's discriminant scores


### LDA: TESTING, FOCUS ON POSTERIORS
###----------------------------------
x <- data.frame(Infg=seq(-10, 35, 0.5))

cyto.LDA.A <- predict(cyto.lda, x)$posterior[,1] # posterior probability for class A
cyto.LDA.B <- predict(cyto.lda, x)$posterior[,2] # posterior probability for class B
predict(cyto.lda, x)$class 

lines(x[,1], cyto.LDA.A, type='l', col='blue', xlab='x', ylab='estimated posterior', main="LDA")
lines(x[,1], cyto.LDA.B, type='l', col='red')
legend(-10, 0.9, legend=c('P(A|X=x)', 'P(B|X=x)'), fill=c('blue','red'), cex = 0.7)

dev.off()

### LDA: GO, BY SETTING PRIOR PROBS
###----------------------------------
cyto.lda.1 <- lda(group ~ Infg, prior=c(0.95,0.05))
cyto.lda.1

#testing
x <- data.frame(Infg=seq(-10, 35, 0.5)) # query
cyto.LDA.A.1 <- predict(cyto.lda.1, x)$posterior[,1] # posterior probability for class A
cyto.LDA.B.1 <- predict(cyto.lda.1, x)$posterior[,2] # posterior probability for class B

x11()
plot  (x[,1], cyto.LDA.A.1, type='l', col='blue', xlab='x', ylab='estimated posterior', main="LDA", ylim=c(0,1))
points(x[,1], cyto.LDA.B.1, type='l', col='red')
legend(-10, 0.9, legend=c('P(A|X=x)', 'P(B|X=x)'), fill=c('blue','red'), cex = 0.7)
points(Infg[A], rep(0, length(A)), pch=16, col='blue')
points(Infg[B], rep(0, length(B)), pch=16, col='red')

points(x[,1], cyto.LDA.A, type='l', col='grey') #without prior
points(x[,1], cyto.LDA.B, type='l', col='grey') #without prior

