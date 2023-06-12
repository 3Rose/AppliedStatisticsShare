### LDA BIVARIATE 3 CLASSES 
###----------------------------------
rm(list = ls())
attach(iris)

species.name <- factor(Species)

g = 3 

i1 <- which(species.name == 'setosa')
i2 <- which(species.name == 'versicolor')
i3 <- which(species.name == 'virginica')

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1 + n2 + n3

detach(iris)

iris2 <- iris[,1:2]

# PERTURBATE
set.seed(1)
iris2 <- iris2 + cbind(rnorm(150, sd=0.025))    

# Linear Discriminant Analysis (LDA)
lda.iris <- lda(iris2, species.name)

Lda.iris <- predict(lda.iris, iris2)
names(Lda.iris)

### LDA: APER
###----------------------------------
table(class.true=species.name, class.assigned=Lda.iris$class)

errors <- (Lda.iris$class != species.name)
APER   <- sum(errors)/length(species.name)
APER

### LDA: AER CV
###----------------------------------
LdaCV.iris <- lda(iris2, species.name, CV=TRUE)  # specify the argument CV

table(class.true=species.name, class.assignedCV=LdaCV.iris$class)
errorsCV <- (LdaCV.iris$class != species.name)

AERCV   <- sum(errorsCV)/length(species.name)
AERCV


### LDA: PLOT THE PARTITION
###----------------------------------
x11()
plot(iris2, main='Iris Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)
points(iris2[i3,], col='blue', pch=20)
legend("topright", legend=levels(species.name), fill=c('red','green','blue'), cex=.7)

points(lda.iris$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5)

x  <- seq(min(iris[,1]), max(iris[,1]), length=200)
y  <- seq(min(iris[,2]), max(iris[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)

z  <- predict(lda.iris, xy)$post  #since we have 3 classes 
z1 <- z[,1] - pmax(z[,2], z[,3])   
z2 <- z[,2] - pmax(z[,1], z[,3])    
z3 <- z[,3] - pmax(z[,1], z[,2]) 

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
