df = read.table('shopping.txt',head=TRUE)
head(df)

#POINT C
# Perform a test of level 95% to verify the hypothesis according to which, 
#in mean, more than 20% of the accesses to the online store result in a purchase.

n <- length(df[,1])
p <- length(df[1,])

m <- colMeans(df)
S <- cov(df)

alpha = 0.05



a <- c(-0.2,1,1)
mu0 = 0

p <- 1 - pt(t(a)%*%m*sqrt(n/t(a)%*%S%*%a),n-1)
p
1-p
