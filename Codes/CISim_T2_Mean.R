df = read.table('shopping.txt',head=TRUE)
head(df)

n <- length(df[,1])
p <- length(df[1,])

m <- colMeans(df)
S <- cov(df)

alpha = 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

#POINT B 
#Build four T2-simultaneous confidence intervals (level 95%) for: 
#1-the mean number of accesses to the onlinestore, 
a1 <- c(1,0,0)
#2-the mean number of purchases of men’s clothing, 
a2 <- c(0,1,0)
#3-the mean number of purchases of women’s clothing and
a3 <- c(0,0,1)
#4-the mean number of total purchases.
a4 <- c(0,1,1)

T2_1 = cbind(inf = t(a1)%*%m - sqrt(cfr.fisher*t(a1)%*%(S)%*%a1/n),
             center = t(a1)%*%m, 
             sup = t(a1)%*%m + sqrt(cfr.fisher*t(a1)%*%(S)%*%a1/n))
T2_1

T2_2 = cbind(inf = t(a2)%*%m - sqrt(cfr.fisher*t(a2)%*%(S)%*%a2/n),
             center = t(a2)%*%m, 
             sup = t(a2)%*%m + sqrt(cfr.fisher*t(a2)%*%(S)%*%a2/n))
T2_2

T2_3 = cbind(inf = t(a3)%*%m - sqrt(cfr.fisher*t(a3)%*%(S)%*%a3/n),
             center = t(a3)%*%m, 
             sup = t(a3)%*%m + sqrt(cfr.fisher*t(a3)%*%(S)%*%a3/n))
T2_3

T2_4 = cbind(inf = t(a4)%*%m - sqrt(cfr.fisher*t(a4)%*%(S)%*%a4/n),
             center = t(a4)%*%m, 
             sup = t(a4)%*%m + sqrt(cfr.fisher*t(a4)%*%(S)%*%a4/n))
T2_4

