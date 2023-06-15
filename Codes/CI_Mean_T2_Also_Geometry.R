df = read.table('shopping.txt',head=TRUE)
head(df)

#POINT A 
#Build a confidence region (level 95%) for the mean
#Obs: 24 small, use T^2
n <- length(df[,1])
p <- length(df[1,])

m <- colMeans(df)
S <- cov(df)

alpha = 0.05

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

T2 <- cbind(inf = m - sqrt(cfr.fisher*diag(S)/n),
            center = m, 
            sup = m + sqrt(cfr.fisher*diag(S)/n))
T2

m #center
eigen(S/n)$vectors #direction of axes

r <- sqrt(cfr.fisher)
r*sqrt(eigen(S/n)$values) #length of semi-axes of ellipse

