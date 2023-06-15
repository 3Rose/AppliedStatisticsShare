setwd('C:/Users/Michele/Desktop/compiti AS/19_01_22')

df = read.table('fish.txt',head=TRUE)
head(df)

high <- df[df$abundance=='H',1:2]
low <- df[df$abundance=='L',1:2]
pool <- df[,1:2]

n1 <- length(high[,1])
n2 <- length(low[,1])
n <- n1+n2
g <- 2
p <- length(low[1,])

#FISHER DISCRIMINANT ANALYSIS
m1 <- colMeans(high)
m2 <- colMeans(low)
m <- colMeans(pool)
S1 <-  cov(high)
S2 <-  cov(low)
Spool  <- ((n1-1)*S1+(n2-1)*S2)/(n-g) #covariance within

B <- 1/(g-1)*(cbind(m1 - m) %*% rbind(m1 - m) +
                cbind(m2 - m) %*% rbind(m2 - m)) #covariance btwn

#Matrix Sp^(-1/2)
val.Sp <- eigen(Spool)$val
vec.Sp <- eigen(Spool)$vec
invSp.2 <- 1/sqrt(val.Sp[1])*vec.Sp[,1]%*%t(vec.Sp[,1]) + 
  1/sqrt(val.Sp[2])*vec.Sp[,2]%*%t(vec.Sp[,2]) #the inverse is obtained by doing the inverse of eigenv.

#spectral decomposition of Sp^(-1/2) B Sp^(-1/2)
spec.dec <- eigen(invSp.2 %*% B %*% invSp.2)

#canonical coordinates (along which you maximize separation between groups)
a1 <- invSp.2 %*% spec.dec$vec[,1]
a2 <- invSp.2 %*% spec.dec$vec[,2]

df1_new <- as.matrix(pool)%*%a1
df2_new <- as.matrix(pool)%*%a2
df_new <- cbind(df1_new,df2_new) #here maximum separation

#projection of the mean: distance from this mean will determine classification
m1_new <- c(m1%*%a1, m1%*%a2)
m2_new <- c(m2%*%a1, m2%*%a2)

#CLASSIFICATION
f.class=rep(0, n)
for(i in 1:n) # for each datum
{
  dist.m=c(d1=sqrt(sum((df_new[i,]-m1_new)^2)), #distance datum-mean
           d2=sqrt(sum((df_new[i,]-m2_new)^2)))
  f.class[i]=which.min(dist.m) #1 if d(x,m1)<d(x,m2)
}
f.class
table(class.true=df[,3], class.assigned=f.class)

#PLOT
color <- df[,3]
levels(color) <- c('red','blue')

x11()
plot(df1_new, df2_new, main='Fisher discriminant analysis', xlab='first canonical coordinate', 
     ylab='second canonical coordinate', pch=20, col=as.factor(color)) #DATA

x.cc  <- seq(min(df1_new),max(df1_new),len=200) #GRID
y.cc  <- seq(min(df2_new),max(df2_new),len=200)
xy.cc <- expand.grid(cc1=x.cc, cc2=y.cc)

z  <- cbind(sqrt(rowSums(scale(xy.cc,m1_new,scale=FALSE)^2)), sqrt(rowSums(scale(xy.cc,m2_new,scale=FALSE)^2)))
z1.cc <- z[,1] - pmin(z[,2])    
z2.cc <- z[,2] - pmin(z[,1])    
# z3.cc <- z[,3] - pmin(z[,1], z[,2])

contour(x.cc, y.cc, matrix(z1.cc, 200), levels=0, drawlabels=F, add=T)
contour(x.cc, y.cc, matrix(z2.cc, 200), levels=0, drawlabels=F, add=T)
#contour(x.cc, y.cc, matrix(z3.cc, 200), levels=0, drawlabels=F, add=T)