#POINT C 19_01_22
df = read.table('fish.txt',head=TRUE)
head(df)

library(class)
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

#PLOT
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
