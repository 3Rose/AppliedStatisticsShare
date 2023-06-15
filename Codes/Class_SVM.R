#POINT E
library(e1071)
df <- read.table('musicCountry.txt',header=TRUE)
head(df)

gr <- factor(df$release.country, levels=c('Germany', 'US'))

#BUILDING DATASET WITH NUMERICAL TARGET
y <- rep(0,nrow(df))
y[which(gr=='US')] <- 1
df <- data.frame(x=df[,1:2], y=as.factor (y))

#SVM
set.seed(1)
#tuning
tune.out <- tune(svm,y~.,data=df ,kernel = 'linear',
                 ranges=list(cost=c(0.001 , 0.01, 0.1, 1,10,100) ), 
                 tunecontrol = tune.control(cross = 10), scale = F)
summary(tune.out)

#best model
best <- tune.out$best.model

best$cost #chosen cost

x11() #plot classification
plot(best, df, col =c('salmon', 'light blue'), pch=19, asp=1,xlim=c(0,20))

query <- data.frame(x.price = 50, x.average.length = 3.5)
prediction <- predict(best,query)
prediction #as US
