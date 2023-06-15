library(rgl)
df = read.table('students.txt',head=TRUE)
head(df)
library(glmnet)

C <- df$gender
females <- which(C=='female')
males <- which(C=='male')

C1 <- rep(0,length(C))
C1[males] <- 1
C1

df$watchtv -> tv
df$age -> age
df$height -> heig
df$distance -> dist
df$siblings -> sib
df$computertime -> comp
df$exercisehours -> ex
df$musiccds -> mu
df$playgames -> ga

#POINT B: FIXED LAMBDA
l2 <- glmnet(cbind(C1,df[,2:9]),tv,lambda=0.3)
l2$beta #only age,distance,siblings,computertime,musiccd,playgame has influence

#POINT C: CV
lambda.grid <- seq(0.01,1,length=1000)
cv.lasso <- cv.glmnet(as.matrix(cbind(C1,df[,2:9])),tv,lambda=lambda.grid) 

bestlam <- cv.lasso$lambda.min
bestlam 

l3 <- glmnet(cbind(C1,df[,2:9]),tv,lambda=bestlam)
l3$beta


#POINT D: PREDICTION
#gender=male, age=21, height=73,distance=100, siblings=1, computertime=10, 
#exercisehours=2, musiccds=35, playgames=4.

query = rbind(C1 = 1,age=21,height=73,distance=100,siblings=1,computertime=10,
              exercisehours=2, musiccds=35, playgames=4)

prediction <- predict(l3,query[,1])
prediction
