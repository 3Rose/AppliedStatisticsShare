df <- read.table('musicCountry.txt',header=TRUE)
head(df)

gr <- factor(df$release.country, levels=c('Germany', 'US'))
pr = c(0.1 , 0.9) #prior probabilities
#AERCV
qda_cv <- qda(df[,1:2], gr, prior = pr, CV=TRUE)

errorsqCV <- (qda_cv$class != df[,3])
AERqCV   <- sum(errorsqCV)/length(df[,3])
AERqCV
