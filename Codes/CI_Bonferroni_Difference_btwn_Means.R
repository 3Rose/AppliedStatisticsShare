#BONFERRONI INTERVAL: DIFFERENCE BTWN MEANS
setwd('C:/Users/Michele/Desktop/Codes')
dfa = read.table('acoruna.txt',head=TRUE)
dfp = read.table('pontevedra.txt',head=TRUE)
head(dfa)
head(dfp)

alpha <- 0.1
n1 <- length(dfa[,1])
n2 <- length(dfp[,1])
g <- 2
p <- length(dfa[1,])

Spool <- ((n1-1)*cov(dfa)+(n2-1)*cov(dfp))/(n1+n2-2) 
mean_a <- sapply(dfa,mean)
mean_p <- sapply(dfp,mean)

a <- c(1,-1) #differences btwn means
k <- length(a) #number of lin.comb.
inf <- mean_a-mean_p - sqrt(t(a)%*%Spool%*%a*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2)
sup <- mean_a-mean_p + sqrt(t(a)%*%Spool%*%a*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2)

CI <- list(g12=cbind(inf, mean_a-mean_p,sup))
CI
