dfa = read.table('acoruna.txt',head=TRUE)
dfp = read.table('pontevedra.txt',head=TRUE)
head(dfa)
head(dfp)

#POINT A
#MANUAL TEST FOR THE MEAN: g=2, p=2
alpha <- 0.01
n1 <- length(dfa[,1])
n2 <- length(dfp[,1])
g <- 2
p <- length(dfa[1,])

Spool <- ((n1-1)*cov(dfa)+(n2-1)*cov(dfp))/(n1+n2-2) #wishart
SpoolInv <- solve(Spool) #inverse

mean_a <- sapply(dfa,mean)
mean_p <- sapply(dfp,mean)

To <- (n1*n2)/(n1+n2) * (mean_a-mean_p)%*%SpoolInv%*%(mean_a-mean_p)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

To > cfr.fisher #yes, reject H0, difference btwn means

P <- 1 - pf(To/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P #p-value

#ALTERNATIVE WITH MANOVA FUNCTION
df <- rbind(dfa,dfp)
df <- cbind(df,rep('A',60))
df[31:60,3] <- 'P'
names(df)[3] <- 'City'

fit <- manova(as.matrix(df[,1:2]) ~ df$City)
summary.manova(fit, test="Wilks") #same p-value moreover