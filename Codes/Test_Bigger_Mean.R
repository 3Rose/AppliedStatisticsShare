dfa = read.table('acoruna.txt',head=TRUE)
dfp = read.table('pontevedra.txt',head=TRUE)
head(dfa)
head(dfp)

n1 <- nrow(dfa)
n2 <- nrow(dfp)
n <- n1 + n2

alpha <- 0.01

mean_a <- colMeans(dfa)
mean_p <- colMeans(dfp)

avg_a <- mean(mean_a)
avg_p <- mean(mean_p)

avg <- c(avg_a, avg_p)

Spool <- ((n1 - 1) * cov(dfa) + (n2 - 1) * cov(dfp)) / (n1 + n2 - 2)

a <- c(1, -1)
mu0 <- 0

pivotal <- sqrt(n) * (avg[1] - avg[2] - mu0) / sqrt(t(a) %*% Spool %*% a)

cfr.t = qt(1-alpha/2, n-1)     
pivotal > -cfr.t   # Q: accettiamo?   <- caso bilaterale: H0: > mu0
pivotal < cfr.t   # Q: accettiamo?   <- caso bilaterale: H0: < mu0

P = pt(pivotal, n-1)  # test unilaterale H0: > mu0
P
P = 1 - pt(pivotal, n-1)      # test unilaterale H0: < mu0
P
