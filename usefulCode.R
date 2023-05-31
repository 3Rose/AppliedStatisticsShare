


###INFERENCE###
#------------

#1 univariate
#Assume gaussianity of data_samples

# automatic
t.test(data_samples, mu = mean.H0, alternative = 'two.sided', conf.level = 1-alpha)

# manual
tstat <- (sample.mean - mean.H0) / (sample.sd / sqrt(n))
cfr.t <- qt(1 - alpha/2, n-1)
abs(tstat) < cfr.t
pvalue  <- ifelse(tstat >= 0, (1 - pt(tstat, n-1))*2, pt(tstat, n-1)*2)

#1.2 confidence intervals
ci <- c(
      sample.mean - qt(1 - alpha/2, n-1) * (sample.sd / sqrt(n)),
      sample.mean,
      sample.mean + qt(1 - alpha/2, n-1) * (sample.sd / sqrt(n))
)


#2 multivariate
# n large => use chi squared
# n small and gaussian samples => use Fisher

#manual
T2stat <- n * (sample.mean-mu0) %*% inv.sample.cov %*% (sample.mean-mu0) 
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p) # gaussian samples
cfr.chisq <- qchisq(1-alpha, p) # not gaussian samples but n large
pvalue <- 1 - pf(x.T2*(n-p) / ((n-1)*p), p, n-p)

#2.1 confidence region
ellipse(sample.mean, sample.cov/n, sqrt(cfr.fisher)) #or
ellipse(sample.mean, sample.cov/n, sqrt(cfr.chisq))

#2.2 one-at-the-time confidence intervals
ci <- c(
      rbind(a)%*%sample.mean - qt(1-alpha/2,n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n),
      rbind(a)%*%sample.mean,
      rbind(a)%*%sample.mean + qt(1-alpha/2,n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n)
)
#for the component i
ci_i <- c(
      sample.mean[i] - qt(1-alpha/2,n-1) * sqrt( sample.cov[i,i] / n),
      sample.mean[i],
      sample.mean[i] + qt(1-alpha/2,n-1) * sqrt( sample.cov[i,i] / n)
)

#2.2 simultaneous confidence intervals
#compute for all components
sci <- cbind(inf = sample.mean - sqrt(cfr.fisher*diag(sample.cov)/n),
            center = sample.mean, 
            sup = sample.mean + sqrt(cfr.fisher*diag(sample.cov)/n))
#for one component (i)
sci_i <- c(
      sample.mean[i] - sqrt(cfr.fisher * sample.cov[i,i]/n),
      sample.mean[i],
      sample.mean[i] + sqrt(cfr.fisher * sample.cov[i,i]/n)
)
# note that cfr.fisher contains ((n-1)*p)/(n-p)

#2.3 Bonferroni confidence intervals
k <- k
bci <- c(
  rbind(a)%*%sample.mean - qt(1-alpha/(2*k),n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n),
  rbind(a)%*%sample.mean,
  rbind(a)%*%sample.mean + qt(1-alpha/(2*k),n-1) * sqrt( (rbind(a)%*%sample.cov%*%cbind(a)) / n)
)
#for the component i
ci_i <- c(
  sample.mean[i] - qt(1-alpha/(2*k),n-1) * sqrt( sample.cov[i,i] / n),
  sample.mean[i],
  sample.mean[i] + qt(1-alpha/(2*k),n-1) * sqrt( sample.cov[i,i] / n)
)
