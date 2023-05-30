#Problem n.1
#The file kimono.txt collects the value (ke) of 528 silk kimonos sold in Mitsukoshi department stores in Kyoto and
#Tokyo, both tailor-made and ready-to-wear.
#a) Formulate an ANOVA model for the value of a kimono as a function of the factors city (Kyoto or Tokyo) and
#type (tailor-made, ready-to-wear). Verify the assumptions of the model.
#b) Through appropriate statistical tests, propose a reduced model.
#c) Provide Bonferroni intervals (global level 95%) for the differences between the mean value of kimonos belonging
#to the homogeneous groups identified by the model at point (b).

kimono <- read.table("kimono.txt")
head(kimono)
help("aov")
attach(kimono)
kimono.am <- aov(value ~ as.factor(city) + as.factor(type) + as.factor(city):as.factor(type))
summary(kimono.am)
g <- length(levels(as.factor(city)))
b <- length(levels(as.factor(type)))
n <- length(value)/(g*b)
N <- length(value)

P <- c(
    shapiro.test(value[which(city == levels(as.factor(city))[1] & type == levels(as.factor(type))[1])])$p,
    shapiro.test(value[which(city == levels(as.factor(city))[1] & type == levels(as.factor(type))[2])])$p,
    shapiro.test(value[which(city == levels(as.factor(city))[2] & type == levels(as.factor(type))[1])])$p,
    shapiro.test(value[which(city == levels(as.factor(city))[2] & type == levels(as.factor(type))[2])])$p
    )
P
#rigetto per ogni combinazione dei gruppi l'ipotesi di gaussianità

s11 <- var(value[which(city == levels(as.factor(city))[1] & type == levels(as.factor(type))[1])])
s12 <- var(value[which(city == levels(as.factor(city))[1] & type == levels(as.factor(type))[2])])
s21 <- var(value[which(city == levels(as.factor(city))[2] & type == levels(as.factor(type))[1])])
s22 <- var(value[which(city == levels(as.factor(city))[2] & type == levels(as.factor(type))[2])])
round(s11, digits = 1)
round(s12, digits = 1)
round(s21, digits = 1)
round(s22, digits = 1) #molto bene per quanto riguarda le varianze
bartlett.test(value, as.factor(city):as.factor(type))


#b)
summary(kimono.am)
#notiamo ce 0.335 è un p-value molto alto = > eseguo un test statistico per capire
# se può essere 0
library(mvtnorm)
library(MASS)
library(car)
linearHypothesis(kimono.am, c(0,1,0,0), c(0))

#quindi
kimono.am2 <- aov(value ~ type + city:type)
summary(kimono.am2)
#anche il termine interazione sembra insgnificante

fit.am <- aov(value ~ type)
summary(fit.am)


#c)
hm.mean <- mean(value[which(type == 'hand-made')])
rtu.mean <- mean(value[which(type == 'ready-to-use')])
S <- sum((fit.am$residuals)^2)/fit.am$df.residual
n.hm <- length(value[which(type == 'hand-made')])
n.rtu <- length(value[which(type == 'hand-made')])
# entrambe gaussiane per il teo centr del lim
CI <- c(
      hm.mean-rtu.mean - sqrt(S*(1/n.hm +1/n.rtu))*qt(1-0.05/2,n.hm+n.rtu-2),
      hm.mean-rtu.mean + sqrt(S*(1/n.hm +1/n.rtu))*qt(1-0.05/2,fit.am$df.residual)
)
print(CI)


#Having picnics in parks is very common in Japan, especially for the traditional custom of Hanami (flower viewing)
#during the bloom of cherry blossoms. The file bento.txt contains the total amount [g] of rice, sashimi, vegetables
#and okashi (traditional sweets) in the bentō’s (packed lunches) of 32 volunteer families, consumed on March 26th
#2017 (for Hanami) and on May 7th 2017 (normal season). Assuming independent the composition of bentō’s of
#different families and not independent that of the same family, answer the following question.
#a) Perform a statistical test to verify if there is evidence of an impact of Hanami on the mean amount of rice,
#sashimi, vegetables and okashi in families bentō’s. Verify the needed assumptions.
#b) Provide four T 2 simultaneous confidence intervals (global confidence 95%) for the increase in the mean con-
#  sumption of rice, sashimi, vegetables and okashi in correspondence of the bloom of cherry blossoms. Comment
#the results.
library(MVN)
#a) Perform a statistical test to verify if there is evidence of an impact of Hanami on the mean amount of rice,
#sashimi, vegetables and okashi in families bentō’s. Verify the needed assumptions.
bento <- read.table("bento.txt")
head(bento)
hanami <- bento[,1:4]
nohanami <- bento[,5:8]
n1 <- dim(hanami)[1]
n2 <- dim(nohanami)[1]
m1 <- colMeans((hanami))
m2 <- colMeans(nohanami)
p <- 4
Spooled <- cov(hanami) + cov(nohanami)/2
Tstat <- (1/n1 + 1/n2)^(-1) * rbind(m1 -m2) %*% solve(Spooled) %*% cbind(m1-m2)
Fstat <- (n1 + n2 -2)*p/(n1+n2-1-p)*qf(1-0.05, p, n1+n2-1-p)
Tstat > Fstat
# true quindi rigetto l'ipotesi che siano uguali
#alternativamente
names(hanami) <- c('rice', 'sashimi', 'veg','okashi')
names(nohanami) <- c('rice', 'sashimi', 'veg','okashi')
dati <- rbind(hanami,nohanami)
dm <- rep(c(0,1), each = 32)
fit <- manova(as.matrix(dati) ~ dm)
summary(fit, test = "Wilks")
mvn(hanami)   #ok
mvn(nohanami) #ok
bartlett.test(dati, dm)
round(cov(hanami)) - round(cov(nohanami)) # non notiamo differenze negli ordini di
# grandezza quindi considerando la robustezza del test si può procedere

#b) Provide four T 2 simultaneous confidence intervals (global confidence 95%) for the increase in the mean con-
#  sumption of rice, sashimi, vegetables and okashi in correspondence of the bloom of cherry blossoms. Comment
#the results.
m.rice <- mean(hanami$rice)
m.sashimi <- mean(hanami$sashimi)
m.veg <- mean(hanami$veg)
m.okashi <- mean(hanami$okashi)
f <- sqrt((32-1)*p/(32-p) * qf(1-0.05, p, 32-p))
ci1 <- c(
      m.rice - sqrt(cov(hanami)[1,1]/32) * f,
      m.rice + sqrt(cov(hanami)[1,1]/32) * f
)
ci2 <- c(
      m.sashimi - sqrt(cov(hanami)[2,2]/32) * f,
      m.sashimi + sqrt(cov(hanami)[2,2]/32) * f
)
ci3 <- c(
  m.veg - sqrt(cov(hanami)[3,3]/32) * f,
  m.veg + sqrt(cov(hanami)[3,3]/32) * f
)
ci4 <- c(
    m.okashi -  sqrt(cov(hanami)[4,4]/32) * f,
    m.okashi + sqrt(cov(hanami)[4,4]/32) * f
)
ci1
ci2
ci3
ci4
ciRose <- c(ci1, ci2, ci3, ci4)


#The file geisha.txt collects data about Geisha hunting in Kyoto (i.e., tours finalized to spot a Geisha). The data
#report the duration (minutes) and starting time (in minutes after 16:00) of 130 trials (not all successful).
#a) Use a hierarchical clustering method based on Euclidean distance and single linkage to identify two groups of
#data (i.e., successful and unsuccessful tours). Report the centers of the clusters, the size of the clusters, the
#cophenetic coefficient and a qualitative plot of the results.
#b) Evaluate the quality of the clustering at point (a) and, in case you deem it unsatisfactory, repeat the procedure
#with another linkage at your choice.
#c) Identify the successful tours with the smaller group found with the clustering method at point (b). Having
#introduced and verified the needed assumptions, provide 4 Bonferroni intervals (global level 90%) for the dif-
#  ference in the mean characteristics of successful and unsuccessful tours, and for the mean characteristics of a
#successful tour.
#d) Comment the results at point (c) and suggest a successful strategy for Geisha hunting.
setwd("/home/rose/Desktop/exams/Exams (in English) - WINDOWS/030717/")
geisha <- read.table('geisha.txt')
head(geisha)
plot(geisha)

D <- dist(geisha, method = 'euclidian')
dendogram <- hclust(D, method = 'single')
dendogram
plot(dendogram)
rect.hclust(dendogram, 2)
dev.off()
clusters <- cutree(dendogram, 2)
clusters



mean.c1 <- colMeans(geisha[which(clusters == 1),])
mean.c2 <- colMeans(geisha[which(clusters != 1),])
plot(geisha, col = clusters+9)
points(mean.c1[1], mean.c1[2], cex=2, pch = '*', col='red')
points(mean.c2[1], mean.c2[2], pch = '*', cex =2, col='blue')

cophenetic(dendogram)

#b
dendogram2 <- hclust(D, method = 'complete') 
dendogram3 <- hclust(D, method = "average")
dendogram4 <- hclust(D, method = 'ward.D2')

x11()
par(mfrow = c(1,3))
plot(dendogram2)
rect.hclust(dendogram2,2)
plot(dendogram3)
rect.hclust(dendogram3,2)
plot(dendogram4)
rect.hclust(dendogram4,2)

clusters2 <- cutree(dendogram2, 2)
clusters3 <- cutree(dendogram3, 2)
clusters4 <- cutree(dendogram4, 2)

x11()
par(mfrow = c(1,3))
plot(geisha, col=clusters2, main='complete')
points(colMeans(geisha[which(clusters2 == 1),])[1],
       colMeans(geisha[which(clusters2 == 1),])[2], col='red', pch ='+', cex =4)
points(colMeans(geisha[which(clusters2 != 1),])[1],
       colMeans(geisha[which(clusters2 != 1),])[2], col='gold', pch='-', cex=4)
plot(geisha, col = clusters3 +2, main= 'average')
plot(geisha, col = clusters4 +4, main='ward')

#clusterizzano tutti alla stessa maniera quindi prendo uno di questi
# prendo il complete linkage

#c) Identify the successful tours with the smaller group found with the clustering method at point (b). Having
#introduced and verified the needed assumptions, provide 4 Bonferroni intervals (global level 90%) for the dif-
#  ference in the mean characteristics of successful and unsuccessful tours, and for the mean characteristics of a
#successful tour.
library(MVN)
succesful <- geisha[which(clusters2==1),]
mvn(succesful)
unsuccesful <- geisha[which(clusters2 !=1),]
mvn(unsuccesful)
m.s <- colMeans(succesful)
m.u <- colMeans(unsuccesful)
n.s <- dim(succesful)[1]
n.u <- dim(unsuccesful)[1]
cov(succesful)
cov(unsuccesful)
S <- cov(geisha)
Spooled <- (cov(succesful)*(dim(succesful)[1]-1) + 
            cov(unsuccesful)*dim(unsuccesful)[1]-1)/
            (dim(succesful)[1] + dim(unsuccesful)[1] -2)

#bonferroni
ci1 <- c(
    inf =(m.s[1] -m.u[1]) - qt(1-0.1/(2*2),n.s+n.u-2) * sqrt(Spooled[1,1]*(1/n.s + 1/n.u)),
    sup =(m.s[1] -m.u[1]) + qt(1-0.1/(2*2),n.s+n.u-2) * sqrt(Spooled[1,1]*(1/n.s + 1/n.u))
)
ci2 <- c(
  (m.s[2] -m.u[2]) - qt(1-0.1/4,n.s+n.u-2) * sqrt(Spooled[2,2]*(1/n.s + 1/n.u)),
  (m.s[2] -m.u[2]) + qt(1-0.1/4,n.s+n.u-2) * sqrt(Spooled[2,2]*(1/n.s + 1/n.u))
)
ci3 <- c(
  (m.s[1]) - qt(1-0.1/4,n.s-1) * sqrt(cov(succesful)[1,1]*1/n.s),
  (m.s[1]) + qt(1-0.1/4,n.s-1) * sqrt(cov(succesful)[1,1]*1/n.s)
)
ci4 <- c(
  (m.s[2]) - qt(1-0.1/4,n.s-1) * sqrt(cov(succesful)[2,2]*1/n.s),
  (m.s[2]) + qt(1-0.1/4,n.s-1) * sqrt(cov(succesful)[2,2]*1/n.s)
)


#The file garden.txt collects the number of carps, maple trees, cherry trees and stones, and the extension [m2 ] of
#156 of garden in the Kantō region of Japan. Experts believe that, to achieve an overall balance of elements, the
#Japanese gardens follow the model
#E = β0 + β1 · x1 + β2 · x2 + β3 · x3 + β4 · x4 + ε,
#with E the extension of the garden, x1 , x2 , x3 , x4 the number of carps, maple trees, cherry trees and stones
#respectively, and ε ∼ N (0, σ 2 ).
#a) Estimate the 6 parameters of the model and verify the model assumptions. Evaluate the residuals of the model.
#b) Perform two statistical tests to verify if
#- there is statistical evidence of a dependence of the mean garden extension on the number of maple or cherry
#trees;
#- there is statistical evidence of a dependence of the mean garden extension on lake elements (stones, carps).
#c) Based on the results at point (b), comment on possible model weaknesses and, if needed, reduce the dimensio-
#  nality of the regressors. Comments the analysis and interpret the results.
#d) Update the estimates of the parameters using the analysis at point (c)

garden <- read.table('garden.txt')
head(garden)
attach(garden)
lm1 <- lm(extension ~ carps + maple + cherry + stones)
summary(lm1)
#assumptions
par(mfrow = c(2,2))
plot(lm1)
shapiro.test(lm1$residuals)
#dal plot e dal test pare che le assumptions siano soddisfatte

#parameters
betas <- lm1$coefficients
sigma <- sum(lm1$residuals^2)/lm1$df.residual

library(car)
#b
linearHypothesis(lm1, c(0,0,1,0,0), 0)
#probabilità di osservare questi valori sapendo che beta =0 : 6%

linearHypothesis(lm1, c(0,0,0,1,0), 0)
#ininfluente

linearHypothesis(lm1, rbind(c(0,1,0,0,0), c(0,0,0,0,1)), c(0,0))
#influente


#c
lm2 <- lm(extension ~ carps + maple + stones)
summary(lm2)
#osservo un p-value alto sul beta relativo a carps ->linearhypothesis
linearHypothesis(lm2, c(0,1,0,0), 0)
# le carpe da sole non risultano influenti
lm3 <- lm(extension ~ maple + stones)
summary(lm3)
# abbiamo ora solo features particolarmente influenti, l'intercetta potrebbe essere 0
linearHypothesis(lm3, c(1,0,0),0)
lm3.3 <- lm(((extension-mean(extension))/sd(extension))~ I((maple- mean(maple))/sd(maple)) + I((stones-mean(stones))/sd(stones)))
summary(lm3.3)
par(mfrow=c(2,2))
plot(lm3.3)
plot(lm3)
#d
betas3 <- lm3$coefficients
sigma3 <- sum(lm3$residuals^2)/lm3$df.residual
betas3
sigma3
par(mfrow=c(2,2))
plot(lm3)
shapiro.test(lm3$residuals)
