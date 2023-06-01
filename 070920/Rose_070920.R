 

#Pb 1
#The file weather.txt reports weather data collected on the first months of the year 2020 in Milan. Each statistical
#unit corresponds to a day. For each unit, the following measurements are reported: mean temperature (◦ C), min
#temperature (◦ C), max temperature (◦ C), dew point (◦ C), humidity (%), visibility (km), mean wind (km/h) and
#max wind (km/h).
#a) Perform a Principal Component Analysis of the dataset, evaluate whether it is appropriate to use the original
#variables or the standardized ones and proceed accordingly. Report a plot of the loadings of the first 3 principal
#components and interpret them.
#b) Report the scatter plot of the data along the first two principal components and describe how to interpret the
#four quadrants of the plane. Use the information about the day of the year to further interpret the results.
#c) Report the screeplot. Propose (with motivations) a dimensionality reduction of the dataset. Report the variance
#explained along the principal components retained.
#d) The measurements for the 1st of August are the following: mean temperature 30◦ C, min temperature 23◦ C, max
#temperature 36◦ C, dew point 22◦ C, humidity 65%, visibility 19km, mean wind 5km/h and max wind 15km/h.
#Project the new datum on the reduced space identified at point (c).


#a) Perform a Principal Component Analysis of the dataset, evaluate whether it is appropriate to use the original
#variables or the standardized ones and proceed accordingly. Report a plot of the loadings of the first 3 principal
#components and interpret them.
weather <- read.table('weather.txt')
head(weather)
S <- cov(weather)
boxplot(weather, las=2)
S
S[7,7]
S[5,5]
#alta differenza tra le varianze della variabile 5  e variabile 7

#bene standardizzare
std_weather <- scale(weather)
head(std_weather)
pc.weather <- princomp(std_weather)
summary(pc.weather)
plot(pc.weather)
#plot all the loadings
x11()
par(mfrow = c(4,2))
for(i in 1:8)
barplot(pc.weather$loadings[,i], main = paste('component ',i, sep = ''))

#plot just first 3 components
x11()
par(mfrow = c(1,3))
for(i in 1:3)
barplot(pc.weather$loadings[,i], ylim= c(-1,1), las=2, main = paste('component ',i, sep = ''))

#b) Report the scatter plot of the data along the first two principal components and describe how to interpret the
#four quadrants of the plane. Use the information about the day of the year to further interpret the results.
plot(pc.weather$scores[,1], pc.weather$scores[,2], col=rep(c('blue','red'), each= 213/2))
abline(v = 0)
abline(h = 0)
#lato destro del grafico associato a temperature alte
#lato sinistro giorni con temperature più basse
#sopra giorni che registrano venti più bassi
# giorni che registrano venti più alti
#notiamo infatti che nei primi giorni dell'anno abbiamo temperature basse
#la seconda metà dei giorni è invece in media a destra (più caldi)



#c) Report the screeplot. Propose (with motivations) a dimensionality reduction of the dataset. Report the variance
#explained along the principal components retained.
plot(cumsum(pc.weather$sdev^2)/sum(pc.weather$sdev^2), type = 'b', ylim=c(0,1))
abline(h = 0.9)
summary(pc.weather)
# se ci accontentassimo di 2 componenti avremmo il 76% della variabilità totale
#spiegata da queste
#aggiungendo la terza componente abbiamo un grande incremento dal punto di vista di 
#variabilità spiegata (cosa che non succede se aggiungo anche la quarta)
#fermanoci alla terza spieghiamo il 94% della variabilità totale (ottimo)
head(pc.weather$scores)

barplot(pc.weather$sdev[1:3]^2, names.arg = pc.weather$sdev[1:3]^2)
pc.weather$sdev[1:3]^2


#d) The measurements for the 1st of August are the following: mean temperature 30◦ C, min temperature 23◦ C, max
#temperature 36◦ C, dew point 22◦ C, humidity 65%, visibility 19km, mean wind 5km/h and max wind 15km/h.
#Project the new datum on the reduced space identified at point (c).
eigensV <- pc.weather$loadings[,1:3]
x.new <- c(30, 23, 36, 22, 65, 19, 5, 15)
pc1.new <- rbind(eigensV[,1])%*%x.new
pc2.new <- rbind(eigensV[,2])%*%x.new
pc3.new <- rbind(eigensV[,3])%*%x.new





#Pb 2
#To compare the performances of the led bulbs produced by two brands (Candle Inc. and Sunshine Corp.), brightness
#tests are performed on 50 led bulbs independently sampled from those produced by Candle Inc. and 50 led bulbs
#independently sampled from those produced by Sunshine Corp.
#For each led bulb, the brightness is measured by two light meters positioned at different distances from the led
#bulb: the first light meter is a few centimeters from the led bulb and the second is positioned at one meter from
#the led bulb. The measurements on different led bulbs are independent (while the measurements on the same led
#                                                                       bulb are not).
#Files candle.txt and sunshine.txt report the measurements obtained with the two light meters on each led
#bulb produced by Candle Inc. and Sunshine Corp. respectively.
#a) Perform a statistical test of level 95% to verify if the mean measurements on the led bulbs produced by the two
#brands differ. State the model assumptions required to perform the test and verify them.
#b) Compute and report the p-value of the test at point (a).
#c) Interpret the results of the test at point (a) through two Bonferroni intervals of global level 95% for appropriate
#differences in the mean. Comment the result.
#d) Is there statistical evidence to state that, at level 95%, the decrease in brightness between the two measurements
#of the led bulbs produced by Candle Inc. is in mean higher than the one of the led bulbs produced by Sunshine
#Corp.?

#a) Perform a statistical test of level 95% to verify if the mean measurements on the led bulbs produced by the two
#brands differ. State the model assumptions required to perform the test and verify them.
candle <- read.table(('candle.txt'))
head(candle)
sunshine <- read.table(('sunshine.txt'))
head(sunshine)
m.c <- colMeans(candle)
m.s <- colMeans(sunshine)
n.c <- dim(candle)[1]
n.s <- dim(candle)[1]
Spooled <- (cov(candle)*(n.c-1) +cov(sunshine)*(n.s-1))/(n.c+n.s-2)
      cov(rbind(candle,sunshine))
#Problem: should I use the pooled one or the one from the entire dataset?
      
p <- 2
m.diff <- m.c -m.s 
cfr <- ((n.c+n.s-2)*p/(n.c+n.s-1-p)) * qf(1-0.05, p, n.c+n.s-1-p)
#Problem: should I use the n.c+n.s-2 or just n.tot -1 ?

T2 <- (1/n.s + 1/n.c)^(-1) * rbind(m.diff) %*% solve(Spooled) %*% cbind(m.diff)
T2 > cfr
#reject at level 5 of significance the hypothesis that 2 means do not differ
#there is hence statistical evidence that 2 means differ

#verify the assumptions
load("/home/rose/Desktop/Applied Statistics/Lab 5/mcshapiro.test.RData")
mcshapiro.test(candle)$pvalue
mcshapiro.test(sunshine)$pvalue
#i do not reject the hypothesis of normality at level 5% or 10%
bartlett.test(cbind(rbind(candle,sunshine), dummy= rep(c(0,1), each = 50))[,-1], dummy)
#secondo me è sbagliato

bartlett.test(list(candle[,-1], sunshine[,-1]))
#questo secondo me è giusto e ha senso vedendo le 2 matrici covarianza
cov(candle)
cov(sunshine)

#b) Compute and report the p-value of the test at point (a).
pval <- pf(T2/((n.c+n.s-2)*p/(n.c+n.s-1-p)),p, n.s+n.c-1-p)
pval
#Problem: wrong pvalue

#c) Interpret the results of the test at point (a) through two Bonferroni intervals of global level 95% for appropriate
#differences in the mean. Comment the result.
k <- 2
bci1 <- c(
      m.diff[1] - qt(1-0.05/(2*k),n.s+n.c-1) * sqrt(Spooled[1,1]/(n.s+n.c)),
      m.diff[1] + qt(1-0.05/(2*k),n.s+n.c-1) * sqrt(Spooled[1,1]/(n.s+n.c))
)
bci2 <- c(
  m.diff[2] - qt(1-0.05/(2*k),n.s+n.c-1) * sqrt(Spooled[2,2]/(n.s+n.c)),
  m.diff[2] + qt(1-0.05/(2*k),n.s+n.c-1) * sqrt(Spooled[2,2]/(n.s+n.c))
)
#Question: should I use -1 or -2 in qt(1-0.05/(2*k),n.s+n.c-1)

bci1
bci2
m.diff



#d) Is there statistical evidence to state that, at level 95%, the decrease in brightness between the two measurements
#of the led bulbs produced by Candle Inc. is in mean higher than the one of the led bulbs produced by Sunshine
#Corp.?
decr.c <- candle[,1] - candle[,2]
decr.s <- sunshine[,1] - sunshine[,2]
decr.diff <- decr.c - decr.s
shapiro.test(decr.diff) #normale in quanto c.l. di variabili normali
m.decr.c <- m.c[1] - m.c[2]
m.decr.s <- m.s[1] - m.s[2]

m.decr.diff <- (m.decr.c - m.decr.s) 

t2 <- (m.decr.diff)/sqrt(var(decr.diff)/(n.s+ n.c))
cfr.t <- qt(1-0.05, n.s+n.c-1)
t2 >cfr.t
# reject at level 5% the hypothesis of the mean decreases at the same rate
# so consider that the 2 types of bulbs don't decrease at the same rate and the mean
# decrease of the candle is bigger than the mean of the sunshine we can say that
# there is statistical evidence that candle decrease
#is in mean higher than the one of the led bulbs produced by Sunshine



#Benedetta is learning to bake bread. To obtain the perfect dough rising, she is performing some experiments to
#evaluate the effect of the kind of yeast used and the effect of the waiting time (measured in hours) on the rising
#of the dough. In the different experiments, she used either the brewer’s yeast (by) or the sourdough (sd) and
#measured the rising of the dough as a percentage with respect to the initial volume at different times. The file
#leaven.txt contains the data collected by Benedetta in 80 experiments conducted in the last months.
#Consider a linear model assuming, for each kind of yeast, a polynomial of degree at most 2 in waiting time
#(time). Impose the same intercept for the two models.
#a) Estimate the parameters of the model. Verify the model assumptions.
#b) Perform two statistical tests - each at level 1% - to verify if
#- there is a significant dependence of the rising of the dough on the kind of yeast;
#- there is statistical evidence that the degree of the polynomial for the brewer’s yeast is higher than the degree
#of the polynomial for the sourdough.
#c) Based on the tests performed in (b) or any other test deemed relevant, reduce the model and update the model
#parameters.
#d) Benedetta wants to bake bread for dinner, but she can wait only 2 hours: which yeast do you suggest to use?
#  Provide a confidence interval with confidence level 99% for the mean of the rising of the dough in such conditions.


#a) Estimate the parameters of the model. Verify the model assumptions.
leaven <- read.table('leaven.txt')
head(leaven)
#Consider a linear model assuming, for each kind of yeast, a polynomial of degree at most 2 in waiting time
#(time). Impose the same intercept for the two models.
leaven.by <- leaven[which(yeast == 'by'), 1:2]
leaven.sd <- leaven[which(yeast == 'sd'), 1:2]

lm.by <- lm(volume ~ time + I(time^2), leaven.by)
lm.sd <- lm(volume ~ time + I(time^2), leaven.sd)
summary(lm.by)  #intercept : 1.0022836
summary(lm.sd)  #intercept : 0.963002
#impose 1 as intercept
lm.by$coefficients[1] <- 1
lm.sd$coefficients[1] <- 1
summary(lm.by)
summary(lm.sd)

#estimators
coef.by <- lm.by$coefficients
coef.sd <- lm.sd$coefficients
sig.by <- sum(lm.by$residuals^2)/lm.by$df.residual
sig.sd <- sum(lm.sd$residuals^2)/lm.sd$df.residual

#verify assumptions
load("/home/rose/Desktop/exams/Exams windows/Labs/Lab 5/mcshapiro.test.RData")
library('MVN')
x11()
par(mfrow=c(2,2))
plot(lm.by)
plot(lm.sd)
shapiro.test(lm.by$residuals) #ok
shapiro.test(lm.sd$residuals) #ok


#b) Perform two statistical tests - each at level 1% - to verify if
#- there is a significant dependence of the rising of the dough on the kind of yeast;
#- there is statistical evidence that the degree of the polynomial for the brewer’s yeast is higher than the degree
#of the polynomial for the sourdough.

mean.by.volume <- mean(leaven.by$volume) 
mean.sd.volume <- mean(leaven.sd$volume)
n.sd <- dim(leaven.sd)[1]
n.by <- dim(leaven.by)[1]
alpha <- 0.01
#Problem: lot of doubts on (n.sd+n.by)/2
stnd.d.pooled <- (sd(leaven.by$volume)*(n.by-1) - sd(leaven.sd$volume)*(n.sd -1))/(n.sd + n.by -2)
tstat <- (mean.by.volume - mean.sd.volume) / (stnd.d.pooled / sqrt((n.sd+n.by)/2))
cfr.t <- qt(1 - alpha/2, (n.sd+n.by)/2 - 1)
tstat > cfr.t
pvalue  <- ifelse(tstat >= 0, (1 - pt(tstat, (n.sd+n.by)/2 -1))*2, pt(tstat, (n.sd+n.by)/2 -1)*2)

#alternativa
library(car)
lm1 <- lm(volume ~ time + I(time^2) + yeast + yeast:time +yeast:I(time^2), leaven )
linearHypothesis(lm1, rbind(c(0,0,0,1,0,0),
                            c(0,0,0,0,1,0),
                            c(0,0,0,0,0,1)), c(0,0,0))

#- there is statistical evidence that the degree of the polynomial for the brewer’s yeast is higher than the degree
#of the polynomial for the sourdough.
summary(lm.by)
summary(lm.sd)

as.numeric(linearHypothesis(lm.by, c(0,0,1), 0)$P)#rigetto
as.numeric(linearHypothesis(lm.sd,c(0,0,1),0)$P) #non rigetto l'ipotesi che il
#secondo termine possa essere = 0 
#Answer is yes, there is statistical evidence that the degree of the polynomial for the brewer’s yeast is higher than the degree
#of the polynomial for the sourdough.


#c) Based on the tests performed in (b) or any other test deemed relevant, reduce the model and update the model
#parameters.

lm.by2 <- lm(volume ~ I(time^2), leaven.by)
lm.sd2 <- lm(volume ~ time, leaven.sd)
summary(lm.by2)
summary(lm.sd2)
lm.by2$coefficients
lm.sd2$coefficients
sum(lm.by2$residuals^2) / lm.by2$df.residual  
sum(lm.sd2$residuals^2) / lm.sd2$df.residual  


#d) Benedetta wants to bake bread for dinner, but she can wait only 2 hours: which yeast do you suggest to use?
#  Provide a confidence interval with confidence level 99% for the mean of the rising of the dough in such conditions.
predict.lm(lm.by2, data.frame(time = 2))
v0 <- predict.lm(lm.sd2, data.frame(time = 2))
#if she want more size grow she may slightly prefer sd
Z <- model.matrix((lm.sd2))
ci <- c(
      v0 - sqrt(sig.sd * cbind(1, 2) %*% solve(t(Z) %*% Z) %*% rbind(1,2)) * qt(1-0.01/2,n.sd-2),
      v0,
      v0 + sqrt(sig.sd * cbind(1, 2) %*% solve(t(Z) %*% Z) %*% rbind(1,2)) * qt(1-0.01/2,n.sd-2)
)



#To forecast the tidal currents in the port of Dublin, the vertical motion induced by the rise and fall of the tides is
#monitored. The file tide.txt reports the measurements of the sea level in the port of Dublin collected every half
#hour for one day. Since the time variation of the sea level is inherently a continuous process, consider a functional
#data analysis approach. It is known that the available measurements are affected by small errors.
#a) Perform a smoothing of the data using a B-spline basis of degree 3. Choose the number of basis functions using
#a generalized cross-validation criterion. Report the number of basis functions chosen, a plot of the B-spline
#basis system used and a plot of the smoothed data.
#b) Compute approximate pointwise confidence intervals at the sampling times of the data and provide a plot.
#c) To study the currents induced by the tide, it is important to have a good estimate of the velocity of the tide.
#Compute an approximation of the first derivative of the curve from the data and the first derivative of the
#smoothed curve obtained at point (a). Provide a plot to compare the two and comment on the result.
#d) Would you suggest the use of a different basis system? Why?