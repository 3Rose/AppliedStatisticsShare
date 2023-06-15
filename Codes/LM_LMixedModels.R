df <- read.table('danceability.txt',header=TRUE)
head(df)

#the categorical variable genre should have random intercept -> Linear Mixed Model
library(lme4)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(insight)

l3 <- lmer(danceability ~ loudness + tempo + (1|genre), data = df)
summary(l3)

#available functions:
#get_variance_x
#x = disperion, distribution, fixed, random,residual, intercept, slope
#Percentage of Variance explained by the Random Effect (PVRE)
sigma2_eps <- as.numeric(get_variance_residual(l3))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(l3))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

#POINT F
dotplot(ranef(l3, condVar=T)) #random effect + conditional variances
