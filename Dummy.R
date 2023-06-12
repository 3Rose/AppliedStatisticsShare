#_______________________________________________________________________________
##### DUMMIZATION, ORDERED DATA
rm(list = ls())
setwd('C:/Users/Michele/Desktop/Applied Statistics Labs/Applied Stats Code/Linear Models')
pb4  <- read.table('Pb4.txt')

# The file Pb4.txt reports the number Y (expressed in thousands of units)
# of vehicles registered annually in three countries of the European Union
# (France, Germany and Italy) during a reference period of 10 years.
# Recent economic models describe the behavior of this variable according
# to the model:
# Y | (X = x, G = g) = beta0.g + beta1.g * x^2 + eps
# with eps ~ N (0, sigma^2), x = 1, 2, ... , 10 (year) and
# g = France, Germany, Italy (EU country).

Year <- rep(1:10,3)
Year #regressor

Reg <- c(pb4[,1], pb4[,2], pb4[,3])
Reg #target

#DUMMY
dFr <- rep(c(1,0), c(10,20))      # dFr = 1 if France,  0 otherwise
dFr #10 volte 1 e 20 volte 0
dGer<- rep(c(0,1,0), c(10,10,10)) # dGer= 1 if Germany, 0 otherwise
dGer #10 volte 0, 10 volte 1, 10 volte 0

#FINAL DATASET
df <- data.frame(Reg   = Reg,
                   Year2 = Year^2,
                   dFr   = rep(c(1,0), c(10,20)),      # dummy for France
                   dGer  = rep(c(0,1,0), c(10,10,10))) # dummy for Germany

fit <- lm(Reg ~ dFr + dGer + Year2 + Year2:dFr + Year2:dGer, data=df)
summary(fit)

#_______________________________________________________________________________
##### DUMMIZATION, ORDERED DATA: INFERENCE
# 1. the variable x^2;
linearHypothesis(fit,
                 rbind(c(0,0,0,1,0,0),
                       c(0,0,0,0,1,0),
                       c(0,0,0,0,0,1)),
                 c(0,0,0)) #year2 significant

# 2. the dummies;
linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0),
                       c(0,0,1,0,0,0),
                       c(0,0,0,0,1,0),
                       c(0,0,0,0,0,1)),
                 c(0,0,0,0)) #dummy&theirInteraction significant

# 3. the interactions
linearHypothesis(fit,
                 rbind(c(0,0,0,0,1,0),
                       c(0,0,0,0,0,1)),
                 c(0,0)) #significant




#_______________________________________________________________________________
##### DUMMIZATION, NOT ORDERED DATA
data <- read.table('work.txt', header=T)
n <- dim(data)[1]
head(data)

attach(data)
Y <- Average_Score
X <- Years_Service
C1 <- Sex
C2 <- Race
detach(data)

result <- lm(Y ~ X)
summary(result)

#DUMMY
females <- which(C1=='Female')
males <- which(C1=='Male')
notwhite <- which(C2=='Nonwhite')
white <- which(C2=='White')

C1 <- rep(0,n)
C1[males] <- 1
C1
C2 <- rep(0,n)
C2[white] <- 1
C2
# # females white
# FW <- which(C1==0 & C2==1)
# # females not white
# FNW <- which(C1==0 & C2==0)
# # males white
# MW <- which(C1==1 & C2==1)
# # males not white
# MNW <- which(C1==1 & C2==0)

result <- lm(Y ~ X + C1 + C2 + X:C1 + X:C2)
summary(result)

#cut X:C1
result <- lm(Y ~ X + C1 + C2 + X:C2)
summary(result)

#cut X:C2 too
result <- lm(Y ~ X + C1 + C2)
summary(result)
