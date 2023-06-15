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

#POINT A
l1 <- lm(tv~C1+age+heig+dist+sib+comp+ex+mu+ga)
summary(l1)
#need var. selection

plot(l1) #first plot contain analysis of residuals -> no relation detected

#second one contain info about assumption of normality, to be precise:
shapiro.test(l1$residuals) #not gaussian at 0.05% level
hist(l1$residuals) #confirmation achieved: not gaussian

l1$coefficients #coeff
sum(residuals(l1)^2)/l1$df #estimate for sigma^2
