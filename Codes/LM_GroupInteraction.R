df = read.table('tattoo.txt',head=TRUE)
head(df)

attach(df)
#POINT A
n <- nrow(df)
dummy_hand = rep(0,n)
dummy_hand[which(method == 'handmade')] = 1

fit <- lm(price ~ dimension + ncolors + dummy_hand + dimension:dummy_hand + ncolors:dummy_hand, data = df)
summary(fit)

b = fit$coefficients
#coeff handmade
a_h <- b[1]+b[4] #intercept + dummy handmade
beta_dimension_h <- b[2]+b[5]
beta_color_h <- b[3]+b[6]
#coeff machine
a_m <- b[1]
beta_dimension_m <- b[2]
beta_color_m <- b[3]
#sigma
sigma <- sum(fit$residuals^2)/fit$df

