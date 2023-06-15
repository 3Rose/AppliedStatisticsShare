#POINT E 19_01_22
df = read.table('tattoo.txt',head=TRUE)
head(df)

attach(df)

dummy_hand = rep(0,n)
dummy_hand[which(method == 'handmade')] = 1
fit3 <- lm(price ~ dimension + ncolors + dimension:dummy_hand, data = df) 

#estimation for the intrcept, no variables, only intercept = fixed cost
alpha = 0.05
query1 = data.frame(dimension = 0.000, ncolors = 0.000, dummy_hand = 1)
Conf1 <- predict(fit3, query1, interval='confidence', level=1-alpha/2)
query2 = data.frame(dimension = 0.000, ncolors = 0.000, dummy_hand = 0)
Conf2 <- predict(fit3, query2, interval='confidence', level=1-alpha/2)


Conf <- (Conf1 + Conf2)/2
Conf


query1 = data.frame(dimension = 6.5, ncolors = 1, dummy_hand = 1)
Conf1 <- predict(fit3, query1, interval='confidence', level=1-alpha/2)
Conf1

