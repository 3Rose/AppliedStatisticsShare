library(fda)
df = read.table('temperature.txt',head=TRUE)
head(df)

df_num <- df[,1:length(df[1,])-1]

#SMOOTHING
ascissa=1:365
mybasis <- create.fourier.basis(rangeval = c(ascissa[1],ascissa[365]), nbasis = 21)
data.fd <- Data2fd(y = t(df_num),argvals = ascissa, basisobj = mybasis)

coef <- data.fd$coefs
coef[1:3,1:2]

#FPCA
pca <- pca.fd(data.fd,nharm=5,centerfns=TRUE)

pca$varprop #variance explained by every FPCs
sum(pca$varprop) #variance explained by the first 5 FPCs

par(mfrow=c(1,3))
plot(pca$harmonics[1,],col=1,ylab='FPC1') #seasonality: raise in summer, fall in winter
plot(pca$harmonics[2,],col=2,ylab='FPC1') #seasonality: anomalies
plot(pca$harmonics[3,],col=3,ylab='FPC1') #spring

par(mfrow=c(1,1)) #screeplot
plot(cumsum(pca$varprop)/sum(pca$varprop))

#NEW DATASET
scores <- pca$scores[,1:3]
plot(scores[,1],scores[,2],col=as.factor(df[,366]),xlab='PC1',ylab='PC2')

#DIM. REDUCTION
#first 3 PCs -> higher interpretability with plot
plot3d(scores[,1],scores[,2],scores[,3],xlab='PC1',ylab='PC2',zlab='PC3')
