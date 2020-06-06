# Pregunta 11
install.packages('mice')
library(mice)
data(boys)
head(boys)
library(VIM)

a <- aggr(boys)
summary(a)


# Pregunta 12
install.packages('mlbench')
library(mlbench)
data(Glass)
head(Glass)

#a 
boxplot(Glass[,-10], col='blue')

zglass <- data.frame(scale(Glass[,-10]), type=Glass[,10])
boxplot(zglass[,-10], col = 'blue')

par(mfrow=c(1,2))
boxplot(Glass[,-10], col = 'blue')
boxplot(zglass[,-10], col = 'blue')
par(mfrow=c(1,1))


#b
cm <- colMeans(Glass[,-10])
S <- cov(Glass[,-10])
dm <- sqrt(apply(Glass[,-10], 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))

d <- dm^2
d <- mahalanobis(Glass[,-10], cm,S)
barplot(d, main = 'Mahalanobis')
which.max(d)
