# Instalaci?n de Paquetes
install.packages(c("VIM","DEoptimR","minqa","nloptr",
                   "DMwR","mvoutlier","TTR","caTools",
                   "arule"),dependencies = c("Depends"))


#########################################################
#  Outliers                                             #
#########################################################

#-------------------------------------------------------#
#  Outliers univariados                                 #
#-------------------------------------------------------#

# Ejemplo: BUPA

setwd("/home/alvin/Desktop/ML-ESAN")
load("bupa.rda")
str(bupa)
summary(bupa)

bupa$V7 <- as.factor(bupa$V7)

boxplot(bupa[,-7],col="blue")

zbupa <- data.frame(scale(bupa[,-7]),V7=bupa[,7])
boxplot(zbupa[,-7],col="blue")

par(mfrow=c(1,2))
boxplot(bupa[,-7],col="blue")
boxplot(zbupa[,-7],col="blue")
par(mfrow=c(1,1))

boxplot(bupa$V1,col="blue")
outliers <- boxplot(bupa$V1,plot=F)$out
nout=as.character(outliers)

for(i in 1:length(outliers))
{
  text(outliers[i],as.character(which(bupa$V1==outliers[i])),
       cex=.8,pos=4)
}
bupa[69,1]
bupa[224,1]

#-------------------------------------------------------#
#  Outliers multivariados                                 #
#-------------------------------------------------------#

# Ejemplo 1: Outliers en dos dimensiones

#load(file = "simpleExample.rda")
load(file.choose())
head(dat)
summary(dat)

par(mfrow=c(1,3))
plot(dat, xlim = c(-5,5), ylim = c(-5,5))
barplot(dat[,1], main="Valores de X")
barplot(dat[,2], main="Valores de Y")
par(mfrow=c(1,1))
#Para ver los valores van cambiando los valores de las variables
#Normalmente para variables cualitativas
#En el an?lisis univariado se ve que si hay outliers

par(mfrow=c(1,3))
plot(dat, xlim = c(-5,5), ylim = c(-5,5))
boxplot(dat[,1],main="Valores de X")
boxplot(dat[,2],main="Valores de y")
par(mfrow=c(1,1))
#El an?lisis univariado no lo detecta
#No siempre las imagenes van a ser claras
#Se necesita un indicador para evaluar lo que se tiene de manera visual
#Se puede usar entonces distancia mahalanobis para outliers multivariados






# para el parcial
dat <- bupa[,-7]
# Distancia de Mahalanobis
cm <- colMeans(dat)
S <- cov(dat)
dm <- sqrt(apply(dat, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))

# Distancia de Mahalanobis cuadrada
d <- dm^2
d <- mahalanobis(dat, cm, S)
barplot(d, main="Mahalanobis")
which.max(d)
#para saber que observaci?n es el outlier
dat[310:335,]

#plot(dat, xlim = c(-5,5), ylim = c(-5,5))
#identify(dat[,1], dat[,2],round(d,1))

#  QQ-plot:
qqplot(qchisq(ppoints(301), df = 2), d,
       main = expression("Q-Q plot para" ~~ {chi^2}[nu == 2]))
par(mfrow = c(1,1))
plot(dat, xlim=c(-5,5), ylim=c(-5,5))
points(dat[301,1],dat[301,2],col="red")

library(rgl)
plot3d(dat, col = c(rep(1,300), 2))


