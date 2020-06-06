###################################
#M.Sc. Richard F. Fern?ndez V?squez
###################################

###################################
#CASO: DATOS ECON?MICOS EUROPA
###################################

data <- read.csv("http://www.instantr.com/wp-content/uploads/2013/01/europe.csv", header=TRUE)
str(data)
attach(data)

#####################
#Pruebas Preliminares
#####################

#1. Prueba bivariada
plot(Area,GDP)
# Test de correlación de Pearson
cor.test(Area,GDP,
         alternative = c("two.sided"),
         method = c("pearson"),conf.level = 0.95)

# Test de correlación Kendall
cor.test(Area,GDP,
         alternative = c("two.sided"),
         method = c("kendall"),conf.level = 0.95)

# Test de correlación de Spearman
cor.test(Area,GDP,
         alternative = c("two.sided"),
         method = c("spearman"),conf.level = 0.95)

#2. Matriz de Correlaciones

correlations <- cor(data[,2:8])
correlations

plot(data[,2:8])

#install.packages("corrplot")
library(corrplot)
# correlations<-cor(data[,2:8]) ya se realizó anteriormente
corrplot(correlations)

# install.packages("car")
library(car) # Nos va a servir para realizar scatter plot de las conbianciones de todas las variables
# Muestra una tendencia y bandas de confianza
scatterplotMatrix(data[,2:8], diagonal = "hist")

#Prueba de Esferecidad de Bartlett
t.bartlett <- function( data,alpha ){
  n <- dim(data)[1] #Filas
  p <- dim(data)[2] #Columnas
  R <- cor(data) #Correlación
  chi.square <- -(n-1-(2*p+5)/6) * log(det(R)) # fórmula de la prueba chicuadrado
  cat("\n","Prueba de Esferecidad de Bartlett","\n","\n")
  cat('chi.square = ', round(chi.square,3) , 
      ', df = ', (p^2-p)/2, 
      ', p-value = ', 1-pchisq(chi.square,(p^2-p)/2))
} 
t.bartlett(data[,2:8],0.05)


########################
#Componentes Principales
########################

pca1 <- prcomp(data[,2:8], scale=TRUE)

#$rotation: son los autovectores
#$sdev: es la ra?z cuadrada autovalores
#$x: nuevas coordenadas

#media de las variables iniciales
pca1$center

#desviaci?n est?ndar de las variables iniciales
pca1$scale

#Desviaci?n est?ndar de cada componente principal
std_dev <- pca1$sdev
std_dev

#Calculo de la varianza (autovalores)
pr_var <- std_dev^2
pr_var
sum(pr_var)

#Proporci?n de varianza explicada
prop_varex <- pr_var/sum(pr_var)
prop_varex

#Proporci?n de varianza explicada
cum_varex<-cumsum(pr_var)/sum(pr_var)
cum_varex

#Carga de componentes
pca1$rotation

#Vectores de puntuaci?n de componentes principales
dim(pca1$x)

#1. Elecci?n del n?mero de componentes este da toda la información de una
summary(pca1) # elegimos de frente con esta técnica (cuando la desviación estandar en mayor a uno)

screeplot(pca1)
abline(a = 1, b = 0, col = "red")

screeplot(pca1,type = "l")
abline(a = 1, b = 0, col = "red")

dim(pca1$x)
head(pca1$x, 3) # Matriz de datos (solo primeras 3 filas) con las componentes (en columnas las nuevas variables).

#2. Etiqueta de componentes
pca1$rotation[,1:3]

#3. Correlaci?n entre las componentes principales y variables iniciales
pca1$rotation #carga de las componentes

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}
evplot(pr_var)

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("Proporciones de varianza:")
  print(x.pvar)
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Componente Principal", ylab="Proporci?n de la varianza explicada", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Componente Principal", ylab="Proporci?n acumulada de la varianza explicada", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}
pcaCharts(pca1)

biplot(pca1,scale=0, cex=.7)

pca2 <- pca1
pca2$rotation <- -pca2$rotation
pca2$x <- -pca2$x
biplot(pca2,scale=0, cex=.7)

# PCA Variable Factor Map 
#install.packages("FactoMineR")
library(FactoMineR)
par(mfrow=c(1,2))
pca3 <- PCA(data[,2:8])
pca3
par(mfrow=c(1,1))
pca3$eig

head(predict(pca1,newdata=data[,2:8]))
head(predict(pca1,newdata=data[,2:8])[,1:3])

data1<-data.frame(data[,1],predict(pca1,newdata=data[,2:8])[,1:3])
names(data1)<-c("Country","Componente1","Componente2","Componente3")
head(data1)
str(data1)

#4- Correlaci?n entre componentes
#library(corrplot)
correlations <- cor(data1[,2:4])
corrplot(correlations)

