#########################################################
# Introduccion a R                                      #
#########################################################

rnorm(10)

x<-rnorm(10)
x

summary(x)
w<-summary(x)
w

5+5
sqrt(25)
pi

attach()

help(rnorm)

#Vector: colecci?n ordenada de elementos del mismo tipo

x<-c(1,2,3)
x

y<-c("a","b","c")
y

#Array y matrix: generalizaci?n multidimensional del vector. 
#Elementos del mismo tipo 
#Todos los elementos de la matriz han de ser del 
#mismo tipo

t<-1:42
t
dim(t)=c(3,7,2)
t

t[1,2,]

#Data.frame: con colummnas de diferentes tipos
z<-data.frame(Genero=c("M","F","M"),Edad=c(25,35,21))
z
plot(z)

#Factor: tipo de vector para datos cualitativos
a<-factor(c(1,2,3,1,2,3,4))
a
plot(a)

#Secuencias
b<-1:30
b

c<-seq(1,50,by=5)
c

d<-seq(1,30,lenght=15)
d

#Operadores logicos: ==,!=,<,<= ... 

#ordenar
x<-c(2,6,3,5,8,22,31,10)
sort(x)
sort(x, decreasing=T)

#manipular los componentes de un vector
x<-c(12,16,13,15,18,22,31,10)
length(x)
x[4]
x[3:5]
x[-2]
x[x>16]

#graficos
par(mfrow=c(2,2)) 

plot(iris[,1], type="l"); title("lines") 
plot(iris[,1], type="b"); title("both") 
plot(iris[,1], type="o"); title("overstruck") 
plot(iris[,1], type="h"); title("high density")

plot(iris[1:30,1], pch="*")
plot(iris[1:30,1], pch="M")

par(mfrow = c(1,1))

#Datos perdidos

#R utiliza NaN para valores no denidos
#R utiliza Null para objetos nulos o no definidos
#R utiliza NA para datos perdidos (not available)
#Una funcion puede manejar los datos que faltan. 
#Por ejemplo la funcion
#mean descarta valores perdidos si se escribe na.rm = TRUE)

x <- c(4, 7, 2, 0, 1, NA)
mean(x)

mean(x, na.rm=TRUE)
which(x>4)
x==NA

#is.na(x):detecta datos NA
#is.nan(x):detecta datos NaN
#is.null(x): verifica si x es nulo.

is.na(x)
any(is.na(x))
(y <- x/0)
is.nan(y)
is.na(y)

#########################################################
# Ejemplo: Marketing Directo                          #
#########################################################

# Lectura de datos
# marketing.csv
DMark<-read.csv(file.choose())
head(DMark)
str(DMark)

#------------------------------------------------------------------#
#  a) Representaci?n de Datos Cualitativos                         #
#------------------------------------------------------------------#

# Tabla de Frecuencia
# -------------------
ni<-table(DMark$Edad)
fi<-prop.table(table(DMark$Edad))
pi<-prop.table(table(DMark$Edad))*100
edad.tabla<-t(rbind(ni,fi,pi))
edad.tabla

## Similar a SAS o SPSS
install.packages("gmodels")
install.packages("gdata")
library(gmodels)
library(gdata)
CrossTable(DMark$Edad, format="SAS")
CrossTable(DMark$Edad, format="SPSS")

# Gr?fico de Barras
# -------------------
barplot(pi, main="Distribuci?n de las edades de los clientes", 
        xlab="Grupo Etario",
        ylab="Porcentaje de Clientes")


par(mfrow=c(2,1))
barplot(table(DMark$Edad), main="Distribuci?n de la Edad de los Clientes", 
        col=1,xlab="Edad",ylab="# de Clientes")

barplot(prop.table(table(DMark$Edad))*100,
        main="Distribuci?n de la Edad de los Clientes", 
        col=3,xlab="Edad",ylab="% de Clientes")

par(mfrow=c(1,1))


# Gr?fico de Sectores Circulares
pie(pi, main="Distribuci?n de la Edad de los Clientes")

## Colocar porcentajes
lbls1 <- paste(names(table(DMark$Edad)), "\n",
               prop.table(table(DMark$Edad))*100,"%", sep="")

pie(pi, labels = lbls1,
    main="Distribuci?n de la Edad de los Clientes")

#------------------------------------------------------------------#
#  b) Tablas de contingencia                                       #
#------------------------------------------------------------------#
tabla1=table(DMark$Edad,DMark$Historial)
tabla1

#------------------------------------------------------------------#
#  c) Representaci?n de Datos Cuantitativos Discretos              #
#------------------------------------------------------------------#
# Tabla de Frecuencias
ni<-table(DMark$Hijos)
fi<-prop.table(table(DMark$Hijos))
pi<-prop.table(table(DMark$Hijos))*100
hijos.tabla<-t(rbind(ni,fi,pi))
hijos.tabla

#Gr?fico de Varas
plot(pi, type="h", lwd=2,
     xlab="N?mero de hijos",
     ylab="Porcentaje de clientes",
     main="Distribuci?n del n?mero de hijos de los clientes")

points(x =as.numeric(row.names(pi)),
       y =as.numeric(pi),
       pch=19,cex=1.5)

#------------------------------------------------------------------#
#  d) Representaci?n de Datos Cuantitativos Continuos              #
#------------------------------------------------------------------#
#########################################################################################################
factorx <- factor(cut(DMark$Monto, breaks=nclass.Sturges(DMark$Monto),right=TRUE))
xout <- as.data.frame(table(factorx))
colnames(xout)<-c("Monto","ni")
xout <- transform(xout, 
                  fi=prop.table(ni),
                  pi=prop.table(ni)*100,
                  Ni = cumsum(ni),
                  Fi = cumsum(prop.table(ni)),
                  Pi = cumsum(prop.table(ni))*100
)
xout

# Histograma (Comparativo)
par(mfrow=c(1,3))
hist(DMark$Monto[DMark$Edad=="Joven"],ylim=c(0,170))
hist(DMark$Monto[DMark$Edad=="Media"],ylim=c(0,170))
hist(DMark$Monto[DMark$Edad=="Adulta"],ylim=c(0,170))
par(mfrow=c(1,1))

#Histograma y Densidad
hist(DMark$Monto,prob=TRUE)
lines(density(DMark$Monto))

#Gr?fico de Densidad
plot(density(DMark$Monto))

#Boxplots
boxplot(DMark$Monto)
boxplot(DMark$Monto ~ DMark$Ecivil,
        xlab="Estado Civil",ylab="Gasto",
        main="Comparacion del gasto por estado civil")

#------------------------------------------------------------------#
#  e) An?lisis descriptivo                                         #
#------------------------------------------------------------------#
##########################################################################################
# Resumen b?sico
summary(DMark$Monto)

# Funci?n para calcular CV
CV <- function(x){
  (sd(x)/mean(x))*100
}

# Funci?n para calcular asimetria
A3 <- function(x){
  3*(mean(x)-median(x))/sd(x)
}

# Funci?n para calcular el rango
rango <- function(x){
  diff(range(x))
}

# Funci?n para calcular el rango intercuart?lico
RIC <- function(x){
  quantile(x,probs = 0.75,type = 6)-quantile(x,probs = 0.25,type = 6)
}

me<-mean(DMark$Monto)
med<-median(DMark$Monto)
q1<-quantile(x = DMark$Monto,probs = 0.25,type = 6)
q3<-quantile(x = DMark$Monto,probs = 0.75,type = 6)
r<-rango(DMark$Monto)
ric<-RIC(DMark$Monto)
s<-sd(DMark$Monto)
cv<-CV(DMark$Monto)
as3<-A3(DMark$Monto)

resumen<-as.matrix(rbind(me,med,q1,q3,r,ric,s,cv,as3))
colnames(resumen)<-c("Valor")
resumen

#------------------------------------------------------------------#
#   Correlaci?n                                                    #
#------------------------------------------------------------------#

plot(DMark$Salario,DMark$Monto)
cor(DMark$Salario,DMark$Monto)

# Matriz de Diagramas de dispersi?n
pairs(~Salario + Monto + Hijos,data=DMark)


