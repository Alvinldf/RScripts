###################################
#Identificaci?n de valores perdidos
###################################

# Instalaci?n de Paquetes
install.packages(c("VIM","DEoptimR","minqa","nloptr",
                   "DMwR","mvoutlier","TTR","caTools",
                   "arule"),dependencies = c("Depends"))

#Ejemplo1
setwd("/home/alvin/Desktop/ML-ESAN/")
load("censusn.rda")
load(file.choose())

head(censusn)
str(censusn)
summary(censusn$V13)

dim(censusn)
dim(censusn)[1] #n?mero de filas
dim(censusn)[2] #n?mero de columnas

install.packages("Hmisc")
library(Hmisc)
describe(censusn)

install.packages("fBasics")
library(fBasics)
basicStats(censusn)

library(VIM)
#Columnas que tienen valores perdidos
which(colSums(is.na(censusn))!=0)

sum(censusn$v1)/nrow(censusn)
sum(censusn$V13,na.rm=TRUE)/nrow(censusn)

#Porcentaje de valores perdidos por columnas
colSums(is.na(censusn))
which(colSums(is.na(censusn))!=0)
colmiss <- c(2,6,13)
100*colSums(is.na(censusn[,colmiss]))/dim(censusn)[1]

#Crear un funci?n para identificar NA


countNA(censusn$V2)

identificaNA <- function(data)
{
  j <- dim(data)[1]
  k <- dim(data)[2]
  na=rep(0,k)
  for(i in 1:k)
    na[i] <- countNA(data[,i])
  na.val <- na
  print(data.frame(variables=names(data),na=na.val,na_por=100*na.val/j))
}
identificaNA(censusn)

library(VIM)
a <- aggr(censusn)
summary(a)
a
a$missings
a$combinations

matrixplot(censusn)

install.packages("mice")
library(mice)
md.pattern(censusn)

#data <- subset(is.na(censusn$V13)==TRUE,)
data<- censusn[-which(is.na(censusn$V2)), ]
data<- censusn[-which(is.na(censusn$V6)), ]
data<- censusn[-which(is.na(censusn$V13)), ]
data <- na.omit(censusn)

sum(complete.cases(censusn))
sum(!complete.cases(censusn))
