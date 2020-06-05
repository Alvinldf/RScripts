# Instalaci?n de Paquetes
install.packages(c("VIM","DEoptimR","minqa","nloptr",
                   "DMwR","mvoutlier","TTR","caTools",
                   "arule"),dependencies = c("Depends"))

#########################################################
#  Discretizaci?n                                       #
#########################################################

setwd('/home/alvin/Desktop/ML-ESAN')
load("bupa.rda")

#1. Discretizacion con intervalos de igual amplitud

library(arules)
#Usando Sturges
nbins <- nclass.Sturges(bupa$V1) 
nbins
dbupa <- bupa
dbupa$V1_dis <- discretize(bupa[,1],method="interval", breaks = nbins)
table(dbupa$V1_dis)
plot(dbupa$V1_dis)

#Usando Friedman-Diaconis
nbins <- nclass.FD(bupa$V1) 
nbins
dbupa <- bupa
dbupa$V1_dis <- discretize(bupa[,1],method="interval",breaks = nbins)
table(dbupa$V1_dis)
plot(dbupa$V1_dis)

#Usando Scott
nbins <- nclass.scott(bupa$V1) 
nbins
dbupa <- bupa
dbupa$V1_dis <- discretize(bupa[,1],method="interval",breaks = nbins)
table(dbupa$V1_dis)
plot(dbupa$V1_dis)

dbupa <- bupa
for(h in 1:6){
  nbins <- nclass.scott(bupa[,h])
  dbupa[,h+7] <- discretize(bupa[,h],method="interval",breaks = nbins)
}
table(dbupa[,1])
table(dbupa[,2])
table(dbupa[,3])
table(dbupa[,4])
table(dbupa[,5])
table(dbupa[,6])

#2.Discretizaci?n con intervalos de igual frecuencia
?discretize
dbupa <- bupa

for(h in 1:6){
  dbupa[,h]<-discretize(bupa[,h],method="frequency",breaks = 10)
}
table(dbupa[,1])
table(dbupa[,2])
table(dbupa[,3])
table(dbupa[,4])
table(dbupa[,5])
table(dbupa[,6])
