# Instalaci?n de Paquetes
install.packages(c("VIM","DEoptimR","minqa","nloptr",
                   "DMwR","mvoutlier","TTR","caTools",
                   "arule"),dependencies = c("Depends"))


#########################################################
#  Transformaci?n                                       #
#########################################################

# Ejemplo: BUPA

setwd('/home/alvin/Desktop/ML-ESAN')
load("bupa.rda")
bupa$V7 <- as.factor(bupa$V7)
str(bupa)
summary(bupa)

########
#Z-score
########

library(reshape)
zbupa<-rescaler(x=bupa[,-7],type="sd")
zbupa<-cbind(zbupa,bupa[,7] )
summary(bupa)
summary(zbupa)

par(mfrow = c(1,2))
boxplot(bupa[,-7],col="blue")
boxplot(zbupa[,-7],col="blue")
par(mfrow = c(1,1))

#Usando la funci?n scale
zbupa <- cbind(scale(bupa[,-7]),bupa[,7])
zbupa <- as.data.frame(zbupa)
summary(bupa)
summary(zbupa)

par(mfrow = c(1,2))
boxplot(bupa[,-7],col="blue")
boxplot(zbupa[,-7],col="blue")
par(mfrow = c(1,1))

########
#Min-Max
########

# Usando la librer?a dprep
#source(file="scripts/dprep.R")
#mmbupa<-mmnorm(bupa,minval=0,maxval=1 )[,-7]
#summary(mmbupa)

library(DMwR)
mmbupa <- bupa
mmbupa[,dim(mmbupa)[2]+1] <- ReScaling(bupa[,1],0,1)
mmbupa[,dim(mmbupa)[2]+2] <- ReScaling(bupa[,2],0,1)
mmbupa[,dim(mmbupa)[2]+3] <- ReScaling(bupa[,3],0,1)
mmbupa[,dim(mmbupa)[2]+4] <- ReScaling(bupa[,4],0,1)
mmbupa[,dim(mmbupa)[2]+5] <- ReScaling(bupa[,5],0,1)
mmbupa[,dim(mmbupa)[2]+6] <- ReScaling(bupa[,6],0,1)

mmbupa <- bupa
for(h in 1:6){
 mmbupa[,h]=ReScaling(bupa[,h],0,1)
}
summary(bupa)
summary(mmbupa)

mmbupa[,-7] <- sapply(bupa[,-7],FUN=ReScaling, t.mn=0, t.mx=1)
summary(bupa)
summary(mmbupa)

boxplot(mmbupa[,-7],col="blue")

#####################
#Escalamiento decimal
#####################

# Usando dprep########################
library(lpSolve)
library(dprep)

decscale <-
  function (data) 
  {
    d = dim(data)
    c = class(data)
    cnames = colnames(data)
    classes = data[, d[2]]
    data = data[, -d[2]]
    maxvect = apply(abs(data), 2, max)
    kvector = ceiling(log10(maxvect))
    scalefactor = 10^kvector
    decdata = scale(data, center = FALSE, scale = scalefactor)
    attributes(decdata) = NULL
    decdata = matrix(decdata, dim(data)[1], dim(data)[2])
    decdata = cbind(decdata, classes)
    if (c == "data.frame") 
      decdata = as.data.frame(decdata)
    colnames(decdata) = cnames
    return(decdata)
  }

dsbupa<-decscale(bupa)[,-7]
dsbupa<-cbind(dsbupa,bupa[,7])
summary(bupa)
summary(dsbupa)

boxplot(dsbupa[,-7],col="blue")

#Sigmoidal################################3333
library(dprep)
sigbbupa<-bupa
sigbupa[,-7]<-signorm(bupa[,-7])
summary(bupa)
summary(sigbupa)

boxplot(sibupa[,-7],col="blue")

#Haciendo plots para ver el efecto de la normalizacion
par(mfrow=c(1,2))
plot(sort(bupa$V1))
plot(sort(sigbupa$V1))

#Softmax
library(DMwR)
softbupa<-bupa
softbupa[,-7]<-SoftMax(bupa[,-7],lambda=2*pi)
summary(softbupa)

boxplot(softbupa,col="blue")

#Gr?fico de comparaci?n
par(mfrow=c(2,3))
boxplot(bupa[,1:6],main="bupa")
boxplot(zbupa[,1:6],main="znorm bupa")
boxplot(mmbupa[,1:6],main="min-max bupa")
boxplot(dsbupa[,1:6],main="dec scale bupa")
boxplot(sigbupa[,1:6],main="signorm bupa")
boxplot(softbupa[,1:6],main="softmax bupa")
par(mfrow=c(1,1))


