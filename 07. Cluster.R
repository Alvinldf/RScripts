#################################################
#An?lisis Cl?ster
#################################################

setwd("/home/alvin/Desktop/R/MineriaDatosESAN/data/")
data <- read.csv("inv_mercados.csv",header=T,sep=",")
head(data)
str(data)
summary(data)

#1.Cluster Jer?rquico
distancias <- dist(data[,2:7]) # excluyo la primera columna por que es el ID
distancias
jerarquico <- hclust(distancias) # prepara la data para aplicar cluster jerárquico
grupos <- cutree(jerarquico, k=3) # computa el cluster con el método jerárquico
head(data.frame(data,grupos)) # convierte a grupos (ya habiendo sido agrupado por clusters) a data frame y los une a data
# gráfica de los cluster jerárquicos
plot(jerarquico,main= "Cluster")
# señala los clusters en un cuadrado (k=número de clusters a señalar)
rect.hclust(jerarquico, k=3, border="red")



#2.Cluster KMeans
res <- kmeans(scale(data[,-1]),3) # 3 clusters
res
?scale
#scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
#Visualiza el cluster de pertenencia de las observaciones
res$cluster

#tama?o de cada cluster
res$size

head(data.frame(data,cluster=res$cluster))
data1 <- data.frame(data,cluster=res$cluster)
head(data1)

#Elecci?n del n?mero de cl?ster (para saber cuántos clusters debo elegir)
# Silueta (indicador sobre si está bien o no el número de clusters)
library(cluster)
diss.data=daisy(scale(data[,-1]))
par(mfrow=c(1,3))
for(h in 2:4){ #prueba 2 a 4 clusters
  res=kmeans(scale(data[,-1]),h)
  plot(silhouette(res$cluster,diss.data))
}
par(mfrow=c(1,1))

#Visualizaci?n de Cl?ster
res <- kmeans(scale(data[,-1]),3)

clusplot(data[,-1],res$cluster, color = TRUE,
         shade = TRUE, labels =2,lines=0,
         main ="Gr?fico de Conglomerados")

#etiquetar cada cluster
data.x <- subset(data1, select= -cluster)
data.c <- subset(data1, select= cluster)
aggregate(data.x, data.c, mean)

install.packages("doBy")
library(doBy)
summaryBy(V1+V2+V3+V4+V5+V6 ~ cluster, data=data1, FUN=mean)











install.packages("sqldf")
library(sqldf)
sqldf("select cluster, avg(V1), avg(V2), 
      avg(v3), avg(v4), avg(v5), avg(V6) from data1 
      group by cluster")
  
