library("geosphere")        
library(vegan)
library(readxl)
library(BAT)
setwd("~/Rstudio/Mitu/Mantel")

################################################################  Distancia al centro urbano
Datos<-read.csv("Coor.csv")
C<-c(-70.2366257,1.2586965)
distHaversine(Datos,C)
Matriz<-distm(Datos,C,fun = distGeo)
Matriz
M<-write.csv(Matriz,file = "M.csv")
###############################################################  Test de mantel - incidencia
Incidencia<-read_excel("TR.xlsx")
Incidencia<-Incidencia[,c(-1,-2)]

Transectos<-read.csv("Coor.csv")
Transectos<-Transectos[,c(-1,-2)]
DistIn<-vegdist(Incidencia)
DistTR<-distm(Transectos,fun = distGeo)
Mantel<-mantel(DistIn,DistTR,method="spearman")
Mantel

###############################################################  Test de mantel - Presencia
Incidencia<-read_excel("TR.xlsx")
Incidencia<-Incidencia[,c(-1,-2,-37)]
Incidencia = replace(Incidencia, Incidencia >0 , 1)

Transectos<-read.csv("Coor.csv")
Transectos<-Transectos[,c(-1,-2)]

#DistIn<-vegdist(Incidencia, method = "jaccard")#Jaccard basado en bray-curtis
DistIn<-beta(Incidencia)#Jaccard de BAT(riqueza+recambio)
DistIn<-DistIn$Btotal#Creamos la matriz
DistTR<-distm(Transectos,fun = distGeo)
Mantel<-mantel(DistIn,DistTR,method="spearman")
Mantel
