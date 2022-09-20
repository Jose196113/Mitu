library(stats)
library(indicspecies)
library(readxl)


################################################################ por trampa
setwd("~/Descargas/Beta/Ceros")
Especies<- read.csv("Especies.csv")
Especies
Sitio<-as.matrix(read.csv("Zonas.csv"))
Sitio

Indval = multipatt(Especies,Sitio)
Indval
sink("Output_IndVal.txt")
summary(Indval)
sink()

plotcoverage(Especies,Indval, group = 1)
plotcoverage(Especies,Indval, group = 2,col="blue", add=TRUE)
plotcoverage(Especies,Indval, group = 3,col="red", add=TRUE)
legend(x = 0.01, y=20,legend=c("Bosque","Chagra", "Urbano"),lty=c(1,2,3), col=c("black","blue","red"), bty="n")


################################################################## Por transecto

library(stats)
library(indicspecies)

setwd("~/Descargas/Multivariado")

Especies<- read_excel("TR1.xlsx")
Especies
Zonas<-read_excel("Variables1.xlsx")
Zonas<-Zonas$Zona

Indval = multipatt(Especies,Zonas)
Indval
sink("Output_IndVal.txt")
summary(Indval)
sink()

plotcoverage(Especies,Indval, group = 1)
plotcoverage(Especies,Indval, group = 2,col="blue", add=TRUE)
plotcoverage(Especies,Indval, group = 3,col="red", add=TRUE)
legend(x = 0.01, y=20,legend=c("Bosque","Chagra", "Urbano"),lty=c(1,2,3), col=c("black","blue","red"), bty="n")

