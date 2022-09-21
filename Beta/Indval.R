library(stats)
library(indicspecies)
library(readxl)


################################################################ por trampa
setwd("~/Rstudio/Mitu/Beta")
Especies<- read_excel("Especies.xlsx")
Especies
Sitio<-as.matrix(read.csv("Zonas.csv"))
Sitio

Indval = multipatt(Especies,Sitio, control = how(nperm=999))
Indval
sink("Output_IndVal_UM.txt")
summary(Indval, indvalcomp=TRUE)
sink()

################################################################## Por transecto

setwd("~/Rstudio/Mitu/Multi")

Especies<- read_excel("TR1.xlsx")
Especies
Especies = replace(Especies, Especies >0 , 1)
Zonas<-read_excel("Variables1.xlsx")
Zonas<-Zonas$Zona

Indval = multipatt(Especies,Zonas)
Indval
sink("Output_IndVal_TR.txt")
summary(Indval)
sink()


