##################################################Cobertura
library(iNEXT)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
setwd("~/Rstudio/Mitu/Dias")
D02<-read.csv("D2.csv",header = T, sep = ";")
m2 <- as.matrix(apply(D02[,-1],2,as.integer))#Eliminamos las columnas que no usa el input
D15<-read.csv("D15.csv",header = T, sep = ";")
m15 <- as.matrix(apply(D15[,-1],2,as.integer))#Eliminamos las columnas que no usa el input
D30<-read.csv("D30.csv",header = T, sep = ";")
m30 <- as.matrix(apply(D30[,-1],2,as.integer))#Eliminamos las columnas que no usa el input

data<-list(D02=m2,D15=m15,D30=m30)#EL input tiene que ser una lista

str(data)
data
DataInfo(data, datatype="incidence_raw") #Estructura nada mas

out <- iNEXT(data, q=c(0, 1, 2), datatype="incidence_raw", endpoint = 28, nboot = 1000)#Los argumentos van por defecto, solo agregamos los numeros de Hill y datos de incidencia

summary(out)

out_qdata <- out$iNextEst
write.table(out_qdata, "out_qdata1.txt")#Tabla de datos que usamos aunque sinceramente eso sale todo desordenado y confunde mas de lo que ayuda
out#Esto es mas dicienciente
summary(out_qdata)

out$iNextEst$size_based %>% filter(Method == "Observed")#Porcentajes de cobertura(SC)
completitud<-data.frame(out$AsyEst$Assemblage,out$AsyEst$Diversity,100*(out$AsyEst$Observed/out$AsyEst$Estimator))
colnames(completitud)<-c("Zona","Hill","Completitud")
completitud

names(out_qdata$Bosque)#Solo para ver
#Graficamos finalmente
ggiNEXT(out, type = 1, se = TRUE, facet.var = "Order.q", color.var = "Assemblage", grey = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 36)) +
  xlab("Unidades de muestreo") + ylab("Riqueza") +
  scale_colour_manual(values=c("red", "blue","green")) +
  scale_fill_manual(values=c("red", "blue","green")) 
ggiNEXT(out, type = 2, se = TRUE, facet.var = "Order.q", color.var = "Assemblage", grey = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 36)) +
  xlab("Unidades de muestreo") + ylab("Cobertura de muestreo") + 
  scale_colour_manual(values=c("red", "blue","green")) +
  scale_fill_manual(values=c("red", "blue","green")) 


#######################################################################################################

dMD <- read.csv("MD1.csv", header = T, sep=",")#El indice se calcula de la formula %MD= 100-((Muestra2*100)/Muestra1) y se ponen los sitios comparandolos en parejas
dMD<-dMD[,c(-3,-4,-5)]
##Para ordenar la información.
dMD$Diversidad <- factor(dMD$Diversidad, levels = c("2D", "1D", "0D"))
dMD$Cobertura <- factor(dMD$Cobertura, levels = c("2 Días - 15 Días","2 Días – 30 Días","15 Días – 30 Días"))

ggplot(dMD, aes(x=Cobertura,y=X.MD,fill=Diversidad)) +
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  scale_fill_grey(breaks = c("0D","1D", "2D"))+
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100,100,50))+
  xlab("")+ylab("%MD")+
  theme(text=element_text(size=20, family = "TT Times New Roman", color = "black"))+ 
  theme(legend.position = c(0.18, 0.15))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(size = 2, color = "black", linetype = "solid", fill = NA), fill = NULL)+
  geom_hline(yintercept = 0, lty = 1, colour = "black", size = 1)
dMD
################################################################################### Kruskal-wallis

kruskal.test(data)
