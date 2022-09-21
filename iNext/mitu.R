#####Código para estimación y comparación de la diversidad qD#########
###Se incluye una breve descripción de cada paso, de esta forma será posible replicar los resultados que se presentan en el capítulo#######

install.packages("iNEXT")
library(iNEXT)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)

setwd("~/Rstudio/Mitu/iNext")
Bosque<-read_excel("Bosque1.xlsx")
mBosque <- as.matrix(apply(Bosque[,-1],2,as.integer))#Eliminamos las columnas que no usa el input
Chagra<-read_excel("Chagra1.xlsx")
mChagra <- as.matrix(apply(Chagra[,-1],2,as.integer))#Eliminamos las columnas que no usa el input
Urbano<-read_excel("Urbano1.xlsx")
mUrbano <- as.matrix(apply(Urbano[,-1],2,as.integer))#Eliminamos las columnas que no usa el input

data<-list(Bosque=mBosque,Chagra=mChagra,Urbano=mUrbano)#EL input tiene que ser una lista

out <- iNEXT(data, q=c(0, 1, 2), datatype="incidence_raw")#Los argumentos van por defecto, solo agregamos los numeros de Hill y datos de incidencia
summary(out)

out_qdata <- out$iNextEst
write.table(out_qdata, "out_qdata1.txt")#Tabla de datos que usamos aunque sinceramente eso sale todo desordenado y confunde mas de lo que ayuda
out#Esto es mas dicienciente
summary(out_qdata)

out1$iNextEst$coverage_based %>% filter(Method == "Observed")#Porcentajes de cobertura

#Graficamos finalmente
ggiNEXT(out1, type = 1, se = TRUE, facet.var = "Order.q", color.var = "Assemblage", grey = FALSE) +
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 36)) +
          xlab("Unidades de muestreo") + ylab("Riqueza") +
          scale_colour_manual(values=c("red", "blue","green")) +
          scale_fill_manual(values=c("red", "blue","green")) 
ggiNEXT(out, type = 2, se = TRUE, facet.var = "Order.q", color.var = "Assemblage", grey = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 36)) +
  xlab("Unidades de muestreo") + ylab("Cobertura de muestreo") + 
  scale_colour_manual(values=c("red", "blue","green")) +
  scale_fill_manual(values=c("red", "blue","green")) 

#ggiNEXT(out, type = 3, se = TRUE, facet.var = "order", color.var = "site", grey = FALSE) +
#theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 40)) +
#xlab("Cobertura de muestreo") + ylab("Riqueza")

####################################################################################
######################Magnitud de la diferencia#####################################
####################################################################################

#Cargar tabla "MD.csv" que permitirá construir la Fig. 3.6

dMD <- read.csv("MD2.csv", header = T, sep=",")#El indice se calcula de la formula %MD= 100-((Muestra2*100)/Muestra1) y se ponen los sitios comparandolos en parejas

##Para ordenar la información.
dMD$Diversidad <- factor(dMD$Diversidad, levels = c("2D", "1D", "0D"))
dMD$Cobertura <- factor(dMD$Cobertura, levels = c("Bosque-Chagra", "Chagra-Urbano", "Bosque-Urbano"))

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


