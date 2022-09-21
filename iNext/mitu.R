#####Código para estimación y comparación de la diversidad qD#########
###Se incluye una breve descripción de cada paso, de esta forma será posible replicar los resultados que se presentan en el capítulo#######
library(iNEXT)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
library(tidyverse)


setwd("~/Rstudio/Mitu/iNext")
Bosque<-read_excel("Bosque1.xlsx")
mBosque <- as.matrix(apply(Bosque[,-1],2,as.integer))#Eliminamos las columnas que no usa el input
Chagra<-read_excel("Chagra1.xlsx")
mChagra <- as.matrix(apply(Chagra[,-1],2,as.integer))#Eliminamos las columnas que no usa el input
Urbano<-read_excel("Urbano1.xlsx")
mUrbano <- as.matrix(apply(Urbano[,-1],2,as.integer))#Eliminamos las columnas que no usa el input

data<-list(Bosque=mBosque,Chagra=mChagra,Urbano=mUrbano)#EL input tiene que ser una lista

out <- iNEXT(data, q=c(0, 1, 2), datatype="incidence_raw", endpoint = 189, nboot = 1000)#Los argumentos van por defecto, solo agregamos los numeros de Hill y datos de incidencia
summary(out)

out_qdata <- out$iNextEst
write.table(out_qdata, "out_qdata1.txt")#Tabla de datos que usamos aunque sinceramente eso sale todo desordenado y confunde mas de lo que ayuda
out#Esto es mas dicienciente
summary(out_qdata)

out$iNextEst$size_based %>% filter(t == 189)#Porcentajes de cobertura(SC)
completitud<-data.frame(out$AsyEst$Assemblage,out$AsyEst$Diversity,100*(out$AsyEst$Observed/out$AsyEst$Estimator))
colnames(completitud)<-c("Zona","Hill","Completitud")
completitud

out_qdata <-as.data.frame(out_qdata)
write.csv(out_qdata %>% filter(size_based.t == 189),file = "tabla.csv")#De aqui salen los valores para el archivo MD.csv

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

##############################################################Escenario 1
escdb_1<-read_excel("Escenarios.xlsx")
D0.1 <-  ggplot(escdb_1, aes(x=Zona, y=Q0))+ geom_point(cex=3)+
  geom_errorbar(aes(ymin=Q0-ir0, ymax=Q0+ur0), width=.1, position=position_dodge(0.05),size=1.4)+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+
  ylab("Riqueza (Q0)")+
  theme(text=element_text(size=12))
D1.1 <- ggplot(escdb_1, aes(x=Zona, y=Q1))+ geom_point(cex=3)+
  geom_errorbar(aes(ymin=Q1-ir1, ymax=Q1+ur1), width=.1, position=position_dodge(0.05),size=1.4)+
  theme(axis.line = element_line(size=0.05, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("No. efectivo de especies (Q1)")+theme(text=element_text(size=12))
D2.1 <- ggplot(escdb_1, aes(x=Zona, y=Q2))+ geom_point(cex=3)+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("No. efectivo de especies (Q2)")+theme(text=element_text(size=12))+
  geom_errorbar(aes(ymin=Q2-ir2, ymax=Q2+ur2), width=.1, position=position_dodge(0.05),size=1.4)
gridplotD0 <- grid.arrange(D0.1, D1.1, D2.1,nrow = 1, ncol = 3)

#ggiNEXT(out, type = 3, se = TRUE, facet.var = "order", color.var = "site", grey = FALSE) +
#theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 40)) +
#xlab("Cobertura de muestreo") + ylab("Riqueza")

####################################################################################
######################Magnitud de la diferencia#####################################
####################################################################################

dMD <- read.csv("MD2.csv", header = T, sep=";")#El indice se calcula de la formula %MD= 100-((Muestra2*100)/Muestra1) y se ponen los sitios comparandolos en parejas

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

###################################################################### Curvas rango-abundancia
library(BiodiversityR)
library(ggrepel)
curva<-as.data.frame(read_excel("Curva.xlsx"))
curva<-curva[,-1]
curva1<-curva[1,]
curva2<-curva[2,]
curva3<-curva[3,]
E<-read_excel("Esp.xlsx")
f<-as.data.frame(rankabundance(curva))
f1<-as.data.frame(rankabundance(curva1))
f2<-as.data.frame(rankabundance(curva2))
f3<-as.data.frame(rankabundance(curva3))

rankabunplot(rankabundance(curva))
rankabunplot(rankabundance(curva1))
rankabunplot(rankabundance(curva2))
rankabunplot(rankabundance(curva3))

gf<-ggplot(f, aes(x = rank, y = abundance)) + 
  geom_line() + geom_point() + geom_text_repel(label=E$Especies,force = 4, max.overlaps = 15,box.padding = 0.5) +
  labs(x = "Especies", y = "Incidencia")

gf1<-ggplot(f1, aes(x = rank, y = abundance)) + 
  geom_line() + geom_point() + geom_text_repel(label=E$Especies1,force = 10, max.overlaps = 15,box.padding = 0.6) +
  labs(x = "Especies", y = "Incidencia(Bosque)")

gf2<-ggplot(f2, aes(x = rank, y = abundance)) + 
  geom_line() + geom_point() + geom_text_repel(label=E$Especies2,force = 4, max.overlaps = 15,box.padding = 0.5) +
  labs(x = "Especies", y = "Incidencia(Chagra)")

gf3<-ggplot(f3, aes(x = rank, y = abundance)) + 
  geom_line() + geom_point() + geom_text_repel(label=E$Especies3,force = 4, max.overlaps = 15,box.padding = 0.6) +
  labs(x = "Especies", y = "Incidencia(Urbano)")

grid.arrange(gf, gf1, gf2, gf3,nrow = 2, ncol = 2)
