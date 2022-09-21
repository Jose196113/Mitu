library(vegan)
library(readxl)
library(ggplot2)
############################################################################# Anosim-Simper Por UM
setwd("~/Rstudio/Mitu/Multi")

Especies<-as.matrix(read_excel("Especies1.xlsx"))
Especies
Zonas<-as.matrix(read.csv("Zonas2.csv"))
Zonas<-as.matrix(Zonas)
Distancias<-vegdist(Especies, method = "jaccard", binary = TRUE, na.rm = TRUE)
Distancias
Anosim<-anosim(Distancias,Zonas, distance = "jaccard")
Zonas1<-read.csv("Zonas1.csv")#Para simper
Simper<-simper(Especies, Zonas1$Zonas)
Anosim
Simper
plot(Anosim)
################################################################################ Anosim-Simper Por transecto
setwd("~/Rstudio/Mitu/Multi")

Especies<-read_excel("TR1.xlsx")
Especies = replace(Especies, Especies >0 , 1)
Especies<-as.matrix(Especies)
##Especies<-Especies[rowSums(Especies[])>0,]##Por si se necesita extraer los ceros
Especies
Zonas<-read_excel("Variables1.xlsx")
Zonas
Distancias<-vegdist(Especies)
Distancias
Anosim<-anosim(Distancias,Zonas$Zona, distance = "jaccard")
Simper<-simper(Especies, Zonas$Zona)
Anosim
Simper
plot(Anosim)
write.csv(Simper,file = "Simper_out.csv")
################################################################################ NMDS UM
library(vegan)
library(readxl)
library(ggplot2)
library(tidyverse)
Especies<-read_excel("Especies1.xlsx")
Especies
Distancias<-vegdist(Especies,na.rm = TRUE)
Distancias
NMDS<-metaMDS(Distancias)
plot(NMDS)
Zonas<-read.csv("Zonas1.csv", header = TRUE, sep = ",")
Zonas<-Zonas$Zonas
DF<-NMDS$points %>% as.data.frame() %>% bind_cols(Zonas)
DF
ggplot(DF,aes(x=MDS1,y=MDS2, col=Zonas)) + geom_point(cex=4) + theme_bw(base_size = 24)
Variables<-read_excel("Variables1.xlsx",col_names = TRUE)
Variables<-as.data.frame(Variables)

DF<-NMDS$points %>% as.data.frame() %>% bind_cols(Variables$Zona,Variables$ID,Variables$Hojarasca,Variables$Altitud,Variables$Dist_Centro,Variables$Luz,Variables$Temperatura,Variables$Humedad)
colnames(DF)<-c("MDS1","MDS2","Zona","ID","Hojarasca","Altitud","Distancia_Centro_Urbano","Cobertura_dosel","Temperatura","Humedad")

ef1 <- envfit(NMDS, DF, permu = 999)
ef1

Vectores = as.data.frame(scores(ef1, "vectors")) * ordiArrowMul(ef1)
ggplot(DF,aes(x=MDS1,y=MDS2, col = Zonas)) + ggtitle("Incidencia vs Variables bióticas y ambientales") + geom_point(cex=6) + theme_bw(base_size = 24) +  
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = Vectores, size =1, alpha = 0.5, colour = "grey30") +
  geom_point(data = Vectores, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_text(data = Vectores, aes(x = NMDS1, y = NMDS2+0.03), label = row.names(Vectores), colour = "navy", fontface = "bold",position=position_jitter(width=0.02,height=0.012))


################################################################################ NMDS Por transectos
library(vegan)
library(readxl)
library(ggplot2)
library(tidyverse)
library(svglite)

setwd("~/Rstudio/Mitu/Multi")

Transectos<-read_excel("TR.xlsx")
Transectos<-Transectos[,c(-1,-2)]
Transectos<-as.matrix(Transectos)

Distancias1<-vegdist(Transectos, method = "jaccard", binary = TRUE, na.rm = TRUE)

Usos<-read_excel("TR.xlsx")
Usos<-as.matrix(Usos[,2])
Variables<-read_excel("Variables1.xlsx",col_names = TRUE)
Variables<-as.data.frame(Variables)

NMDS1<- metaMDS(Distancias1)
plot(NMDS1)  
NMDS1$stress

DF2<-NMDS1$points %>% as.data.frame() %>% bind_cols(Variables$ID,Variables$Hojarasca,Variables$Altitud,Variables$Dist_Centro,Variables$Luz,Variables$Temperatura,Variables$Humedad)
colnames(DF2)<-c("MDS1","MDS2","ID","Hojarasca","Altitud","Distancia_Centro_Urbano","Cobertura_dosel","Temperatura","Humedad")

ggplot(DF2,aes(x=MDS1,y=MDS2, col = Usos)) + ggtitle("Riqueza vs Usos de suelo") + geom_point(cex=6) + theme_bw(base_size = 24) + stat_ellipse()
ef <- envfit(NMDS1, DF2, permu = 999)

Vectores = as.data.frame(scores(ef, "vectors")) * ordiArrowMul(ef)

ggplot(DF2,aes(x=MDS1,y=MDS2, col = Usos)) + ggtitle("Riqueza vs Variables bióticas y ambientales") + geom_point(cex=6) +  
        geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), data = Vectores, size =1, alpha = 0.5, colour = "grey30") +
        geom_point(data = Vectores, aes(x = NMDS1, y = NMDS2), shape = "diamond", size = 8, alpha = 0.6, colour = "navy") +
        stat_ellipse(level = 0.85) + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
        #geom_text(data = Vectores, aes(x = NMDS1, y = NMDS2+0.03), label = row.names(Vectores), colour = "navy", fontface = "bold",position=position_jitter(width=0.02,height=0.012)) +
        geom_text(data=DF2,aes(x=MDS1,y=MDS2,label=ID),alpha=0.5, nudge_y = -0.020) +
        annotate(geom="text", x=0.55, y=-0.50, label="Stress =",color="red") +
        annotate(geom="text", x=0.55, y=-0.54, label=NMDS1$stress,color="red")


