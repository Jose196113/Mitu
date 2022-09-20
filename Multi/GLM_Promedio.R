setwd("~/Descargas/Multivariado/Transectos")
library(readxl)
library(vegan)
library(ggplot2)
library(Metrics)
library(DescTools)
Transectos<-read_xlsx("Variables1.xlsx")

##La exploracion por esa mierda de puntos es un dolor de huevos asi que lo mejor es coger todos las funciones esas
##de mierda y luego comparar entre ellas con akaike a ver si tus datos sirven o no

#Ensayamos con modelo lineal general
lm<-lm(Transectos$Incidencia~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3)
lm#Sinceramente creo q esto sobra
plot(lm)#Y esto tambien sobra, de igual manera en los que vienen jaja
summary(lm)


#Ensayamos con modelo lineal generalizado poisson
glm<-glm(Transectos$Incidencia~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = poisson(link = "log"))
glm
plot(glm)
summary(glm)


#Ensayamos con modelo lineal generalizado quasipoisson
glm1<-glm(Transectos$Incidencia~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = quasipoisson(link = "log"))
glm1
plot(glm1)
summary(glm1)


#Ensayamos con modelo lineal generalizado gaussiano
glm2<-glm(Transectos$Incidencia~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = gaussian(link = "identity"))
glm2
plot(glm2)
summary(glm2)

##Comparamos los modelos con akaike y con el error medio cuadrado porque en quasipoisson no se puede akaike
##tanto el error como el akaike se escoge el q sea mas bajito
AIC(lm)
AIC(glm)
AIC(glm1)
AIC(glm2)
rmse(Transectos$Incidencia, predict(lm))
rmse(Transectos$Incidencia, predict(glm))
rmse(Transectos$Incidencia, predict(glm1))
rmse(Transectos$Incidencia, predict(glm2))

##Despues de haber elegido el modelo mejor ajustado a los datos, eliminas las variables que no sea significativas
##O sea las que no tienen asteriscos ni puntos cuando le das en "summary(lo que sea)"
##Entonces haces lo mismo de arriba con menos variables
glm3<-glm(Transectos$Incidencia~Transectos$Troncos+Transectos$Luz,family = poisson(link = "log"))
glm3
plot(glm3)
summary(glm3)
AIC(glm3)

##Si siguen saliendo variables no significativas podes seguirlas eliminando pero ojo con el AIC
##Se supone que el modelo deberia mejorar sino es asi(sube el AIC) te quedas con el modelo anterior
glm4<-lm(Transectos$Incidencia~Transectos$Troncos)
glm4
plot(glm4)
summary(glm4)
#Comparamos
AIC(glm4)
rmse(Transectos$Incidencia, predict(glm3))
#Si tu modelo fue poisson(probablemente lo sea) sacas los valores de la funcion link con esto,
#Si fue lineal general no y solo tomas el del summary
exp(glm3$coefficients)
##Estos los graficos que deberian decirle que distribucion usar pero ni idea
plot(Transectos$Incidencia~Transectos$Troncos)
plot(Transectos$Incidencia~Transectos$Luz)
plot(Transectos$Incidencia~Transectos$Humedad)
plot(Transectos$Incidencia~Transectos$Temperatura)
plot(Transectos$Incidencia~Transectos$Zona)
plot(Transectos$Incidencia~Transectos$Temperatura+Transectos$Humedad+Transectos$Troncos)
##El grafico que resume tu modelo
install.packages("patchwork")
library(patchwork)
A<-ggplot(Transectos,aes(x=Troncos,y=Incidencia, col = Zona,)) + ggtitle("") + geom_point(cex=6) + theme(legend.position="none",text = element_text(size = 24))
B<-ggplot(Transectos,aes(x=Humedad,y=Incidencia, col = Zona)) + ggtitle("") + geom_point(cex=6) + theme(legend.position="none",text = element_text(size = 24))
C<-ggplot(Transectos,aes(x=Temperatura,y=Incidencia, col = Zona)) + ggtitle("") + geom_point(cex=6) + theme(legend.position="none",text = element_text(size = 24))

A+B+C + plot_layout(nrow = 3,ncol = 1)

######################################################################### Por riqueza
setwd("~/Descargas/Multivariado/Transectos")
library(readxl)
library(vegan)
library(ggplot2)
library(Metrics)
library(DescTools)
Transectos<-read_xlsx("Variables1.xlsx")

lm<-lm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3)
lm
plot(lm)
summary(lm)

glm<-glm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = poisson(link = "log"))
glm
plot(glm)
summary(glm)


glm1<-glm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = quasipoisson(link = "log"))
glm1
plot(glm1)
summary(glm1)


glm2<-glm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = gaussian(link = "identity"))
glm2
plot(glm2)
summary(glm2)

AIC(lm)
AIC(glm)
AIC(glm1)
AIC(glm2)
rmse(Transectos$Riqueza, predict(lm))
rmse(Transectos$Riqueza, predict(glm))
rmse(Transectos$Riqueza, predict(glm1))
rmse(Transectos$Riqueza, predict(glm2))


#glm3<-lm(Transectos$Riqueza~Transectos$Luz3+Transectos$Altitud)
glm3<-glm(Transectos$Riqueza~Transectos$Luz3+Transectos$Altitud,  family = poisson(link = "log"))
glm3
plot(glm3)
summary(glm3)
AIC(glm3)

#glm4<-lm(Transectos$Riqueza~Transectos$Luz3)
glm4<-glm(Transectos$Riqueza~Transectos$Luz3, family = poisson(link = "log"))
glm4
plot(glm4)
summary(glm4)
AIC(glm4)
rmse(Transectos$Riqueza, predict(glm3))
rmse(Transectos$Riqueza, predict(glm4))

exp(glm4$coefficients)

plot(Transectos$Riqueza~Transectos$Troncos)
plot(Transectos$Riqueza~Transectos$Humedad)
plot(Transectos$Riqueza~Transectos$Temperatura)
plot(Transectos$Riqueza~Transectos$Zona)
plot(Transectos$Riqueza~Transectos$Altitud)
plot(Transectos$Riqueza~Transectos$Temperatura+Transectos$Humedad+Transectos$Troncos)
install.packages("patchwork")
library(patchwork)
A<-ggplot(Transectos,aes(x=Troncos,y=Incidencia, col = Zona,)) + ggtitle("") + geom_point(cex=6) + theme(legend.position="none",text = element_text(size = 24))
B<-ggplot(Transectos,aes(x=Humedad,y=Incidencia, col = Zona)) + ggtitle("") + geom_point(cex=6) + theme(legend.position="none",text = element_text(size = 24))
C<-ggplot(Transectos,aes(x=Temperatura,y=Incidencia, col = Zona)) + ggtitle("") + geom_point(cex=6) + theme(legend.position="none",text = element_text(size = 24))
riqueza<-ggplot(Transectos,aes(x=Luz3,y=Riqueza, col = Zona)) + ggtitle("Modelo lineal generalizado(Poisson)") + geom_point(cex=6) + theme(text = element_text(size = 36),plot.title = element_text(hjust = 0.5))+ 
  xlab("% Cobertura de dosel") + geom_point() +geom_segment(aes(x = min(Luz3), y = min(Riqueza), xend = max(Luz3), yend = max(Riqueza)))#+ geom_smooth(method="glm")
riqueza
A+B+C + plot_layout(nrow = 3,ncol = 1)
