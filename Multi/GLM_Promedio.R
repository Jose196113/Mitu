setwd("~/Rstudio/Mitu/Multi")
library(readxl)
library(vegan)
library(ggplot2)
library(Metrics)
library(DescTools)
library(patchwork)
library(car)
library(GGally)
library(AICcmodavg)
Transectos<-read_xlsx("Variables1.xlsx")

#Analisis preliminar
A<-data.frame(Transectos$Riqueza,Transectos$Hojarasca3,Transectos$Altitud,Transectos$Dist_Centro,Transectos$Luz3,Transectos$Temperatura3,Transectos$Humedad3,Transectos$Troncos3)
colnames(A)<-c("Riqueza","Hojarasca","Altitud","Distancia_Centro_Urbano","Cobertura_dosel","Temperatura","Humedad", "Troncos")

Ceros<-sum(Transectos$Riqueza == 0) * 100 / nrow(Transectos)
Ceros

#Observamos la linealidad de las variables
a<-ggplot(A,aes(y=Riqueza,x=Hojarasca,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
b<-ggplot(A,aes(y=Riqueza,x=Troncos,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
c<-ggplot(A,aes(y=Riqueza,x=Humedad,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
d<-ggplot(A,aes(y=Riqueza,x=Temperatura,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
e<-ggplot(A,aes(y=Riqueza,x=Altitud,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
f<-ggplot(A,aes(y=Riqueza,x=Distancia_Centro_Urbano,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
g<-ggplot(A,aes(y=Riqueza,x=Cobertura_dosel,col=Transectos$Zona))+geom_point()+theme(legend.position="none")
a+b+c+d+e+f+g+plot_layout(nrow = 4,ncol = 2)

#Correlacion entre variables(Multicolinealidad)
ggpairs(A,ggplot2::aes(colour=Transectos$Zona),title="Correlación entre las variables") +
  theme(plot.title = element_text(hjust = 0.5))
ggpairs(A)+theme(text = element_text(size = 11, face = "bold"))

######################################################################### Evaluando los modelos lineales

#Ensayamos con modelo lineal general
lm<-lm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3)
lm
plot(lm)
summary(lm)
#Ensayamos con modelo lineal generalizado poisson
glm<-glm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = poisson(link = "log"))
glm
plot(glm)
summary(glm)
#Esta parte es porque seleccionamos poisson por tener datos discretos
#Probamos lo colinealidad
multicolinealidad<-vif(glm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+
                             Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+
                             Transectos$Humedad3+Transectos$Troncos3,family = poisson, data = Transectos))
multicolinealidad # Si es >3 alta colinealidad

#Eliminamos la variable con la colinealidad mas alta
multicolinealidad<-vif(glm(Transectos$Riqueza~Transectos$Altitud+
                             Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+
                             Transectos$Humedad3+Transectos$Troncos3,family = poisson, data = Transectos))
multicolinealidad # Si es >3 alta colinealidad

#Eliminamos la variable con la colinealidad mas alta
multicolinealidad<-vif(glm(Transectos$Riqueza~Transectos$Altitud+
                             Transectos$Dist_Centro+Transectos$Luz3+
                             Transectos$Humedad3+Transectos$Troncos3,family = poisson, data = Transectos))
multicolinealidad # Si es >3 alta colinealidad

#Modelo sin variables correlacionadas
glm5<-glm(Transectos$Riqueza~Transectos$Altitud+
           Transectos$Dist_Centro+Transectos$Luz3+
           Transectos$Humedad3+Transectos$Troncos3,family = poisson)

#Medimos la sobredispersion de los datos
dispersion <- glm5$deviance / glm5$df.residual 
dispersion #si es >1.2 tenemos problemas

cooks.distance(glm)#Para detectar outliers que esten molestando en el ajuste
plot(cooks.distance(glm), xlab = "Observation", ylab = "Cook's distance", 
     type = "h", ylim = c(0, 1.1), cex.lab = 1.5)
abline(h = 1, lty = 2)# Si supera la linea tenemos observaciones con muchos outliers creando ruido
AIC(glm5)
extractAIC(glm5,AIC="AICc")
AICc(glm5, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 




#Ensayamos con modelo lineal generalizado quasipoisson
glm1<-glm(Transectos$Riqueza~Transectos$Hojarasca3+Transectos$Altitud+Transectos$Dist_Centro+Transectos$Luz3+Transectos$Temperatura3+Transectos$Humedad3+Transectos$Troncos3, family = quasipoisson(link = "log"))
glm1
plot(glm1)
summary(glm1)

#Ensayamos con modelo lineal generalizado gaussiano
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
AICc(lm, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 
AICc(glm, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 
AICc(glm1, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 
AICc(glm2, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 
AICc(glm3, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 
AICc(glm4, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 
AICc(glm5, return.K = FALSE, c.hat = 1, second.ord = TRUE, nobs = NULL) 


#Modelo con las variables que son signficativas
glm3<-glm(Transectos$Riqueza~Transectos$Luz3+Transectos$Altitud,  family = poisson(link = "log"))
glm3
plot(glm3)
summary(glm3)
AIC(glm3)

multicolinealidad<-vif(glm(Transectos$Riqueza~Transectos$Altitud++Transectos$Luz3,family = poisson, data = Transectos))
multicolinealidad # Si es >3 alta colinealidad

#Modelo con las variables mas significativas
#glm4<-lm(Transectos$Riqueza~Transectos$Luz3)
glm4<-glm(Transectos$Riqueza~Transectos$Luz3, family = poisson(link = "log"))
glm4
plot(glm4)
summary(glm4)
AIC(glm4)
rmse(Transectos$Riqueza, predict(glm3))
rmse(Transectos$Riqueza, predict(glm4))


dispersion <- glm4$deviance / glm4$df.residual 
dispersion #si es >1.2 tenemos problemas
plot(cooks.distance(glm4), xlab = "Observation", ylab = "Cook's distance", 
     type = "h", ylim = c(0, 1.1), cex.lab = 1.5)
abline(h = 1, lty = 2)
#Extraemos los coeficientes del modelo
exp(glm4$coefficients)

riqueza<-ggplot(Transectos,aes(x=Luz3,y=Riqueza, col = Zona)) + ggtitle("Modelo lineal generalizado(Poisson)") + geom_point(cex=6) + theme(text = element_text(size = 36),plot.title = element_text(hjust = 0.5))+ 
          xlab("% Cobertura de dosel") + geom_point() +geom_segment(aes(x = min(Luz3), y = min(Riqueza), xend = max(Luz3), yend = max(Riqueza)))#+ geom_smooth(method="glm")
riqueza

