library(BAT)
library(gtools)
library(vegan)
library(readxl)

setwd("~/Descargas/Beta/Ceros")

Bosque <- read.csv("Bosque.csv")
Bosque = replace(Bosque, Bosque >0 , 1)
Chagra <- read.csv("Chagra.csv")
Chagra = replace(Chagra, Chagra >0 , 1)
Urbano <- read.csv("Urbano.csv")
Urbano = replace(Urbano, Urbano >0 , 1)

Bosquecolumna <- colSums(Bosque)
Chagracolumna <- colSums(Chagra)
Urbanocolumna <- colSums(Urbano)
write.csv(t(Bosquecolumna), "Bosquecolumna.csv", row.names=F)
write.csv(t(Chagracolumna), "Chagracolumna.csv", row.names=F)
write.csv(t(Urbanocolumna), "Urbanocolumna.csv", row.names=F)
Bosquecol <- read.csv("Bosquecolumna.csv")
Chagracol <- read.csv("Chagracolumna.csv")
Urbanocol <- read.csv("Urbanocolumna.csv")
Zonas <- smartbind(Bosquecol,Chagracol,Urbanocol,fill=0)
Zonasorden <- Zonas[,order(names(Zonas))] #to sort alphabetically
Zonasnombre <- c('Bosque','Chagra','Urbano')
row.names(Zonasorden) <- Zonasnombre
write.csv(Zonasorden, "species_abundance.csv")	
sink("alpha_beta_Zonas.txt")
alpha(Zonasorden)
alpha.estimate(Zonasorden)
beta(Zonasorden) #jaccard para incidencia
beta(Zonasorden, func="soerensen")
raref.td <- alpha(Zonasorden, raref = 34)
raref.td
sink()
pdf("boxplot_rare_BAT.pdf",width=6,height=8,paper='special')
boxplot(t(raref.td), names = Zonasnombre, main='Usos de suelo', pars = list (boxwex = 0.5, staplewex = 0.5, outwex = 0.5), range=0, lwd=0.8, lty=1, col = c('grey30', 'grey60', 'grey90'), ylab = expression(alpha))
dev.off()
acc.riqueza_Bosque<- alpha.accum(Bosque)
acc.riqueza_Chagra<- alpha.accum(Chagra)
acc.riqueza_Urbano<- alpha.accum(Urbano)
write.table(acc.riqueza_Bosque, "acc_riqueza_Bosque.csv", sep=",", row.names=F)
write.table(acc.riqueza_Chagra, "acc_riqueza_Chagra.csv", sep=",", row.names=F)
write.table(acc.riqueza_Urbano, "acc_riqueza_Urbano.csv", sep=",", row.names=F)
pdf("estimates_Bosque.pdf",width=6,height=8,paper='special')
par(mfrow=c(2,2))
plot(acc.riqueza_Bosque[,2], acc.riqueza_Bosque[,17], xlab="Individuos", ylab="Chao1p", cex.lab = 1)
plot(acc.riqueza_Bosque[,2], slope(acc.riqueza_Bosque)[,17], xlab="Individuos", ylab="Slope", cex.lab = 1)
plot(acc.riqueza_Bosque[,2], acc.riqueza_Bosque[,9], xlab="Individuos", ylab="Jack1abP", cex.lab = 1)
plot(acc.riqueza_Bosque[,2], slope(acc.riqueza_Bosque)[,9], xlab="Individuos", ylab="Slope", cex.lab = 1)
dev.off()
pdf("estimates_Chagra.pdf",width=6,height=8,paper='special')
par(mfrow=c(2,2))
plot(acc.riqueza_Chagra[,2], acc.riqueza_Chagra[,17], xlab="Individuos", ylab="Chao1p", cex.lab = 1)
plot(acc.riqueza_Chagra[,2], slope(acc.riqueza_Chagra)[,17], xlab="Individuos", ylab="Slope", cex.lab = 1)
plot(acc.riqueza_Chagra[,2], acc.riqueza_Chagra[,9], xlab="Individuos", ylab="Jack1abP", cex.lab = 1)
plot(acc.riqueza_Chagra[,2], slope(acc.riqueza_Chagra)[,9], xlab="Individuos", ylab="Slope", cex.lab = 1)
dev.off()
pdf("estimates_Urbano.pdf",width=6,height=8,paper='special')
par(mfrow=c(2,2))
plot(acc.riqueza_Urbano[,2], acc.riqueza_Urbano[,17], xlab="Individuos", ylab="Chao1p", cex.lab = 1)
plot(acc.riqueza_Urbano[,2], slope(acc.riqueza_Urbano)[,17], xlab="Individuos", ylab="Slope", cex.lab = 1)
plot(acc.riqueza_Urbano[,2], acc.riqueza_Urbano[,9], xlab="Individuos", ylab="Jack1abP", cex.lab = 1)
plot(acc.riqueza_Urbano[,2], slope(acc.riqueza_Urbano)[,9], xlab="Individuos", ylab="Slope", cex.lab = 1)
dev.off()
library(ggplot2)
df1 <- data.frame(acc.riqueza_Bosque)
p <- ggplot(df1, aes(x = Ind)) + geom_line(df1 = df1,aes(y = Obs, colour="Obs")) + geom_line(df1 = df1,aes(y = Chao1, colour="Chao1")) + geom_line(df1 = df1,aes(y = Jack1abP, colour="Jack1abP"))  + geom_line(df1 = df1,aes(y = Jack2abP, colour="Jack2abP")) + geom_line(df1 = df1,aes(y = Chao2, colour="Chao2")) + ylab("Riqueza") + xlab("Individuos") + ggtitle("Bosque") + scale_color_manual(name = "Indices", breaks=c("Obs","Chao1","Jack1abP","Jack2abP","Chao2"), values=c("darkorange2","red","green","blue","cyan"))
p + theme(plot.title = element_text (face="bold", size=16, hjust = 0.5), axis.title.x = element_text(face="bold", size=16), axis.title.y = element_text(face="bold", size=16), legend.title = element_text(size=16), legend.text=element_text(size=14), legend.key = element_rect(fill="white"))
ggsave("Accu_curves_Bosque.pdf", width=10,height=5)
df2 <- data.frame(acc.riqueza_Chagra)
p <- ggplot(df2, aes(x = Ind)) + geom_line(df2 = df2,aes(y = Obs, colour="Obs")) + geom_line(df2 = df2,aes(y = Chao1, colour="Chao1")) + geom_line(df2 = df2,aes(y = Jack1abP, colour="Jack1abP"))  + geom_line(df2 = df2,aes(y = Jack2abP, colour="Jack2abP")) + geom_line(df2 = df2,aes(y = Chao2, colour="Chao2")) + ylab("Riqueza") + xlab("Individuos") + ggtitle("Chagra") + scale_color_manual(name = "Indices", breaks=c("Obs","Chao1","Jack1abP","Jack2abP","Chao2"), values=c("darkorange2","red","green","blue","cyan"))
p + theme(plot.title = element_text (face="bold", size=16, hjust = 0.5), axis.title.x = element_text(face="bold", size=16), axis.title.y = element_text(face="bold", size=16), legend.title = element_text(size=16), legend.text=element_text(size=14), legend.key = element_rect(fill="white"))
ggsave("Accu_curves_Chagra.pdf", width=10,height=5)
df3 <- data.frame(acc.riqueza_Urbano)
p <- ggplot(df3, aes(x = Ind)) + geom_line(df3 = df3,aes(y = Obs, colour="Obs")) + geom_line(df3 = df3,aes(y = Chao1, colour="Chao1")) + geom_line(df3 = df3,aes(y = Jack1abP, colour="Jack1abP"))  + geom_line(df3 = df3,aes(y = Jack2abP, colour="Jack2abP")) + geom_line(df3 = df3,aes(y = Chao2, colour="Chao2")) + ylab("Riqueza") + xlab("Individuos") + ggtitle("Urbano") + scale_color_manual(name = "Indices", breaks=c("Obs","Chao1","Jack1abP","Jack2abP","Chao2"), values=c("darkorange2","red","green","blue","cyan"))
p + theme(plot.title = element_text (face="bold", size=16, hjust = 0.5), axis.title.x = element_text(face="bold", size=16), axis.title.y = element_text(face="bold", size=16), legend.title = element_text(size=16), legend.text=element_text(size=14), legend.key = element_rect(fill="white"))
ggsave("Accu_curves_Urbano.pdf", width=10,height=5)
Simpson <- diversity(Zonasorden, index="simpson")
Shannon <- diversity(Zonasorden)
pdf("plot_diversity.pdf",width=6,height=8,paper='special')
boxplot(t(Simpson), names = Zonasnombre, ylab="Simpson")
boxplot(t(Shannon), names = Zonasnombre, ylab="Shannon")
dev.off()
hill <- renyi(Zonasorden, hill = TRUE) #
q0 <- data.frame(hill[,c(1)]) #
q1 <- data.frame(hill[,c(4)]) #
q2 <- data.frame(hill[,c(5)]) #
row.names(q0) <- Zonasnombre #
row.names(q1) <- Zonasnombre #
row.names(q2) <- Zonasnombre #
pdf("plot_diversity_hill.pdf",width=6,height=8,paper='special') #
par(mfrow=c(3,1))
boxplot(t(q0), ylab="q0") #
boxplot(t(q1), ylab="q1") #
boxplot(t(q2), ylab="q2") #
dev.off() #
sink("diversity_per_method.txt") #
Simpson #
Shannon #
hill #
sink() #
sink("diversity_per_sample_simpson.txt")
SimpsonBosque <- diversity(Bosque, index="simpson")
SimpsonChagra <- diversity(Chagra, index="simpson")
SimpsonUrbano <- diversity(Urbano, index="simpson")
SimpsonBosque
SimpsonChagra
SimpsonUrbano
sink()
pdf("boxplot_diversity_simpson.pdf",width=6,height=8,paper='special')
colors = terrain.colors(6)[5:1]
boxplot(SimpsonBosque, SimpsonChagra, SimpsonUrbano, names=Zonasnombre, boxwex=0.5, col=colors, cex.axis=0.5, ylab="Simpson")
dev.off()
sink("diversity_per_sample_hill.txt") #
hillBosque <- renyi(Bosque, hill = TRUE) #
hillChagra <- renyi(Chagra, hill = TRUE) #
hillUrbano <- renyi(Urbano, hill = TRUE) #
hillBosque #
hillChagra #
hillUrbano #
sink() #
q2Bosque <- (hillBosque[,c(5)]) #
q2Chagra <- (hillChagra[,c(5)]) #
q2Urbano <- (hillUrbano[,c(5)]) #
pdf("boxplot_diversity_q2.pdf",width=6,height=8,paper='special') #
colors = terrain.colors(6)[5:1] #
boxplot(q2Bosque, q2Chagra, q2Urbano, names=Zonasnombre, boxwex=0.5, col=colors, cex.axis=0.5, ylab="q2") #
dev.off() #
sink("diversity_per_sample_shannon.txt")
ShannonBosque <- diversity(Bosque)
ShannonChagra <- diversity(Chagra)
ShannonUrbano <- diversity(Urbano)
ShannonBosque
ShannonChagra
ShannonUrbano
sink()
pdf("boxplot_diversity_shannon.pdf",width=6,height=8,paper='special')
colors = terrain.colors(6)[5:1]
boxplot(ShannonBosque, ShannonChagra, ShannonUrbano, names=Zonasnombre, boxwex=0.5, col=colors, cex.axis=0.5, ylab="Shannon")
dev.off()
q1Bosque <- (hillBosque[,c(4)]) #
q1Chagra <- (hillChagra[,c(4)]) #
q1Urbano <- (hillUrbano[,c(4)]) #
pdf("boxplot_diversity_q1.pdf",width=6,height=8,paper='special') #
colors = terrain.colors(6)[5:1] #
boxplot(q1Bosque, q1Chagra, q1Urbano, names=Zonasnombre, boxwex=0.5, col=colors, cex.axis=0.5, ylab="q1") #
dev.off() #
pdf("hist_simpsondiversity.pdf",width=8,height=6,paper='special')
par(mfrow = c(1, 3))
hist(SimpsonBosque)
hist(SimpsonChagra)
hist(SimpsonUrbano)
dev.off()
pdf("hist_shannondiversity.pdf",width=8,height=6,paper='special')
par(mfrow = c(1, 3))
hist(ShannonBosque)
hist(ShannonChagra)
hist(ShannonUrbano)
dev.off()
sink("rare_vegan.txt")
raremin <- 18
sRare <- rarefy(Zonasorden, raremin)
sRare
sink()
pdf("rare_curve.pdf",width=6,height=8,paper='special')
rarecurve(Zonasorden)
dev.off()
sink("dissimilarity_vegan.txt")
bray <- vegdist(Zonasorden, "bray")
jaccard <- vegdist(Zonasorden, "jaccard")
morisita <- vegdist(Zonasorden, "morisita")
bray
jaccard
morisita
sink()
pdf("dissimilarity.pdf",width=8,height=6,paper='special')
par(mfrow = c(1, 3))
plot(hclust(bray, method="complete"))
plot(hclust(jaccard, method="complete"))
plot(hclust(morisita, method="complete"))
dev.off()

###################################################### Diagrama de barras jaccard
A<-read_excel("A1.xlsx")
library(ggplot2)
ggplot(A, aes(x = Zonas, y = Disimilaridad, fill = Componentes)) + geom_bar(stat = "identity") +
  scale_y_continuous(breaks=seq(0,1,by=0.1)) + ggtitle("Diversidad beta (Jaccard)") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28))


################################################################ Diagrama de venn
library(devtools)
devtools::install_github("gaospecial/ggVennDiagram")
library(ggVennDiagram)
library(ggplot2)
library(ggrepel)
A<-read_excel("Compartidos1.xlsx")
B<-ggVennDiagram(A, label_size = 10,set_size = 8) 
B + scale_fill_distiller(palette = "RdBu") 
