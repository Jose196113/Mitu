library(BAT)
library(gtools)
library(vegan)
library(readxl)

setwd("~/Rstudio/Mitu/Beta")

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

Zonasorden = Zonasorden = replace(Zonasorden, Zonasorden >0 , 1)

write.csv(Zonasorden, "Especies_presencia.csv")	

sink("alpha_beta_Zonas.txt")
alpha(Zonasorden)
alpha.estimate(Zonasorden)
beta(Zonasorden,comp = TRUE) #jaccard para incidencia
#beta(Zonasorden, func="soerensen")
raref.td <- alpha(Zonasorden, raref = 34)
raref.td
sink()
acc.riqueza_Bosque<- alpha.accum(Bosque)
acc.riqueza_Chagra<- alpha.accum(Chagra)
acc.riqueza_Urbano<- alpha.accum(Urbano)
write.table(acc.riqueza_Bosque, "acc_riqueza_Bosque.csv", sep=",", row.names=F)
write.table(acc.riqueza_Chagra, "acc_riqueza_Chagra.csv", sep=",", row.names=F)
write.table(acc.riqueza_Urbano, "acc_riqueza_Urbano.csv", sep=",", row.names=F)
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

###################################################### Diagrama de barras jaccard
A<-read_excel("A2.xlsx")
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
A<-as.list(A)
B<-ggVennDiagram(A, label_size = 10,set_size = 8, edge_size = 3) 
B + theme(legend.position = "none")+scale_fill_gradient(low = "orange", high = "red")

