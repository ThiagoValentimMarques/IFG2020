######################### Construção da Linha do Tempo ######################

#Carregando o pacote tidyverse

library(tidyverse) #será carregado para utilizar o Pipe (%>%)

#Local da pasta
caminho <- "C:\\Users\\Thiago\\Desktop\\Palestra - IFG\\conjuntos de dados\\Linha do Tempo\\linhadotempo.csv" 

#Carregando a tabela da linha do tempo
tabela <- read.csv(caminho,header=TRUE,sep=";")

#Acrescentando uma nova coluna para as cores não ficarem sobrepostas
tabela <- tabela %>%
  mutate(acontecimento = as.character(acontecimento),
         cor = c(rep(NA,23),rep(0,31),rep(NA,29),rep(0,31),rep(NA,30),
                 rep(0,31),rep(NA,18)))

#Retirar os valores faltantes (NA) para inserir os marcos da Covid-19
tabela2<-tabela[complete.cases(tabela$acontecimento),]
tabela2[,6]<-as.numeric(row.names(tabela2))
names(tabela2)[6]<-"linha"

#Posição na ordenada (y) das frases (lembrando que y estará entre -10 e 10)
ord <- c(6,-3,4,-7,9,-5,6,-2,3,-8,7,-5,5,-3,9,-1.3,2,-9.2,
         -7.5,6,3.5,-2.5,-6.5,5,-9,8,-5,10,-1,3)

par(mar=c(0,0,0,0),xpd=TRUE)
plot(NA,xlim=c(-8,dim(tabela)[1])+15,ylim=c(-10,10),ann=FALSE,axes=FALSE)
segments(tabela2$dias,0,tabela2$dias,ord,col="blue",lty=10)

points(tabela2$linha,ord,col="darkblue",lty=10,pch=16)
lines(tabela$dias,rep(0,dim(tabela)[1]),lwd=4,col="red4",type="l")
lines(tabela$dias,tabela$cor,lwd=4,col="darkolivegreen1",
      xlim=c(1,dim(tabela)[1]))

meses<-c("dez/2019","jan/2020","fev/2020","mar/2020","abr/2020","mai/2020",
         "jun/2020")
x<-tapply(tabela$dias,tabela$mes,mean)
text(x[7],0.3,labels=meses[1],cex=0.9,font=2)
text(x[1],-0.3,labels=meses[2],cex=0.9,font=2)
text(x[2],0.3,labels=meses[3],cex=0.9,font=2)
text(x[3],-0.3,labels=meses[4],cex=0.9,font=2)
text(x[4],0.3,labels=meses[5],cex=0.9,font=2)
text(x[5],-0.3,labels=meses[6],cex=0.9,font=2)
text(x[6],0.3,labels=meses[7],cex=0.9,font=2)



for(i in 1:dim(tabela2)[1]){
  text(tabela2$linha[i],y=ord[i],
       labels=paste(strwrap(tabela2$acontecimento[i],width=35),
                    collapse="\n"),pos=1,cex=1.0,font=2)
}

#Salvar em pdf 8 x 14 inches