############################################################################
############### JOHNS HOPKINS UNIVERSITY ###################################
############################################################################

#-------------------------------- Códigos em R ---------------------------------#

#########################################################
#---------- Pacotes necessários para a análise ---------#
#########################################################

library(tidyverse) #ggplot2, dplyr, ...
library(ggrepel) #Labels em retângulos
library(zoo) #Médias móveis
library(gridExtra) #Gráficos lado a lado no ggplot2

#########################################################
#------------------ URL dos dados ----------------------#
#########################################################

# Casos confirmados
url1 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv" 

# Óbitos 
url2 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"

#########################################################
#-------------- Preparação dos dados -------------------#
#########################################################

casos <- read.csv(url1,header=TRUE)
obitos <- read.csv(url2,header=TRUE)

# Países que desejo fazer a análise
paises <- c("Brazil","Italy","Germany","US","Mexico",'Spain',
            "New Zealand","Korea, South","India","Chile","Peru","Uruguay") 

# Nomemclatura que serão exibidas nas análises
sel <- c("Brasil","Itália","Alemanha","EUA","México","Espanha",
         "Nova Zelândia","Coréia do Sul","Índia","Chile","Peru","Uruguai")

# População dos respectivos países
pop <- c(212521080,60463697,83777677,330954637,128896013,46754380,
         5002100,51268205,1379641143,19111913,32968619,3473709)

# Testes para 1 milhão de pessoas
teste1M <- c(14445,90065,64602,103367,4511,116544,80366,
             25068,6396,58056,50937,19086) #Fonte: worldometers

##############################################################################
################ Início da rotina para os casos ##############################
##############################################################################

casos <- casos %>%
  filter(Country.Region %in% paises)

n<-dim(casos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  valor <- as.vector(apply(casos[casos$Country.Region==i,-c(1,2,3,4)],2,sum))
  if(names(table(valor))[1]=="0"){
    cont<-table(valor)[1]
    valor<-valor[-c(1:cont)]
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    valor[(length(valor)+1):(length(valor)+cont)]<-rep("NA",cont)
    last_point[(length(last_point)+1):(length(last_point)+
                                         cont)]<-rep(NA_character_,cont)
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }else{
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }
  j<-j+1
  matriz[,j]<-valor
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
} 

point<-as.vector(matriz2)
casos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
logcasos <- log10(casos)
propcasos100k <- 100000*casos/rep(pop,each=n)
propdia1m <- 1000000*diario/rep(pop,each=n)
data <- seq(as.Date("2020/02/26"), by = "day", length.out = n)
data <- rep(data,length(paises))
data <- substr(data,6,10)
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
corona <- data.frame(data,dia,País,casos,logcasos,propcasos100k,point,
                     diario,propdia1m)
corona <- as.tibble(corona)

##############################################################################
################# Final da rotina para os casos ##############################
##############################################################################

#########################################################################
######################### Rotina para os óbitos #########################
#########################################################################


obitos <- obitos %>%
  filter(Country.Region %in% paises)

n<-dim(obitos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  valor <- as.vector(apply(obitos[obitos$Country.Region==i,-c(1,2,3,4)],2,sum))
  if(names(table(valor))[1]=="0"){
    cont<-table(valor)[1]
    valor<-valor[-c(1:cont)]
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    valor[(length(valor)+1):(length(valor)+cont)]<-rep("NA",cont)
    last_point[(length(last_point)+1):(length(last_point)+
                                         cont)]<-rep(NA_character_,cont)
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }else{
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }
  j<-j+1
  matriz[,j]<-valor
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
} 

point<-as.vector(matriz2)
obitos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
logobitos <- log10(obitos)
propobt100k <- 100000*obitos/rep(pop,each=n)
propdiaobt1m <- 1000000*diario/rep(pop,each=n)
data <- seq(as.Date("2020/02/26"), by = "day", length.out = n)
data <- rep(data,length(paises))
data <- substr(data,6,10)
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
cor_obt <- data.frame(data,dia,País,obitos,logobitos,propobt100k,point,
                      diario,propdiaobt1m)
cor_obt <- as.tibble(cor_obt)

##############################################################################
################# Final da rotina para os óbitos #############################
##############################################################################

##### Mundo 0 (casos)

#Número sem formato decimal
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

a<-corona%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México",
                     "Índia","Coréia do Sul"))%>%
  ggplot(.,aes(x=dia,y=casos))+ geom_line(size=0.8)+
  # geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Casos registrados")+
  scale_y_continuous(labels = point)+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos de Covid-19",
       caption=" ",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

####### Mundo 0 (óbitos)

#point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

b<-cor_obt%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México",
                     "Índia","Coréia do Sul"))%>%
  ggplot(.,aes(x=dia,y=obitos))+ geom_line(size=0.8)+
  #geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Óbitos registrados")+
  scale_y_continuous(labels = point)+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Acumulado de óbitos por Covid-19",
       caption="Fonte: Johns Hopkins University    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

grid.arrange(a,b,nrow=1)
#salvar em pdf 8 x 12 inches

##### Mundo 1 (casos)

c<-corona%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México",
                     "Índia","Coréia do Sul"))%>%
  ggplot(.,aes(x=dia,y=diario))+ geom_line(size=0.8)+
  # geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Casos novos")+
  scale_y_continuous(labels = point)+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Casos novos de Covid-19",
       caption=" ",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

####### Mundo 1 (óbitos)

d<-cor_obt%>%
  filter(País %in% c("Brasil","Alemanha","Itália","EUA","Nova Zelândia","México",
                     "Índia","Coréia do Sul"))%>%
  ggplot(.,aes(x=dia,y=diario))+ geom_line(size=0.8)+
  #geom_line(aes(y=rollmean(diario, 7, na.pad=TRUE),size=0.8))+ #Média móvel
  facet_wrap(vars(País),scales="free_y",ncol=2)+
  ylab("Óbitos novos")+
  scale_y_continuous(labels = point)+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Óbitos novos por Covid-19",
       caption="Fonte: Johns Hopkins University    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]))

grid.arrange(c,d,nrow=1)
#salvar em pdf 8 x 12 inches

##### Mundo 2 (casos confirmados)

ggplot(corona,aes(x=dia,y=casos,group=País,colour=País))+
  geom_line(size=1.1)+
  ylab("Casos confirmados")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos confirmados de Covid-19",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  scale_y_continuous(labels = point)+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]+10))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")
#salvar em pdf 6 x 8 inches

##### Mundo 3 (casos confirmados - escala log10)

ggplot(corona,aes(x=dia,y=log10(casos),group=País,colour=País))+
  geom_line(size=1.1)+
  ylab("Casos confirmados (escala log10)")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos confirmados de Covid-19",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  scale_y_continuous(breaks = 0:6,
                     labels=c("1","10","100","1000","10000","100000","1000000"))+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]+10))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")
#salvar em pdf 6 x 8 inches

##### Mundo 4 (óbitos confirmados)

ggplot(cor_obt,aes(x=dia,y=obitos,group=País,colour=País))+
  geom_line(size=1.1)+
  ylab("Óbitos confirmados")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Acumulado de óbitos por Covid-19",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  scale_y_continuous(labels = point)+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]-25))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

##### Mundo 5 (óbitos confirmados - escala log10)

ggplot(cor_obt,aes(x=dia,y=log10(obitos),group=País,colour=País))+
  geom_line(size=1.1)+
  ylab("Óbitos confirmados (escala log10)")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Acumulado de óbitos por Covid-19",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  scale_y_continuous(breaks = 0:5,
                     labels=c("1","10","100","1000","10000","100000"))+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]-25))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

##### Mundo 6 (porporção de casos)

ggplot(corona,aes(x=dia,y=propcasos100k,group=País,colour=País))+
  geom_line(size=1.1)+
  ylab("Casos confirmados para cada 100 mil habitantes")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos confirmados por Covid-19 (incidência)",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]+10))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")
#salvar em pdf 6 x 8 inches

##### Mundo 7 (proporção de óbitos)

ggplot(cor_obt,aes(x=dia,y=propobt100k,group=País,colour=País))+
  geom_line(size=1.2)+
  ylab("Óbitos confirmados para cada 100 mil habitantes")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Acumulado de óbitos por Covid-19 (mortalidade)",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]-25))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

############# Testes (mundo 8)

letalidade <- data.frame(sel,teste1M)

ggplot(letalidade, aes(x=reorder(sel,desc(teste1M)), y=teste1M,fill=sel)) + geom_col()+
  ylab("Testes por 1 mihão de pessoas")+xlab("Países")+ 
  labs(title="Fig. 9b: Testes para Covid-19",
       caption="Fonte: Johns Hopkins Univ. e Worldometer   Autor: Thiago Valentim",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))


##### Letalidade

letalidade <- NULL
for(i in sel){
  a<-max(corona[corona$País==i,]$casos,na.rm=TRUE)
  b<- max(cor_obt[cor_obt$País==i,]$obitos,na.rm=TRUE)
  letalidade[i]<-round(100*b/a,2)
}

let <- as.vector(letalidade)
letalidade <- data.frame(let,sel,teste1M)

p1<-ggplot(letalidade, aes(x=reorder(sel,desc(let)), y=let,fill=sel)) + geom_col()+
  ylab("Letalidade (%)")+xlab("Países")+ 
  labs(title="Letalidade da Covid-19",
       caption= " ",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

p2<-ggplot(letalidade, aes(x=reorder(sel,desc(teste1M)), y=teste1M,fill=sel)) +
  geom_col()+
  ylab("Testes por 1 mihão de pessoas")+xlab("Países")+ 
  labs(title="Testes de Covid-19",
       caption="Fonte: Johns Hopkins University e Worldometer   Autor: Thiago Valentim",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(p1, p2, nrow = 1)

#salvar em pdf 5 x 8 inches

