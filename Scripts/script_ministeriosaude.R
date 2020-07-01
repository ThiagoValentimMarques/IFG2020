################################################################################
######################## CENTRO-OESTE ##########################################
################################################################################

library(tidyverse) #Pacote com muitas opções: dplyr, ggplot2, ...
library(brazilmaps) #Shapefiles do Brasil
library(ggrepel) #labels mais elaborados
library(gridExtra) #Inserir mais de uma figura em uma mesma janela
library(lubridate) #Formato mais rápido para datas
library(ggspatial)  #Pacote utilizado na elaboração de mapas
library(scales) #trabalhar com opções de datas dentro do ggplot2
library(sf) #Coordenadas dos pontos
library(rnaturalearthdata) #Mapa do mundo
library(rnaturalearth) #Mapa do mundo
library(zoo) #Médias móveis

################# INFORMAÇÕES DOS DIAS DA SEMANA ##########################
#################     ESTADOS DO CENTRO-OESTE    ##########################

#Caminho para o arquivo .csv do Ministério da Saúde

caminho2 <- "C:\\Users\\Thiago\\Desktop\\Palestra - IFG\\conjuntos de dados\\Ministério da Saúde\\COVIDBR_30jun2020.csv"

#Lendo os dados
dados <- read.csv(caminho2,header=TRUE,sep=";")
dados <- as_tibble(dados) #Transformando tibble (impressão do data.frame mais sofisticado)

#Tamanho da série (número de dias)

n<-dim(dados[dados$estado=="GO" & is.na(dados$codmun),])[1]

### Filtrando os estados da Região Centro-Oeste e alterando o formato das datas

centro_oeste <- dados %>%
  filter(estado %in% c("GO","MT","MS","DF"), is.na(codmun))%>%
  mutate(data = substr(data,1,5))

### Identificando o dia da semana

dia <- factor(rep(c("TER","QUA","QUI","SEX","SAB",
                rep(c("DOM","SEG","TER","QUA","QUI","SEX","SAB"),17),
                "DOM","SEG","TER"),4),
              levels=c("DOM","SEG","TER","QUA","QUI","SEX","SAB"))

### Estados do Centro-Oeste

estados <- rep(c("GO","MT","MS","DF"),each=n)

# Incluindo os dias no data.frame

centro_oeste[,18] <- dia
names(centro_oeste)[18] <- "dia"

# Incluindo as datas no data.frame (Note que 29/06/2020 é a data de hoje)

centro_oeste[,19]<-rep(seq(as.Date("25/02/2020",format="%d/%m/%y"),
             as.Date("30/06/2020",format="%d/%m/%y"),
             by=1),4)
names(centro_oeste)[19] <- "datapt"

### Função para inserir a média em facet_wrap

StatMeanLine <- ggproto("StatMeanLine", Stat,
                        compute_group = function(data, scales) {
                          transform(data, yintercept=mean(y))
                        },
                        required_aes = c("x", "y")
)

stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


hoje <- "30/06" #Informar a data de hoje dd/mm

## Incluindo os labels para um formato melhor esteticamente (usar no pacote ggrepel)

centro_oeste <- centro_oeste %>%   
  mutate(label = if_else(data == hoje,
                         as.character(estado), NA_character_),
         propcasos = (casosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000,
         propobitos = (obitosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000)


##################################
######### FIGURAS ################
##################################

#### Brasil

### Brasil 1 (a - acumulado de casos)
#Números sem formato decimal
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

a<-dados %>%
  filter(regiao=="Brasil")%>%
  mutate(datapt = seq(as.Date("25/02/2020",format="%d/%m/%y"),
                          as.Date("30/06/2020",format="%d/%m/%y"),
                          by=1))%>%
  ggplot(.,aes(x=datapt,y=casosAcumulado))+geom_point(colour="blue",cex=0.9)+
  scale_x_date(date_breaks = "1 month",date_labels = "%d/%b")+
  scale_y_continuous(labels = point)+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Acumulado de casos",
       caption=" ") +
    theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 10)))

### Brasil 1 (b - casos diários)

b<-dados %>%
  filter(regiao=="Brasil")%>%
  mutate(datapt = seq(as.Date("25/02/2020",format="%d/%m/%y"),
                      as.Date("30/06/2020",format="%d/%m/%y"),
                      by=1),
         mediam = rollmean(casosNovos, 7, na.pad=TRUE))%>%
  ggplot(.,aes(x=datapt,y=casosNovos))+
  geom_line(cex=1.04)+
  geom_line(aes(y=mediam,colour="Média Móvel (7 dias)"),cex=1.04)+
  scale_x_date(date_breaks = "1 month",date_labels = "%d/%b")+
  scale_y_continuous(labels = point)+
  labs(x = "Data", y = "Casos confirmados", colour = " ",
       title="Casos novos",
       subtitle="A linha azul é a média móvel",
       caption="",
       fill=" ")+
  scale_color_manual(values=c("blue"))+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 10)))

### Brasil 1 (c - óbitos acumulados)

c<-dados %>%
  filter(regiao=="Brasil")%>%
  mutate(datapt = seq(as.Date("25/02/2020",format="%d/%m/%y"),
                      as.Date("30/06/2020",format="%d/%m/%y"),
                      by=1))%>%
  ggplot(.,aes(x=datapt,y=obitosAcumulado))+geom_point(colour="red",cex=0.9)+
  scale_x_date(date_breaks = "1 month",date_labels = "%d/%b")+
  scale_y_continuous(labels = point)+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Acumulado de óbitos",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 10)))

### Brasil 1 (d - óbitos diários)

d<-dados %>%
  filter(regiao=="Brasil")%>%
  mutate(datapt = seq(as.Date("25/02/2020",format="%d/%m/%y"),
                      as.Date("30/06/2020",format="%d/%m/%y"),
                      by=1),
         mediam = rollmean(obitosNovos, 7, na.pad=TRUE))%>%
  ggplot(.,aes(x=datapt,y=obitosNovos))+
  geom_line(cex=1.04)+
  geom_line(aes(y=mediam,colour="Média Móvel (7 dias)"),cex=1.04,col="red")+
  scale_x_date(date_breaks = "1 month",date_labels = "%d/%b")+
  scale_y_continuous(labels = point)+
  labs(x = "Data", y = "Óbitos confirmados", colour = " ",
       title="Óbitos novos",
       subtitle="A linha vermelha é a média móvel",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim",
       fill=" ")+
  scale_color_manual(values=c("blue"))+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 5)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(a, b,c,d, nrow = 2)

#### Brasil (Boxplot por dia da semana)

dados %>%
  filter(regiao=="Brasil")%>%
  
  mutate(dia_semana = factor(c("TER","QUA","QUI","SEX","SAB",
                               rep(c("DOM","SEG","TER","QUA","QUI","SEX","SAB"),17),
                               "DOM","SEG","TER"),
                             levels=c("DOM","SEG","TER","QUA","QUI","SEX","SAB")),
         datapt = seq(as.Date("25/02/2020",format="%d/%m/%y"),
                             as.Date("30/06/2020",format="%d/%m/%y"),
                             by=1))%>%
  filter(datapt > "2020-04-30")%>%
 ggplot(.,aes(x=dia_semana,y=casosNovos,fill=dia_semana))+geom_boxplot(fill="gray")+
  labs(x = "Dia da semana (a partir de 30/04/2020)",
       y = "Óbitos confirmados", colour = " ",
       title="Boxplots dos casos novos consideando o dia da semana",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim",
       fill=" ")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

#### Brasil (Boxplot por dia da semana)

dados %>%
  filter(regiao=="Brasil")%>%
  
  mutate(dia_semana = factor(c("TER","QUA","QUI","SEX","SAB",
                               rep(c("DOM","SEG","TER","QUA","QUI","SEX","SAB"),17),
                               "DOM","SEG","TER"),
                             levels=c("DOM","SEG","TER","QUA","QUI","SEX","SAB")),
         datapt = seq(as.Date("25/02/2020",format="%d/%m/%y"),
                      as.Date("30/06/2020",format="%d/%m/%y"),
                      by=1))%>%
  filter(datapt > "2020-04-30")%>%
  ggplot(.,aes(x=dia_semana,y=obitosNovos,fill=dia_semana))+geom_boxplot(fill="gray")+
  labs(x = "Dia da semana (a partir de 30/04/2020)",
       y = "Óbitos confirmados", colour = " ",
       title="Boxplots dos óbitos novos consideando o dia da semana",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim",
       fill=" ")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))


#### Centro-Oeste 1 (casos)
centro_oeste %>%
  filter(datapt > "2020-03-30")%>%
ggplot(.,aes(x=datapt,y=casosAcumulado,group=estado,
              colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Acumulados de casos de Covid-19 por estado",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

#### Centro-Oeste 2 (óbitos)
centro_oeste %>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=obitosAcumulado,group=estado,
               colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados", colour = "Estado",
       title="Acumulados de óbitos por Covid-19",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

#### Centro-Oeste 3 (casos proporcional)
centro_oeste %>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=propcasos,group=estado,
               colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados por 100 mil habitantes", colour = "Estado",
       title="Acumulados de casos de Covid-19 por estado (incidência)",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")

#salvar em pdf 6 x 8 inches

#### Centro-Oeste 4 (óbitos proporcional)
centro_oeste %>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=propobitos,group=estado,
               colour=estado))+geom_line()+
  geom_line(cex=1.1) +
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados por 100 mil habitantes", colour = "Estado",
       title="Acumulados de óbitos por Covid-19 (mortalidade)",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.5,segment.colour = "transparent")
#salvar em pdf 6 x 8 inches

#### Centro-Oeste 9 (testagem e letalidade)

p1<-centro_oeste %>%
  filter(datapt == "2020-06-30")%>%
  mutate(letalidade = (obitosAcumulado/casosAcumulado)*100,
         testes1mi = (c(39409,17974,48945,25126)/c(2778986,3484466,
                                                   7018354,3015268))*1E6)%>%
  ggplot(., aes(x=reorder(estado,desc(testes1mi)), y=testes1mi,fill=estado)) + geom_col()+
  ylab("Testes por 1 milhão de pessoas")+xlab("Estados da Região Centro-Oeste")+ 
  labs(title="Testes para Covid-19",
       caption="Fonte: covid19br.wcota.me       Autor: Thiago Valentim",
       fill="estado")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

p2<-centro_oeste %>%
  filter(datapt == "2020-06-30")%>%
  mutate(letalidade = (obitosAcumulado/casosAcumulado)*100,
         testes1mi = (c(39409,17974,48945,25126)/c(2778986,3484466,
                                                   7018354,3015268))*1E6)%>%
  ggplot(., aes(x=reorder(estado,desc(letalidade)), y=letalidade,fill=estado)) + geom_col()+
  ylab("Letalidade (%)")+xlab("Estados da Região Centro-Oeste")+ 
  labs(title="Letalidade da Covid-19",
       caption="Fonte: Ministério da Saúde       Autor: Thiago Valentim",
       fill="estado")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(p1, p2, nrow = 1)
#salvar em pdf 3 x 7 inches

#### Centro-Oeste 5 (a - casos em Goiás)

p3<-centro_oeste %>%
  filter(estado == "GO")%>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=casosAcumulado,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="blue")+
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Acumulado de casos em Goiás",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

#### Centro-Oeste 5 (b - óbitos em Goiás)
p4<-centro_oeste %>%
  filter(estado == "GO")%>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=obitosAcumulado,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="red")+
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados", colour = "Estado",
       title="Acumulado de óbitos em Goiás",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

#### Centro-Oeste 5 (c- Casos diários em Goiás)
p5<-centro_oeste %>%
  filter(estado == "GO")%>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=casosNovos,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="blue")+
   scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Casos confirmados", colour = "Estado",
       title="Casos diários em Goiás",
       caption=" ") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

#### Centro-Oeste 5 (d- Óbitos diários em Goiás)
p6<-centro_oeste %>%
  filter(estado == "GO")%>%
  filter(datapt > "2020-03-30")%>%
  ggplot(.,aes(x=datapt,y=obitosNovos,group=estado,
               colour=estado))+geom_line(cex=1.1,colour="red")+
  scale_x_date(date_breaks = "1 week",date_labels = "%d/%b")+
  labs(x = "Data", y = "Óbitos confirmados", colour = "Estado",
       title="Óbitos diários em Goiás",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(p3, p4,p5,p6, nrow = 2)
#salvar em pdf 6 x 8 inches

#### Centro-Oeste 6 (casos em Goiás)

centro_oeste %>%
  filter(estado == "GO",semanaEpi>18) %>%
  ggplot(.,aes(x=dia,y=casosNovos,group = regiao, colour = regiao))+
  geom_point(colour="blue")+stat_mean_line(color="black",lty=2)+
  geom_line(size=1.1,colour="blue")+facet_wrap(~semanaEpi)+
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  labs(x = "Dia da semana", y = "Casos diários", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="Casos diários por Covid-19 distribuídos por semana epidemiológica", 
       subtitle="A linha tracejada representa a média diária")

#salvar em pdf 6 x 8 inches

#### Centro-Oeste 7 (óbitos em Goiás)

centro_oeste %>%
  filter(estado == "GO",semanaEpi>18) %>%
  ggplot(.,aes(x=dia,y=obitosNovos,group = regiao, colour = regiao))+
  geom_point(colour="red")+stat_mean_line(color="black",lty=2)+
  geom_line(size=1.1,colour="red")+facet_wrap(~semanaEpi)+
  theme(legend.position="bottom", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  labs(x = "Dia da semana", y = "Óbitos diários", colour = "",
       caption="Fonte: Ministério da Saúde    Autor: Thiago Valentim",
       title="Óbitos diários por Covid-19 distribuídos por semana epidemiológica", 
       subtitle="A linha tracejada representa a média diária")
#salvar em pdf 6 x 8 inches

########################################################################
###################### MAPAS PARA O BRASIL #############################
########################################################################

#### Mapa brasil (casos)

world <- ne_countries(scale = "medium", returnclass = "sf")

nacional <- dados %>%
  filter(is.na(codmun)== FALSE,is.na(codRegiaoSaude)==FALSE)%>%
  filter(data=="30/06/2020")

mapa <- get_brmap("City")
names(nacional)[5]<-"City"

mapa<- mapa %>%
  mutate(City = as.numeric(substr(as.character(mapa$City),1,6)))

geral2 <- merge(x = mapa, y = nacional, by = "City", all.x=TRUE)

geral2$casosAcumulado[geral2$casosAcumulado==0]<-NA

mapa1<-ggplot(geral2) + geom_sf(data=world)+geom_sf(aes(fill = casosAcumulado),color = NA)+
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.5) +
  scale_fill_gradient(low = "rosybrown2",high = "red", guide = "colorbar",
                      na.value = "white")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "lightblue"),
        legend.position = "bottom",
        legend.text = element_text(size=9),
        legend.key.height = unit(0.4,'cm'),
        legend.key.width = unit(1.3,'cm'))+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(fill = " ",                                
       title="Casos com Covid-19",
       subtitle ="Brasil (30/06/2020)", 
       caption="Fonte: Ministério da Saúde         Autor: Thiago Valentim")+
  coord_sf(xlim = c(-75, -30), ylim = c(-33, 3))

#### Mapa brasil (óbitos)

geral2$obitosAcumulado[geral2$obitosAcumulado==0]<-NA

mapa2<-ggplot(geral2) + geom_sf(data=world)+geom_sf(aes(fill = obitosAcumulado),color = NA)+
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.5) +
  scale_fill_gradient(low = "rosybrown2",high = "red", guide = "colorbar",
                      na.value = "white")+
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_rect(fill = "lightblue"),
        legend.position = "bottom",
        legend.text = element_text(size=9),
        legend.key.height = unit(0.4,'cm'),
        legend.key.width = unit(1.3,'cm'))+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(fill = " ",                                
       title="Óbitos por Covid-19",
       subtitle ="Brasil (30/06/2020)", 
       caption="Fonte: Ministério da Saúde         Autor: Thiago Valentim")+
  coord_sf(xlim = c(-75, -30), ylim = c(-33, 3))

grid.arrange(mapa1, mapa2,nrow = 1)

#Salvar com 7 x 10 inches

########################################################################
############# MAPAS PARA O ESTADO DE GOIÁS #############################
########################################################################

#### Mapa 1 (casos)

GO_mapa <- dados %>%
  filter(estado == "GO",codmun!=520000)%>%
  mutate(data = substr(data,1,5))%>%
  filter(data=="30/06") #Substituir por 01/04, 01/05, 01/06 e 30/06

GO_mapa <- GO_mapa[-1,]

mapa <- get_brmap("City",geo.filter = list(State = 52))
mapa2 <- get_brmap("Region")


names(GO_mapa)[5]<-"City"

mapa<- mapa %>%
  mutate(City = as.numeric(substr(as.character(mapa$City),1,6)))

geral2 <- merge(x = mapa, y = GO_mapa, by = "City", all.x=TRUE)

geral2$casosAcumulado[is.na(geral2$casosAcumulado)]<-0

#(length(geral2$casosAcumulado[geral2$casosAcumulado>0])/length(geral2$nome))*100
#max(geral2$casosAcumulado[geral2$casosAcumulado>0])

### Mapa2 (óbitos)

geral2 <- geral2%>%
  mutate(categ = cut(casosAcumulado, c(-1,0,10, 100, 500, 2000,6931)))%>%
  mutate(inc = (casosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000)

  ggplot(geral2) + geom_sf(data=mapa2,fill = "gray70")+
  geom_sf(aes(fill =categ,text=nome),size=0.1)+
  theme(panel.background = 
          element_rect(fill = "lightblue"),
        panel.grid = element_line(colour = "transparent"),
        legend.position = "right",
        legend.text = element_text(size=9))+
 # scale_fill_gradient(low="lightyellow", high="red3",
    #                  na.value = "white")+
    scale_fill_manual(values = c("white","lightyellow","rosybrown2","tomato1","red",
                                 "red4"),
                      labels=c("0","1 a 10","11 a 100","101 a 500","501 a 2000",
                               "2001 a 6931"))+
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  labs(fill = "",                                
       title="Casos confirmados",
       subtitle ="30/06/2020", 
       caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
    coord_sf(xlim = c(-54, -46), ylim = c(-19.5, -12.5))
  
#Salvar em pdf 5 x 6 inches (data mais recente)

########## Mapa 2 (óbitos)
  
  GO_mapa <- dados %>%
    filter(estado == "GO",codmun!=520000)%>%
    mutate(data = substr(data,1,5))%>%
    filter(data=="30/06")  #Substituir por 01/04, 01/05, 01/06 e 25/06
  
  GO_mapa <- GO_mapa[-1,]
  
  mapa <- get_brmap("City",geo.filter = list(State = 52))
  mapa2 <- get_brmap("Region")
  
  names(GO_mapa)[5]<-"City"
  
  mapa<- mapa %>%
    mutate(City = as.numeric(substr(as.character(mapa$City),1,6)))
  
  geral3 <- merge(x = mapa, y = GO_mapa, by = "City", all.x=TRUE)
  
  geral3$obitosAcumulado[is.na(geral3$obitosAcumulado)]<-0

#(length(geral3$obitosAcumulado[geral3$obitosAcumulado>0])/length(geral3$nome))*100

  geral3 <- geral3%>%
    mutate(categ = cut(obitosAcumulado, c(-1,0,1, 10, 20, 50,328)))%>%
    mutate(mort = (obitosAcumulado/as.numeric(as.character(populacaoTCU2019)))*100000)
    
  ggplot(geral3) + geom_sf(data=mapa2,fill = "gray70")+
    geom_sf(aes(fill =categ,text=nome),size=0.1)+
    theme(panel.background = 
            element_rect(fill = "lightblue"),
          panel.grid = element_line(colour = "transparent"),
          legend.position = "right",
          legend.text = element_text(size=9))+
    # scale_fill_gradient(low="lightyellow", high="red3",
    #                  na.value = "white")+
    scale_fill_manual(values = c("white","lightyellow","rosybrown2","tomato1","red",
                                 "red4"),
                      labels=c("0","1","2 a 10","11 a 20","21 a 50",
                               "51 a 162"))+
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)+
    labs(fill = "",                                
         title="Óbitos por Covid-19",
         subtitle ="30/06/2020", 
         caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
      coord_sf(xlim = c(-54, -46), ylim = c(-19.5, -12.5))
  
  #Salvar 5 x 6 inches
  
########## Mapa 3 (incidência e mortalidade)
  
### a - Incidência
    
  m<-ggplot(geral2) + geom_sf(data=mapa2,fill = "gray70")+
      geom_sf(aes(fill =inc,text=nome),size=0.1)+
      theme(panel.background = 
              element_rect(fill = "lightblue"),
            panel.grid = element_line(colour = "transparent"),
            legend.position = "bottom",
            legend.text = element_text(size=9),
            legend.key.height = unit(0.4,'cm'),
            legend.key.width = unit(0.8,'cm'))+
       scale_fill_gradient(low="lightyellow", high="red3",
                        na.value = "white")+
      annotation_scale(location = "br", width_hint = 0.3) +
      annotation_north_arrow(location = "br", which_north = "true", 
                             pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering)+
      labs(fill = "Taxa por 100k hab.",                                
           title="Incidência",
           subtitle ="30/06/2020", 
           caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
    coord_sf(xlim = c(-54, -46), ylim = c(-19.5, -12.5))
  
### b- mortalidade
  
  n<-ggplot(geral3) + geom_sf(data=mapa2,fill = "gray70")+
    geom_sf(aes(fill =mort,text=nome),size=0.1)+
    theme(panel.background = 
            element_rect(fill = "lightblue"),
          panel.grid = element_line(colour = "transparent"),
          legend.position = "bottom",
          legend.text = element_text(size=9),
          legend.key.height = unit(0.4,'cm'),
          legend.key.width = unit(0.8,'cm'))+
    scale_fill_gradient(low="white", high="red3",
                        na.value = "white")+
    annotation_scale(location = "br", width_hint = 0.3) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.08, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering)+
    labs(fill = "Taxa por 100k hab.",                                
         title="Mortalidade",
         subtitle ="30/06/2020", 
         caption="Fonte: Ministério da Sáude         Autor: Thiago Valentim")+
    coord_sf(xlim = c(-54, -46), ylim = c(-19.5, -12.5))

grid.arrange(m, n,nrow = 1)

#Salvar 6 x 10 inches
