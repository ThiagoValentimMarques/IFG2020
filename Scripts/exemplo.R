# Carregando o pacote tidyverse
require(tidyverse)

# Transformando a variável cyl em fator
mtcars <- mtcars %>% 
  mutate(cyl = as.factor(cyl))

# Gráfico de dispersão 2

ggplot(data=mtcars,aes(x=wt,y=mpg))+geom_point()
ggplot(data=mtcars,aes(x=wt,y=mpg,color=cyl))+
  geom_point()

# Gráfico de dispersão mais elaborado
ggplot(data=mtcars,aes(x=wt,y=mpg,color=cyl))+
  geom_point()+theme_update()+
labs(title="Gráfico de dispersão do peso versus consumo",
         caption="Fonte: Motor Trend        Autor: Thiago Valentim",
         x = "Peso (1000 lb)",
         y = "Consumo (mi/gal)",
         colour = "Cilindros")


