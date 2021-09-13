#Nome; Gustavo Casteletti de Alc?ntara; RA: 210524; Mestrando em Demografia pelo IFCH
#preparando meu ambiente
rm( list = ls( ) ) 
graphics.off()
setwd('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\exercicio3')
dat <- read.table('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\exercicio3\\Emprego.txt', sep='\t', header=T)
library(ggplot2)
library(dplyr)
library(data.table)

#Questão 1 
#Faça um gráfico e indique, das categorias de trabalho existentes na base, qual é
#dominada por homens e qual é mais ocupada por mulheres.

ftable(dat$gender~dat$jobcat) 

ggplot(data = dat) + 
  geom_bar(aes(x = jobcat, fill=gender), position="fill")+
  scale_y_continuous(labels = scales::percent) + theme_light() +
  labs(y="Porcentagem",x="Ensino")+
  scale_fill_discrete(name="Genero", labels=c("Feminino", "Masculino" ))

  