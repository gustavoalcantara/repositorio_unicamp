#Passo 0 - Bibliotecas e Diret?rio
rm( list = ls( ) ) 
graphics.off()
gc(reset = TRUE)
getwd()
setwd('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Dados_carto')
library(spatial)
library(geobr)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)

#Passo 1 - Download dos arquivos 
data <- fread('casos.csv', header=T, sep= ';', encoding = "UTF-8") #Do Seade. J? no Meu Desktop

#download, via geobr, da base de SP
mun <- read_municipality(code_muni=35, year=2010) #baixar shapefile do estado S?o Paulo

mun <- rename(mun, codigo_ibge = 'code_muni') #alterar nome da vari?vel codigo_muni

#Passo 2 - Verificar o nome das variaveis que aparecem na base do SEADE e no meu shapefile
names(data) #verifico o nome das minhas vari?veis
names(mun)

#Passo 3  O Exercicio foi de selecionar uma doen?a, no meu caso a cardiopatia, e integra-la, a partir da limpeza dos dados, com o shapefile do municipio de SP.  
data %>% 
  select(nome_munic, codigo_ibge, idade, cs_sexo, diagnostico_covid19, obito, cardiopatia, data_inicio_sintomas) %>% #selecionar para cardiopatia
  filter(!(cardiopatia=='IGNORADO' | cs_sexo=='IGNORADO')) %>% #Filtrar sem o Ignorado a Cardipatia e o Sexo. Foi para 449141
  filter(obito==1, cardiopatia=='SIM', diagnostico_covid19=='CONFIRMADO') %>% #Selecionando quem veio a ?bito por Covid e apresenta cardiopatia. Agora para 65206
  group_by(codigo_ibge) %>%
  count(obito) -> cardiopatia

#an?lise da idade em um histograma
data%>% 
  select(nome_munic, codigo_ibge, idade, cs_sexo, diagnostico_covid19, obito, cardiopatia, data_inicio_sintomas) %>% #selecionar para cardiopatia
  filter(!(cardiopatia=='IGNORADO' | cs_sexo=='IGNORADO')) %>% #Filtrar sem o Ignorado a Cardipatia e o Sexo. Foi para 449141
  filter(obito==1, cardiopatia=='SIM', diagnostico_covid19=='CONFIRMADO') -> idade_cardio

hist(idade_cardio$idade)

#Passo 4 - Realizar o Merge ('join do qgis') via Dplyr
join <- dplyr::left_join(mun, cardiopatia, by='codigo_ibge')

#Passo 5 - Elabora??o de Gr?ficos simples, fill= n ('?bitos de cardiopatia')
ggplot()+
  geom_sf(data = join, aes(fill=n))+
  scale_x_log10()+
  scale_fill_distiller(palette = "Oranges", name="?bitos") +
  ggtitle("?bitos por Covid-19 com cardiopatia existente no per?odo de 2020 a 2021")
theme_minimal()


#Fim - 22/12