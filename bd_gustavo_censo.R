
#Projeto BD Gustavo, Teste 7/11/2021

#PASSO 1: criar usu?rio e projeto no BigQuery

# PASSO 2: criar arquivo de credenciais e salvar numa pasta
# https://console.cloud.google.com/apis/credentials/serviceaccountkey?project=<projeto-bd-gustavo>
# service account name: admin
# role: project owner

rm(list = ls())
gc()
library(DBI)
library(bigrquery)
library(ggplot2)
library(basedosdados)
library(dplyr)
library(tidyverse)
library(forcats)

# PASSO 3: apontar a autentica??o para o arquivo json
bq_auth(path = "C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Dados_carto\\teste-2-331421-00b1a3346954.json")

# PASSO 4: criar conex?o com o BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  billing = "teste-2-331421",
  project = "basedosdados"
)


#------------------------------------------------------------------------------#
# Exemplo 1: baixando dados dos setores censit?rios de 2010 para todo o Brasil
#------------------------------------------------------------------------------#

setor_censo <- "SELECT * FROM `basedosdados.br_ibge_censo_demografico.setor_censitario_basico_2010`" #Setores, Censit?rios de 2010 usando SQL na Base

set_censo <- dbGetQuery(con, setor_censo) #rodou

set_censo %>%
  select(sigla_uf, v003) %>% #seleciona o que preciso
  filter(!(v003==0)) %>% #elimina tudo que cont?m 0
  group_by(sigla_uf) %>% #fa?o meu agrupamento
  summarise(mean(v003)) -> v003_censo #salvo em um table.df

mean(v003_censo$`mean(v003)`)  #verifico a media da variavel


#gr?fico
ggplot(v003_censo, aes(y = sigla_uf , x = `mean(v003)`)) + 
  geom_point() +
  xlab("Média do número de moradores em domicílios particulares permanentes por UF") +
  ylab("UF") +
  geom_vline(xintercept=3.52, color="red")




#Fim do código
