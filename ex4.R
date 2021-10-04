rm( list = ls( ) ) 
graphics.off()
getwd()
setwd('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr')
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(readxl)
dir.create('temp_maio')
dir.create('temp_novembro')

#baixei os dicion?rios antes para manipula??o
#download e extra??o dos dados via R
#Download
download.file(url='https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Dados/PNAD_COVID_052020.zip', 
              destfile='temp_mai.zip') #mes de maio

download.file(url='https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_PNAD_COVID19/Microdados/Dados/PNAD_COVID_112020.zip', 
              destfile='temp_nov.zip') #mes de novembro

#extracao
unzip( zipfile = 'temp.zip', #mes de maio
       exdir = 'C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr\\temp_mai')

unzip( zipfile = 'temp_nov.zip', #mes de novembro
       exdir = 'C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr\\temp_nov')

#lendo as bases
maio <- fread('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr\\temp_mai\\PNAD_COVID_052020.csv')
nov <- fread('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr\\temp_nov\\PNAD_COVID_112020.csv')
