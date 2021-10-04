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

#Questão 1 
#Quem apresentou mais sintomas do Covid-19 (dificuldades de respirar e dor de
#garganta), homens ou mulheres? Antes de responder, trate os dados (informações faltantes, não sabe etc.).

#selecionar as variaveis que vou trabalhar
maio %>%
  rename(sexo = A003,
         dor_gar=B0013,
         dif_resp=B0014)%>%
  mutate(sex_cat=if_else(sexo==1, 'homem', "mulher")) %>% 
  select(sex_cat, dor_gar, dif_resp) %>%  
  mutate(garganta=case_when(dor_gar==1~ 'sim',
                            dor_gar==2~'nao',
                            dor_gar==3~'ignorado',
                            dor_gar==9~'ignorado')) %>% 
  mutate(respirar=case_when(dif_resp==1~'sim',
                            dif_resp==2~'nao',
                            dif_resp==3~'ignorado',
                            dif_resp==9~'ignorado'))->maio_exerc1

filter(maio_exerc1, garganta=='sim', respirar=='sim') -> maio_exerc1
ftable(maio_exerc1$sex_cat~maio_exerc1$garganta) #quem mais apresentou os dois sintomas em conjunto.

#Questão 1 - Mês de Novembro
nov %>% #as variaveis s?o iguais
  rename(sexo = A003,
         dor_gar=B0013,
         dif_resp=B0014)%>%
  mutate(sex_cat=if_else(sexo==1, 'homem', "mulher")) %>% 
  select(sex_cat, dor_gar, dif_resp) %>%  
  mutate(garganta=case_when(dor_gar==1~ 'sim',
                            dor_gar==2~'nao',
                            dor_gar==3~'ignorado',
                            dor_gar==9~'ignorado')) %>% 
  mutate(respirar=case_when(dif_resp==1~'sim',
                            dif_resp==2~'nao',
                            dif_resp==3~'ignorado',
                            dif_resp==9~'ignorado'))->nov_exerc1

filter(nov_exerc1, garganta=='sim', respirar=='sim') -> nov_exerc1
ftable(nov_exerc1$sex_cat~nov_exerc1$garganta) #quem mais apresentou em novembro?

#Questão 2
#Quem tomou mais providências contra doença (ficar em casa em isolamento), homens
#ou mulheres?
ftable(maio$A003~maio$B0031)
maio %>%
  filter(!is.na(B0031)) %>% 
  select(B0031, A003, B0013, B0014) %>% 
  mutate(sex_cat=if_else(A003==1, 'homem', "mulher")) %>% 
  mutate(quarentena=case_when(B0031==1~ 'sim',
                              B0031==2~'nao',
                              B0031==9~'ignorado')) %>% 
  mutate(dor_garganta=case_when(B0013==1~'sim',
                                B0013==2~'nao',
                                B0013==3~'ignorado',
                                B0013==9~'ignorado')) %>% 
  mutate(respirar=case_when(B0014==1~'sim',
                            B0014==2~'nao',
                            B0014==3~'ignorado',
                            B0014==4~'ignorado')) %>% 
  select(sex_cat, quarentena, dor_garganta, respirar)->maio_exerc2

filter(maio_exerc2, dor_garganta=='sim', respirar=='sim', quarentena=='sim') ->maio_exerc2

ftable(maio_exerc2$sex_cat~maio_exerc2$quarentena)

ggplot(data=maio_exerc2)+
  geom_bar((aes(x=as.factor(sex_cat))))

#Mes de novembro
nov %>%
  filter(!is.na(B0031)) %>% 
  select(B0031, A003, B0013, B0014) %>% 
  mutate(sex_cat=if_else(A003==1, 'homem', "mulher")) %>% 
  mutate(quarentena=case_when(B0031==1~ 'sim',
                              B0031==2~'nao',
                              B0031==9~'ignorado')) %>% 
  mutate(dor_garganta=case_when(B0013==1~'sim',
                                B0013==2~'nao',
                                B0013==3~'ignorado',
                                B0013==9~'ignorado')) %>% 
  mutate(respirar=case_when(B0014==1~'sim',
                            B0014==2~'nao',
                            B0014==3~'ignorado',
                            B0014==4~'ignorado')) %>% 
  select(sex_cat, quarentena, dor_garganta, respirar)->nov_exerc2

ftable(nov_exerc2$sex_cat~nov_exerc2$quarentena)
ggplot(data=nov_exerc2)+
  geom_bar((aes(x=as.factor(sex_cat))))


