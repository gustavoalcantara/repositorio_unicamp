#Gustavo Casteletti de Alcântara, 210524
rm( list = ls( ) ) 
graphics.off()
getwd()
setwd('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\exercicio5')
library(data.table)
library(dplyr)
library(survey) 
library(srvyr) 
library(ggplot2)
maio <- fread('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr\\temp_mai\\PNAD_COVID_052020.csv')
nov <- fread('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\aula_dplyr\\temp_nov\\PNAD_COVID_112020.csv')

#Vou selecionar as vari?veis que preciso e depois dar um Rbind
maio %>% 
  select(UF, Estrato, UPA, V1032, A003, B0013, B0014, B0031, A005, B0011, B0012, V1013) ->mai

nov %>% 
  select(UF, Estrato, UPA, V1032, A003, B0013, B0014, B0031, A005, B0011, B0012, V1013) ->novembro

rbind(mai, novembro)->covid_geral

pnad_pesos <- covid_geral %>% 
  as_survey_design(ids=UPA, strata=Estrato, weights=V1032, nest=TRUE) #atribui??o dos pesos

#exercicio1
#Quem apresentou mais sintomas do Covid-19 (dificuldades de respirar e dor de
 #garganta), homens ou mulheres? Antes de responder, trate os dados (informações faltantes, não sabe etc.).

pnad_pesos %>% 
  rename(dor_gar=B0013,
         dif_resp=B0014,
         quarentena=B0031,
         ensino=A005,
         febre=B0011,
         tosse=B0012,
         mes=V1013) %>% 
  mutate(one = 1, Sexo = ifelse(A003 == 1, "Homem", "Mulher")) %>% 
  mutate(garganta=case_when(dor_gar==1~ 'sim',
                            dor_gar==2~'nao',
                            dor_gar==3~'ignorado',
                            dor_gar==9~'ignorado')) %>% 
  mutate(respirar=case_when(dif_resp==1~'sim',
                            dif_resp==2~'nao',
                            dif_resp==3~'ignorado',
                            dif_resp==9~'ignorado')) %>% 
  group_by(Sexo)%>% #agrupamento por sexo
  summarise(
    sintomas=survey_total(dor_gar==1 & dif_resp==1, na.rm = TRUE), #qtde de pessoas, por sexo, atribuindo peso. 
    nao_sintomas=survey_total(dor_gar>=2 & dif_resp>=2, na.rm = TRUE)) %>% #qtde de pessoas, por sexo, nao infectadas. 
  mutate(prop_sint=(sintomas/nao_sintomas)*1000) %>% #propor??o de infectados por mil pessoas
  drop_na()
#fiz a propor??o multiplicando por 1000. Portanto, atribuindo os pesos, dos homens e mulheres que apresentaram os dois sintomas, as mulheres apresentaram maior n?mero
#de infectadas. 

#Exercicio 2
#Quem tomou mais providências contra doença (ficar em casa em isolamento), homens ou mulheres?
pnad_exerc2 <- pnad_pesos %>% 
  mutate(sex_cat=if_else(A003==1, 'homem', "mulher")) %>% 
  mutate(repouso=case_when(B0031==1~ 'sim',
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
  group_by(sex_cat) %>% 
  summarise(
    quarentena=survey_total(B0031==1 & B0013==1 & B0014==1, na.rm=TRUE), #quem teve sintoma e fez quarentena
    nao_quarentena_sin=survey_total(B0031>=2 & B0013==1 & B0014==1, na.rm = TRUE), #quem teve sintoma e nao fez quarentena
    nao_quarentena_naosin=survey_total(B0031>=2 & B0013>=2 & B0014>=2, na.rm=TRUE)) %>% #quem nao teve sintoma e nao fez quarentena
  mutate(nao_quarentena_geral=(nao_quarentena_sin+nao_quarentena_naosin)) %>% #somando as duas 'nao quarentenas'
  mutate(proporcao_quarentena=(quarentena/nao_quarentena_geral)*100) %>% #proporcao de quem apresentou sintoma e fez quarentena
  mutate(proporcao_naoquarentena=(100-proporcao_quarentena)) #proporcao de quem nao fez quarentena em geral
drop_na()
# ? evidente que quem realizou mais quaretena e apresentou os dois sintomas foram as mulheres. 

#########################
#Exercicio 3
#Construa uma nova variável que incorpore as informações sobre os sintomas de febre e tosse, e depois verifique sua relação com o nível educacional dos respondentes.


pnad_pesos %>% 
  mutate(feb_tos=(B0011+B0012)/2) %>% #criando a variavel
  mutate(sexo=if_else(A003==1, "homem", "mulher")) %>% 
  mutate(escolaridade=case_when(A005==1~'sem instrucao', #nivel de instrucao
                                A005==2~'fund incompleto',
                                A005==3~'fund completo',
                                A005==4~'med incompleto',
                                A005==5~'med completo',
                                A005==6~'sup incompleto',
                                A005==7~'sup completo',
                                A005==8~'pos grad')) %>% 
  mutate(sintomas=case_when(feb_tos==1~'sim', #apresentou os dois sintomas em conjunto?
                            feb_tos==2~'nao',
                            feb_tos==3~'ignorado',
                            feb_tos==9~'ignorado')) %>% 
  group_by(escolaridade) %>% #agrupamento por escolaridade
  summarise(
    sintomas=survey_total(feb_tos==1, na.rm = TRUE), #apresentou os dois sintomas
    nao_sintomas=survey_total(feb_tos==2)) %>%  #nao apresentou os dois sintomas
  mutate(prop_sintomas=(sintomas/nao_sintomas)*100) %>%  #proporcao
  drop_na()


############
#exercicio 4
# Construa um gráfico que mostre a mudança no tempo (Maio e Novembro no eixo x)
dos percentuais de pessoas que sentiram ou não dificuldades de respirar.
pnad_exerc4 <- pnad_pesos %>% 
  mutate(mes=if_else(V1013==5,'maio', 'novembro')) %>% 
  mutate(dif_resp=case_when(B0014==1~'sim',
                            B0014==2~'nao',
                            B0014==3~'ignorado',
                            B0014==9~'ignorado')) %>% 
  group_by(mes) %>% 
  summarise(
    respirar_dif=survey_total(dif_resp=='sim'), #selecionei quem apresentou sintomas de dificuldade de respirar nos meses
    resp_nao_dif_nao=survey_total((dif_resp=='nao')), #quem nao apresentou na categoria nao
    resp_nao_dif_ign=survey_total(dif_resp=='ignorado')) %>% #quem nao apresentou na categoria ignorado
  mutate(nao_dif_resp=(resp_nao_dif_nao+resp_nao_dif_ign)) %>% #soma das duas variaveis 'nao' para calcular a proporcao
  mutate(proporcao_difresp=(respirar_dif/nao_dif_resp)*100) %>%
  mutate(proporcao_semdifresp=100-proporcao_difresp)#calculo da proporcao
drop_na()

as.data.frame(pnad_exerc4)->pnad_exerc4df

ggplot(pnad_exerc4df, aes(x = mes, group=1)) +
  geom_line(aes(y=proporcao_semdifresp, color=proporcao_difresp)) +  
  theme_dark() + ggtitle("Declara??o de dificuldade de respirar entre maio e novembro \n (2020)") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "M?s", y = "Percentual", color = "Teve dificuldade \n de respirar (em proporc?o)?") +
  scale_fill_manual(values=c("#D6ED17FF", "#606060FF", "#95DBE5FF"))



