#Nome; Gustavo Casteletti de Alc?ntara; RA: 210524; Mestrando em Demografia pelo IFCH
#preparando meu ambiente
rm( list = ls( ) ) 
graphics.off()
setwd('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\exercicio3')
dat <- read.table('C:\\Users\\User\\Desktop\\Gustavo\\Unicamp\\Mestrado\\1sem2021\\dm026\\exercicio3\\Emprego.txt', sep='\t', header=T)
library(ggplot2)
library(dplyr)
library(data.table)

#Descrição das variáveis
#id – Código do empregado
#gender - gênero
#bdate – data de nascimento
#educ – nível educacional em anos
#jobcat – Categoria de emprego: 1) Secretário(a); 2) Serviços Gerais (limpeza) e 3) Gerente
#salary – salário corrente
#salbegin – salário inicial
#jobtime – meses desde a contratação
#prevexp – experiências previas em meses
#minority – pertence a uma minoria 0) Não e 1) sim

#Exercicio 1
#Faça um gráfico e indique, das categorias de trabalho existentes na base, qual é
#dominada por homens e qual é mais ocupada por mulheres.
ftable(dat$gender~dat$jobcat) 

ggplot(data = dat) + 
  geom_bar(aes(x = jobcat, fill=gender), position="fill")+
  scale_y_continuous(labels = scales::percent) + theme_light() +
  labs(y="Porcentagem",x="Ensino")+
  scale_fill_discrete(name="Genero", labels=c("Feminino", "Masculino" ))


#Exercicio 2
#Veja graficamente se existe uma relação entre o salário inicial e corrente dos
#empregados no banco.
ggplot(data=dat) +
  geom_point(mapping=aes(x=salbegin, y=salary, color=as.factor(jobcat)))+
  geom_smooth(mapping = aes(x=salbegin, y=salary))+
  labs(y="Salario", x="Salario inicial")+
  labs(title = "Rela??o entre Sal?rio Inicial e Corrente")+
  labs(color="Categoria de Trabalho", labels=c("Secretario", "Auxiliar", "Gerente"))


#Exercicio 3
#Com base nas informações salarias, descubra graficamente que mais evoluiu (homens
#ou mulheres) em termos de renda do trabalho. Lembre-se a que variável salário é
#contínua e para contrastar com uma variável categórica necessita de manipulação.
ggplot(data=dat) +
  geom_point(mapping=aes(x=salbegin, y=salary))+
  geom_smooth(mapping = aes(x=salbegin, y=salary, fill=gender))+
  labs(y="Salario", x="Salario Inicial")+
  labs(title="Progress?o de Sal?rio por Sexo")+
  scale_fill_discrete(name="Genero",labels=c("Feminino", "Masculino"))

#Exercicio 3
#Construa uma nova variável para anos de estudo, com as categorias, até 12 anos de
#estudo (secundário completo), entre 12 a 15 anos (ensino superior incompleto) e 16
#ou mais anos (superior completo), e analise com um gráfico a distribuição de
#escolaridade entre trabalhadores que são de grupos minoritários e não.
dat%>%mutate(nova=case_when(educ<=12~'secundario completo',
                            educ>=13 & educ<=15~ 'superior incompleto',
                            educ>=16~ 'superior completo')) ->dat

ftable(dat$nova~dat$minority)

ggplot(data = dat) + 
  geom_bar(mapping = aes(x =as.factor(minority), fill=nova))+
  labs(title="Distribui??o de Escolaridade entre grupos minorit?rios ou n?o")+
  labs(y="Minorit?rios", x="Distribui??o")+
  scale_fill_discrete(name="Nivel de Escolaridade")

#Fim