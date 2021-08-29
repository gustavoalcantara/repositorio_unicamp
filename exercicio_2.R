#Nome: Gustavo Casteletti de Alc?ntara, RA: 210524
#Exercício 2. Resolva os próximos exercícios referente a características socioeconômica e reprodutivas de conglomerados Suíços. 

#O dado contém 47 observações em 6 variáveis, cada uma delas em percentagem, ou seja, em [0, 100]. 
#[, 1] Fecundidade Ig, "medida padronizada comum de fertilidade"; 
#[, 2] Agricultura% de homens envolvidos na agricultura como ocupação; 
#[, 3] Exame% recrutados recebendo nota mais alta no exame do exército; 
#[, 4] Educação% de educação além da escola primária para recrutados; 
#[, 5] Católico% ‘católico’ (em oposição a ‘protestante’); 
#[, 6] Nascidos vivos com mortalidade infantil que vivem menos de 1 ano. 


#Preparando o Ambiente
rm( list = ls( ) ) #limpando meus diret?rios
graphics.off()
getwd()
setwd('C:/Users/User/Desktop/Gustavo/Unicamp/Mestrado/1sem2021/dm026/exercicios/exerc2')
dat <- read.table('C:/Users/User/Desktop/Gustavo/Unicamp/Mestrado/1sem2021/dm026/exercicios/exerc2/exercicio2.txt', sep=',', header=T)

#Exercicio 1
#Realizando a estatistica explorat?ria para todas as vari?veis
#1) Faça uma análise descritiva das características socioeconômicas e reprodutivas das áreas no banco.
#Valores máximo, mínimo, média, 1º Quartil e 3º Quartil e mediana)
summary(dat$Education)
summary(dat$Agriculture)
summary(dat$Examination)
summary(dat$Fertility)
summary(dat$Catholic) 
summary(dat$Infant.Mortality)

#Exercicio 2
#Estime a correlação entre o nível de catolicismo e o número médio de filhos por mulher das localidades. 
#Faça outras estimações, e busque outras associações com a fecundidade, desta vez,
#considerando o grau de urbanização, o nível educacional da região e o nível de mortalidade infantil.
catolicos_filhos <- cor(dat$Catholic, dat$Fertility)#Correla??o entre o n?mero m?dio de filhos nivel de catolicismo
urban_filhos <- cor(dat$Agriculture, dat$Fertility) #grau de urbaniza??o e Fertilidade
education_filhos <- cor(dat$Education, dat$Fertility) #correla??o entre Educa??o e Fertilidade

#Exercicio 3
#Identifique as áreas com fecundidade abaixo do valor médio encontrado do banco.
fec_lower70 <- dat[which(dat$Fertility<70.14),]

#Exercicio 4
#Encontre o nível de escolaridade médio, o nível médio de catolicismo e o nível médio de urbanização das áreas com fecundidade muita baixa
#(valor de fecundidade abaixo do Primeiro quartil). Repita a operação, porém, desta vez, considerando as áreas com o maior nível reprodutivo
#(Valor de fecundidade acima do terceiro quartil). 

fec_lower64 <- dat[which(dat$Fertility<64.7),] 
#Descri??o das estatisticas
summary(fec_lower64$Education) #escolaridade
summary(fec_lower64$Catholic) #catolicismo
summary(fec_lower64$Agriculture) #urbaniza??o

#criando um novo d.f para os n?veis acima do 3? quartil ~78.45~
fec_higher78 <- dat[which(dat$Fertility>78.45),]
#descri??o das estatisticas
summary(fec_higher78$Education)
summary(fec_higher78$Catholic)
summary(fec_higher78$Agriculture)




