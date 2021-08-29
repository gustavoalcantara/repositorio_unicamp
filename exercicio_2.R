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
