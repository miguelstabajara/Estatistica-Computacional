                         #Introdução
dados1 <- sample(x = 1:6, size=10000,replace =  TRUE)

dados2 <- sample(x = 1:6, size=10000,replace =  TRUE)

dados3 <- dados1+dados2

info <- dados3<=4

mean(info)

x <- seq(from=1,to=100,by=2) #Código que calcula de 1 até 100 pulando de 2 em 2

media <- c()

for(j in 1:500){
    
    media[j] <- mean((sample(x = 1:6,size=10000,replace=TRUE)+sample(x=1:6,size=10000,replace=TRUE))<=4)
    
}#Laço
media

#Laço do primeiro código cima
hist(media)#Gera histograma

#Utilização da ferramenta function em R
joga_dado <- function(n){
    
    dados <- sample(x=1:6,size=n,replace=TRUE)
    
    return(mean(dados==3))
    
}


#Retorna as posições TRUE em um vetor
which(c(TRUE, FALSE,TRUE,TRUE,FALSE,FALSE))


#Soma acumulada 
cumsum(x)

joga_dados<-sample(x=1:6,size=10000,replace=TRUE)
x <- joga_dados==3
proporcao3 <- cumsum(x)/1:10000
plot(x = 1:10000,y = proporcao3,plot="l")    


acertos<-0
j<-0
while(acertos<4){
    j <- j+1
    sorteio <- sample(x=1:60,size=6,replace=FALSE)
    bilhete <- sample(x=1:60,size=6,replace=FALSE)
    acertos <- sum(bilhete %in% sorteio) #%in% mostra quais números de bilhete estão dentro de sorteio
}
j/50

#Uso do while
acertos<-0
j<-0
gasto <-0
ganho<-0
while(acertos<6){
    j <- j+1
    gasto<-gasto+10
    sorteio <- sample(x=1:60,size=6,replace=FALSE)
    bilhete <- sample(x=1:60,size=6,replace=FALSE)
    acertos <- sum(bilhete %in% sorteio) #%in% mostra quais números de bilhete estão dentro de sorteio
    if(acertos==4) ganho<-ganho+10000
    if(acertos==5) ganho<-ganho+50000
    if(acertos==6) ganho<-ganho+10000000
}
j/50
ganho-gasto

iris[which(iris[,5]=="setosa"),]#Retorna apenas as setosas

iris[which(iris[,5]=="setosa"),2]#Retorna o tamanho da sepala

str(iris)#A função "str" retorna varias informações acerca do vetor ou objeto sendo utilizado
                                       #Aula 24/01
amostra <- sample(x=1:150,size=150,replace=FALSE)
iris<- iris[amostra,] #Embaralha os dados da matriz iris, mudando suas posições de lugar

#data(iris) retorna a condição inicial da matriz

n<-round(nrow(iris)*0.8) #utiliza o valor total de elementos para separar 80% para treinamento e 20% para teste(round arredonda para um inteiro e nrow conta a quantidade de elementos)

table(iris$species) #separa quantas especies tem de cada em uma tabela

treinamento<-iris[1:n,] #usa 80% dos dados(separados na linha 5) para treinamento
teste<-iris[(n+1):nrow(iris),] #uso de 80% até o final dos dados iris

table(treinamento$Species)
barplot(table(treinamento$Species))
unique(treinamento$Species) #Utiliza sequencia de dados não repitidos


treinamento
setosa<-iris[which(treinamento[,5]=="setosa"),] #Extrai somente os elementos que são setosas da matriz treinamento e assim para as proximas especies
virginica<-iris[which(treinamento[,5]=="virginica"),]
versicolor<-iris[which(treinamento[,5]=="versicolor"),]
hist(setosa$Petal.Length) #Gera histograma de frequencia de tamanho das petalas das SETOSAS
hist(virginica$Petal.Length)
hist(versicolor$Petal.Length)

#par(mfrow = c(1,3)) #Imprime os tres histogramas acima em apenas uma imagem


plot(x = treinamento$Petal.Length,y=treinamento$Petal.Width,type="n",pch=16) #Gera um gráfico de pontos com relação dos tamanhos com larguras das petalas da matriz de treinamento

points(x=setosa$Petal.Length,y=setosa$Petal.Width,pch=16,col="red")#Colore os pontos referentes a largura e altura das petalas da setosas
points(x=virginica$Petal.Length,y=virginica$Petal.Width,pch=16,col="green")
points(x=versicolor$Petal.Length,y=versicolor$Petal.Width,pch=16,col="blue")
abline(h=1.75) #Traça uma linha horizontal na altura 1.75
abline(v=2) #Traça uma linha vertical na altura 2

respostas<-c()
for(j in 1:nrow(teste)){
    if(teste$Petal.Length[j]<2.5){
        respostas[j]<-"setosa"
    }else if(teste$Petal.Width[j]<1.75){
        respostas[j]<-"versicolor"
    }else{
        respostas[j]<-"virginica"
    }
}
respostas #Esta ultima parte do código verifica as informações de largura e altura da petala no teste para classificar qual a especie da flor e guardando ela no vetor resposta

mean(respostas==teste$Species) #Compara as respostas com a especie dos testes e calcula a proporção de acerto

                                #Aula 26/01
dados<-read.table(file="pinguim.txt",header=TRUE,sep=",") #Importar dados de um arquivo de texto para um vetor

str(dados)
sum(dados[,1]=="Adelie")
sum(dados[,1]=="Gentoo")
sum(dados[,1]=="Chinstrap")
tabela<-table(dados$species)
barplot(tabela) #Gera tabela de barras

summary(dados) #Gera os resumos dos dados
table(dados$sex) #Contagem de elementos 

which(dados$sex==".") #Retorna um pinguim com erro
faltante<-which(dados$sex==".") #Guarda a posição do pinguim com erro

dados<-dados[-faltante,] #Remove o pinguim da posição que estava com erro, detalhe que só remove a linha

dados$sex<-as.factor(dados$sex) #Transforma o sexo de string para categoria 

erros<-which(is.na(dados$sex))
dados<-dados[-erros,] #Remove as linhas como na dando erro

dados$island<-as.factor(dados$island) #Transforma a ilha de string para categoria
dados$species<-as.factor(dados$species)

#library(ggplot2) Ativa o pacote de plot

ggplot(data=dados,mapping=aes(x=island,fill = species))+
    geom_bar()
    theme_linedraw()
#Função de plot da bibliotea ggplot

ggplot(data=dados,mapping=aes(x=culmen_length_mm,y=flipper_length_mm,color=species))+ #color para graficos de ponto e linha (unidimensional) e fill para multidimensional
    geom_jitter(size=2) #jitter e point são geometrias parecidas (jitter não sobrepõe pontos, agitando alguns dados)

ggplot(data=dados,mapping=aes(y=flipper_length_mm,x=species))+
    geom_boxplot()
    

                            #Aula 02/02
dados<-read.table(file = "pinguim.txt",
                  header=TRUE,
                  sep=",")
head(dados) #Mostra as primeiras linhas da tabela

str(dados)

unique(dados$sex)#Mostra quais tipos de dados da coluna sex existem

posicoes<-which(dados$sex==".")
dados<-dados[-posicoes,]
posicoes<-which(is.na(dados$sex))
dados<-dados[-posicoes,]
unique(dados$sex)

summary(dados)
dados$sex<-as.factor(dados$sex)
dados$island<-as.factor(dados$island)
dados$species<-as.factor(dados$species)

amostra<-sample(x=1:333,size=333,replace=FALSE)
dados<-dados[amostra,]

n<-round(nrow(dados)*0.8)
treinamento<-dados[1:n,]
teste<-dados[(n+1):nrow(dados),]

ggplot(data=treinamento,aes(x=species,fill=sex))+
    geom_bar()+
    labs(x="espécies",y="frequência",title="frequência de cada uma das espécies"     ,fill = "sexo")+
    scale_fill_manual(values =c("darkorange","darkred"),labels=c("macho","femea"
    ))

ggplot(data=treinamento,aes(x=sex,y=body_mass_g))+
    geom_boxplot()+
    facet_wrap(~species) #Separa o boxplot nas especies de pinguim

grafico_base<-ggplot(data=treinamento,aes(x=sex,y=body_mass_g))+
    geom_boxplot() #Guarda a estrutura do gráfico dentro do vetor

grafico_base+facet_wrap(~species) #Utiliza a estrutura do grafico e separa entre as especies

ggplot(data=treinamento,aes(x=culmen_length_mm,y = culmen_depth_mm,color=species))+
    geom_point(size = 1.5)+
    scale_color_manual(values=c("darkorange","darkred","darkgreen"))

respostas<-c()
for(j in 1:nrow(teste)){
    distancia<-c()
    for(k in 1:nrow(treinamento)){
        distancia[k]<-sqrt(sum((teste[j,c(3,4)]-treinamento[k,c(3,4)])**2))
    }
    respostas[j]<-as.character(treinamento$species[order(distancia)[1]]) #order retorna a posição com menor valor do vetor
}

mean(respostas==teste$species)

