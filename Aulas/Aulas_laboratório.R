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

                            #Aula 07/02
iris
cor(iris$Sepal.Length,iris$Sepal.Width) #Cor é uma função que retorna o coeficiente de correlação, quanto mais próximo de 1(em módulo, tanto perto de 1 quanto de -1), maior a relação linear entre os elementos
ggplot(data=iris,aes(x=Sepal.Length,y=Sepal.Width))+
    geom_point()

cor(iris$Petal.Length,iris$Petal.Width) 
ggplot(data=iris,aes(x=Petal.Length,y=Petal.Width))+
    geom_point()

cor(iris[,1:4]) #Pegar as informações das flores com correlações mais próximas de 1 e utilizar para filtrar os dados

#Buscar itens correlacionados para usar, itens não correlacionados podem atrapalhar as previsões

lm(iris$Petal.Length ~iris$Petal.Width) 
#Petal.Length = 2230*Petal.Width+1084
#Tomar cuidado para usar correlação por conta da causa

#Aula 09/02
dados <- read.csv("cancer.csv", header = TRUE)
str(dados)
dados$diagnosis <- as.factor(dados$diagnosis)

dados <- dados[sample(nrow(dados)),]

n <- round(0.8*nrow(dados))
n

treinamento <- dados[1:n,]
teste <- dados[-(1:n),]

ggplot(data = treinamento, aes(x = diagnosis))+
    geom_bar()

colnames(dados)

ggplot(data = treinamento, aes(y = radius_mean))+
    geom_boxplot()+
    facet_wrap(~diagnosis)

ggplot(data = treinamento, aes(x = concavity_mean, y = texture_mean, color = diagnosis))+
    geom_point()

cor(treinamento[,-1])

library(class)
?knn

#determinando o treinamento, o teste e escalonando os dados
treinamento.X <- scale(treinamento[,-1])
teste.X <- scale(teste[,-1])
treinamento.Y <- treinamento[,1]

#usando a funcao knn para construir o modelo com k=3 vizinhos
modelo.knn.cancer <- knn(train = treinamento.X, test = teste.X, cl = treinamento.Y, k = 3)

mean(modelo.knn.cancer == teste$diagnosis)

#matriz de confusao para determinar os falsos positivos e os falsos negativos.
table(modelo.knn.cancer, teste$diagnosis)

                            #Aula 16/02

dados<-iris[sample(nrow(iris)),]
n<-round(nrow(dados)*0.8)

treino<-dados[1:n,]
teste<-dados[-(1:n),]
passos_intervalo<-n/10


intervalos<-seq(from=1,to=n,by=passos_intervalo)
intervalos<-c(intervalos,n+1)
intervalos

library(class)
acertos<-rep(0,times=10)
indices<-0
for(j in 1:(length(intervalos)-1)){
    indices<-intervalos[j]:(intervalos[j+1]-1)
    teste_cv<-treino[indices,]    #Cross Validation
    treino_cv<-treino[-indices,]
    acertos_atual<-c()
    for(k in 1:10){
        modelo.knn<-knn(train=treino_cv[,-5],test=teste_cv[,-5],cl=treino_cv$Species, k)
        acertos_atual[k]<-mean(modelo.knn==teste_cv$Species)
    }
    acertos<-acertos+acertos_atual
}
media_acertos<-acertos/10
media_acertos

plot(x=1:10,y=media_acertos,type="l")
modelo_final<-knn(train=treino[,-5],test=teste[,-5],cl=treino$Species,k=6)
mean(modelo_final==teste$Species)

#Raspagem de dados da web
library(rvest)
library(dplyr)

url <- "https://www.bbc.com/portuguese/brasil-36473280"

html<-read_html(url)

html |>
    html_element("h1") |>
    html_text2()

html |>
    html_elements("a") |>
    html_text2()

url<-"https://www.imdb.com/chart/top/"
html<-read_html(url)
dados<-(html |>
    html_elements("h3") |>
    html_text2())[-1]
dados[-(251:262)] #Pega todos os nomes e elimina os lixos

url<-"https://www.imdb.com/chart/top/"
html<-read_html(url)
dados<-(html |>
            html_elements("ul.ipc-metadata-list") |>
            html_elements("li") |>
            html_text2())
dados


                        #Aula 21/02 - Raspagem de dados
url<-"https://www.imdb.com/chart/top/"

html<-read_html(url)
titulos<-(html |>
            html_elements("ul.ipc-metadata-list") |>
            html_elements("li") |>
            html_elements("h3") |>
            html_text2()|>
            str_replace("\\d+\\.\\s",""))
titulos

anos<-(html |>
           html_elements("ul.ipc-metadata-list") |>
           html_elements("li") |>
           html_elements("span.sc-be6f1408-8:first-child") |>
           html_text2())
anos

duracao<-(html |>
           html_elements("ul.ipc-metadata-list") |>
           html_elements("li") |>
           html_elements("span.sc-be6f1408-8:nth-child(2)") |>
           html_text2())
duracao

classificacao<-(html |>
           html_elements("ul.ipc-metadata-list") |>
           html_elements("li") |>
           html_element("span.sc-be6f1408-8:nth-child(3)") |>
           html_text2())
classificacao

dados<-data.frame(titulos,anos,duracao,classificacao)

library(stringr)
v<-c("21","ola",".3aw7")
str_view(v,"\\d")
str_view(v,"\\d+")

getwd()
write.csv(dados,file="top_250.csv") #Guarda o dataframe em uma planilha
write.table(dados,file="top_250.txt",sep=",",row.names=FALSE)


url<-"https://wisevoter.com/state-rankings/gun-violence-by-state/"
html<-read_html(url)

estados<-(html |>
              html_elements("table.shdb-on-page-table") |>
              html_table()|>
              as.data.frame())
estados<-estados[,-1]

estados$Gun.Ownership.Rate<-as.numeric(str_replace(estados$Gun.Ownership.Rate,"%",""))

str(estados)       

estados$Gun.Death.Rate<-as.numeric(str_replace(estados$Gun.Death.Rate, " per 100k" ,""))

estados$Red.or.Blue.State<-as.factor(estados$Red.or.Blue.State)

ggplot(data=estados,aes(x= Gun.Ownership.Rate,y = Gun.Death.Rate, color = Red.or.Blue.State))+
    geom_point()

                                #Aula 28/02
url<-"https://mathbits.com/MathBits/TISection/Statistics2/linearREAL.html"
html<-read_html(url)

dados<-(html|>
            html_elements("table.blackbordergreen")|>
            html_table()|>
            as.data.frame())
dados<-dados[-1,]
colnames(dados)<-c("som","temperatura")
dados$temperatura<-as.numeric(dados$temperatura)
dados$som<-as.numeric(dados$som)
dados$temperatura<-(dados$temperatura-32)*(5/9)

cor(dados$temperatura,dados$som)
cor(dados)

modelo<-lm(data=dados,formula=som~temperatura) 
ggplot(data=dados,aes(x=temperatura,y=som))+
    geom_point()+
    geom_smooth(method="lm",col="red",lwd=2)#Se adicionar ,se=FALSE a sombra de confiança some
#som = 0.3654*temperatura+6.9553

femur<-read.csv("femur.csv",header=TRUE)
femur<-femur[,-1]
which(femur[,1]=="Male")
homem<-femur[which(femur[,1]=="Male"),]
mulher<-femur[-which(femur[,1]=="Male"),]

cor(mulher$altura,mulher$femur)
modelo_mulher<-lm(data=mulher,formula=altura~femur)
modelo_mulher
ggplot(data=mulher,aes(x=altura,y=femur))+
    geom_point()+
    geom_smooth(method="lm",se=FALSE)

cor(homem$altura,homem$femur)
modelo_homem<-lm(data=homem,formula=altura~femur)
modelo_homem
ggplot(data=homem,aes(x=altura,y=femur))+
    geom_point()+
    geom_smooth(method="lm")
    

                              #Aula 01/03
install(GGally) #Biblioteca importante
library(GGally)
library(ISLR2)
ggcorr(iris[,-5],label=TRUE)

dados<-Boston
str(dados)
ggcorr(dados,label=TRUE)

modelo1<-lm(dados$medv~dados$lstat)
#Equação da reta: medv = -0.95*lstat+34.55

ggplot(data=dados,aes(x=medv,y=lstat))+
    geom_point()+
    geom_smooth(method="lm")

summary(modelo1)
hist(modelo1$residuals) #Não é simetrico, logo não é uma distribuição normal

modelo2<-lm(data=dados,medv ~ lstat + rm) #2 variaveis dependentes-3 dimensões
#Equação da reta: rad = 0.04703*tax-9.64827
ggplot(data=dados,aes(x=rad,y=tax))+
    geom_point()+
    geom_smooth(method="lm")

summary(modelo2) #Pr(>|t|) se ele for menor que 5, a variavel é importante pro modelo
#Adjusted R-squared*100= porcentagem de previsão
hist(modelo2$residuals) #Não é simetrico, logo não é uma distribuição normal

modelo3<-lm(data=dados,medv ~ .)
summary(modelo3)

modelo4<-lm(data=dados,medv~ . -age - indus)
summary(modelo4)

predict(modelo4,interval="confidence")

                    #Aula 06/03
#Conteudo para prova
N<-20
figurinhas<-1:N
resultados<-c()
probabilidades <- c(1,rep(3,times = 19)) #o primeiro tem três vezes menos chance de sair do que os outros elementos
for(j in 1:10000){
    minhas_figurinhas<-sample(figurinhas,size=N,replace = TRUE,prob = probabilidades)
    while(length(unique(minhas_figurinhas))<N){
        minhas_figurinhas<-c(minhas_figurinhas,sample(figurinhas,size = 1,prob = probabilidades))
    }
    resultados[j] = length(minhas_figurinhas)
}
mean(resultados)

#Aula
dados<-read.table(file = "marketing.txt",sep=";",header = TRUE)
boxplot(dados[,1:3])

ggcorr(dados,label=TRUE)

modelo1<-lm(data=dados,sales~youtube)
#sales = 0.04754*youtube+8.439
summary(modelo1)
hist(modelo1$residuals)

dados$previsao<-predict(modelo1)
ggplot(data = dados,aes(x=youtube,y=sales))+
    geom_point()+
    geom_smooth(method = "lm",se=FALSE)+
    geom_segment(aes(x=youtube,xend=youtube,y=sales,yend=previsao),col ="red")
#geom_segmente calcula o erro de cada ponto para a reta de regressão linear.

modelo2<-lm(data=dados,sales~facebook)
#sales = 0.20250*facebook+11.17397
summary(modelo2)

modelo3<-lm(data=dados,sales~newspaper)
#sales = 0.05469*newspaper+14.82169
summary(modelo3)

modelo4<-lm(data=dados,sales~youtube+facebook+newspaper)
#ou
modelo4<-lm(data=dados[,-5],sales~.)
summary(modelo4)
#Newspaper atrapalha o modelo

modelo5<-lm(data=dados,sales~youtube+facebook)
summary(modelo5)
#sales = 0.04575*youtube+0.18799*facebook+3.50532

respostas<-c()
jigsawOrdenado<-1:1000
for(i in 1:10000000){
    jigsaw<-sample(x=1:1000,size=1000,replace=FALSE)
    if(sum(jigsaw==jigsawOrdenado)==1000) respostas[i]<-1
    else respostas[i]=0
}
mean(respostas)

                            #Aula 15/03
marketing<-read.table(file = "marketing.txt",sep=";",header=TRUE)
str(marketing)

set.seed(123)
marketing <- marketing[sample(nrow(marketing)),]

n<-round(0.8*nrow(marketing))

treino<-marketing[1:n,]
teste<-marketing[-(1:n),]

str(treino)
cor(treino)

modelo1<-lm(data=treino, formula = sales~youtube) #Somente o youtube 
summary(modelo1)


modelo2<-lm(data=treino,formula=sales~ .) #Todo mundo 
summary(modelo2)

modelo3<-lm(data=treino,formula=sales~.-newspaper) #Todo mundo menos o newspaper
summary(modelo3)

hist(modelo3$residuals) #Verificar se é uma distirbuição normal em função dos erros

#Teste de normalidade
shapiro.test(modelo3$residuals) #p-value grande(acima 5%) não rejeita a hipotese nula(importante que seja alto!!!!)
qqnorm(modelo3$residuals)
qqline(modelo3$residuals,col="red")

treino$sqrt_youtube<-sqrt(treino$youtube)
modelo4<-lm(data=treino,formula = sales~facebook+sqrt_youtube)
summary(modelo4)
shapiro.test(modelo4$residuals)
qqnorm(modelo4$residuals)
qqline(modelo4$residuals,col="red")

#Previsao: sales = -1.87617+facebook*0.191107+sqrt(youtube)*1.06901

marketing$sqrt_youtube<-sqrt(marketing$youtube)
teste<-marketing[-(1:n),]

previsao<-predict(modelo4,newdata=teste,interval="prediction")

comparacao<-data.frame(inferior = previsao[,2],superior = previsao[,3],teste$sales)

                          #Aula 22/03
#Bibliotecas importantes
library(rpart)
library(rpart.plot)

#Arvore de decisão
iris
set.seed(123)
iris<-iris[sample(nrow(iris)),]
n<-round(nrow(iris)*0.8)
treino<-iris[(1:n),]
teste<-iris[-(1:n),]

arvore.iris<-rpart(data=treino,formula=Species~.)
rpart.plot(arvore.iris,extra=101)

previsao<-predict(arvore.iris,newdata = teste,type="class")
mean(previsao==teste$Species)

                #Floresta de decisão
#Demonstração
amostra1<-treino[sample(1:120,replace=TRUE,size=120),]
arvore.iris.1<-rpart(formula = Species~.,data=amostra1)

amostra2<-treino[sample(1:120,replace=TRUE,size=120),]
arvore.iris.2<-rpart(formula = Species~.,data=amostra2)

amostra3<-treino[sample(1:120,replace=TRUE,size=120),]
arvore.iris.3<-rpart(formula = Species~.,data=amostra3)

amostra4<-treino[sample(1:120,replace=TRUE,size=120),]
arvore.iris.4<-rpart(formula = Species~.,data=amostra4)

par(mfrow = c(2,2))
rpart.plot(arvore.iris.1,extra=101)
rpart.plot(arvore.iris.2,extra=101)
rpart.plot(arvore.iris.3,extra=101)
rpart.plot(arvore.iris.4,extra=101)

#Utilidade
library(randomForest) #Biblioteca importante

floresta.iris<-randomForest(formula=Species~.,data=treino,ntree=500)
previsao.floresta<-predict(floresta.iris,newdata=teste,type="class")
mean(previsao.floresta==teste$Species)

plot(floresta.iris)

#Exemplo

treino<-read.csv("train_digits.csv")
n<-as.numeric(treino[8,-1])
str(treino)

matriz<-matrix(n,ncol=28,byrow=TRUE)
matriz
image(matriz,col=gray.colors(2))

nova_matriz <- matrix(1:784,ncol=28,byrow=TRUE)
for(j in 1:28){
    nova_matriz[j,]<-rev(matriz[,j])
}
image(nova_matriz,col=gray.colors(2))

#Aula 27/03 importante para árvore e floresteas aleatórias

library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)
#Tratamento dos dados
dados<-read.table(file = "churn.txt",header=TRUE,sep=",")
str(dados)
dados<-dados[,-(c(1,2,3))]
unique(dados$Geography)#Verificar os níveis para verificar se transforma em fator

dados[,c(2,3,8,9,11)]<-lapply(dados[,c(2,3,8,9,11)],factor)
str(dados)

dados<-dados[sample(nrow(dados)),]
n<-round(nrow(dados)*0.8)
treino<-dados[(1:n),]
teste<-dados[-(1:n),]

#Os dois precisam ser próximos para respeitar as proporções após embaralhar
prop.table(table(treino$Exited))
prop.table(table(teste$Exited))
#Análise gráfica dos dados
ggplot(data=treino,aes(x=Exited))+
    geom_bar()

ggplot(data=treino,aes(x=Gender,fill=Exited))+
    geom_bar()

ggplot(data=treino,aes(x=Age,fill=Exited))+
    geom_histogram()+
    theme_minimal()

ggplot(data=treino,aes(x=Age,fill=Exited))+
    geom_density(alpha=0.5)

#Árvore de decisão
arvore.churn<-rpart(formula = Exited~.,data=treino,method="class")
rpart.plot(arvore.churn,extra=101)

previsao<-predict(arvore.churn,newdata=teste,type="class")
mean(previsao==teste$Exited)

#Floreste aleátoria
floresta.churn<-randomForest(formula=Exited~.,data=treino,ntree=500, importance = TRUE)

varImpPlot(floresta.churn) #Importante para ver dados mais úteis do modelo

previsao.floresta<-predict(floresta.churn,newdata=teste,type="class")
mean(previsao.floresta==teste$Exited)

plot(floresta.churn)

#Floresta aleatoria sem a variavel menos importante(HasCrCard) aparecida no varImpPlot

floresta.churn<-randomForest(formula=Exited~.-HasCrCard,data=treino,ntree=500, importance = TRUE)

varImpPlot(floresta.churn)

previsao.floresta<-predict(floresta.churn,newdata=teste,type="class")
mean(previsao.floresta==teste$Exited)

plot(floresta.churn)

#Aula 03/04
data(iris)
dados<-iris[,-5]
dados_padronizados<-scale(dados) #Leva todas as variaveis para mesma escala

#O metodo abaixo é ward.D2, é mais eficiente
matriz_distancia<-dist(dados_padronizados)
modelo<-hclust(matriz_distancia,method="ward.D2")

plot(modelo)
rect.hclust(modelo,k=3)

aglomerados<-cutree(modelo,k=3)
aglomerados

iris$Species[aglomerados==1]
#O metodo abaixo é single, mas menos eficiente
matriz_distancia<-dist(dados_padronizados)
modelo<-hclust(matriz_distancia,method="ward.D2")
plot(modelo)
rect.hclust(modelo,k=3)
aglomerados<-cutree(modelo,k=3)
aglomerados
iris$Species[aglomerados==1]

#Teste
library(rvest)
library(ggplot2)
library(tidyverse)
url<-"https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_violent_crime_rate"

html<-read_html(url)
dados<-(html |>
              html_element("table") |>
              html_table())
          
dados<-data.frame(dados)   
str(dados)
colnames(dados)[c(2,6)]<-c("violent.crime","assault")

estados<-map_data("state")

?map_data     

dados$region <- tolower(dados$Location)
dados<-dados[,-1]

dados_gerais<-left_join(estados,dados,by="region")

ggplot(data=dados_gerais)+
    geom_polygon(aes(x = long,y = lat,group = group,fill=Homicide))+
    theme(title = element_text(family="mono"),legend.position="bottom")+
    labs(title="Taxa de homicidio")+
    scale_fill_gradient(low = "#56B1F7",
                        high = "#132B43",)+
    theme_void()

?scale_fill_gradient

                #Aula 10/04
n<-10000
resultados<-sample(1:6,size=n,replace=TRUE)
mean(resultados)
soma_acumulada<-cumsum(resultados)/(1:n)
plot(x = 1:n,y=soma_acumulada,type = "l")
abline(h=3.5,col="red")

k<-1000000
acertos<-c()
for(j in 1:k){
    if(sum(sample(1:6,size=2,replace=TRUE))==7) acertos[j]<-1
    else acertos[j]<-0
}
mean(acertos)

n<-15
for(i in 1:10000){
    x<-sample(1:365,size=n,replace=TRUE)
    resultado[i]<-length(unique(x))<n
}
mean(resultado)


library(gganimate)
library(ggplot2)
library(png)
library(gifski)

x<-runif(1000000,-1,1)
y<-runif(1000000,-1,1)
c(x,y)
circulo<-x^2+y^2<=1
4*mean(circulo)
orde<-1:1000

dados<-data.frame(x,y,circulo,orde)

ggplot(dados,aes(x,y,col=circulo))+
    geom_point()+
    coord_fixed()+
    transition_states(orde)+
    shadow_mark(past=TRUE)


library(webdriver)
library(httr)
url<-"https://blaze1.space/api/roulette_games/history?startDate=2024-03-13T16:33:55.255Z&endDate=2024-04-12T16:33:55.256Z&page=1"
#Para uma página
dados<-content(GET(url))
dados$records[[2]]$color
sorteio<-c()
for(j in 1:100){
    sorteio[j]<-dados$records[[j]]$color
}
sorteio
prop.table(table(sorteio))
#Para varias páginas
urlbase<-"https://blaze1.space/api/roulette_games/history?startDate=2024-03-13T16:33:55.255Z&endDate=2024-04-12T16:33:55.256Z&page="
for(k in 1:10){
    url<- paste0(urlbase,k)
    dados<-content(GET(url))
    for(j in 1:100){
        sorteio<-c(sorteio,dados$records[[j]]$color)
    }
}
sorteio
prop.table(table(sorteio))

f<-c()
for(i in 1:100000){
    x[i]<-runif(1,0,1)
    f[i]<-exp(-(x[i]**2)/2)
}
mean(f)/sqrt(2*pi)





                                





