#Exercício 1 - Crie os seguintes vetores:
#a)(10, 11, 12,…, 30)

v1<-seq(from=10,to=30,by=1)
v1
#b)(30, 29, 28,…, 10)
v2<-seq(from=30,to=10,by=-1)
v2
#c)(10, 11, 12,…, 30, 29, 28,…, 10)
v3<-c(seq(from=10,to=30,by=1),seq(from=29,to=10,by=-1))
v3

#Exercício 2 - Use a função help do R para descobrir o funcionamento das funções rep e seq. Em seguida, utilize estas funções para resolver os seguintes itens:
#a)Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8), em que há dez ocorrências do número 2
v4<-rep(seq(from=2,to=8,by=2),10)
v4
#b)Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,…, 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez
ocorrências dos números 4, 6 e 8.
v5<-rep(seq(from=2,to=8,by=2),11,length.out=41)
v5
#c)Considere o vetor (3, 7, 1). Crie um novo vetor que repita os valores do vetor original três vezes.
Depois, crie outro vetor onde o primeiro valor do vetor original se repita 4 vezes, o segundo valor se
repita duas vezes e o terceiro se repita três vezes mantendo a ordem original.
v6<-c(3,7,1)
v7<-rep(v6,3)
v7
v8<-rep(v6,c(4,2,3))
v8

#Exercício 3 - Utilize a estrutura de vetores do R para realizar as seguintes somas:
#a)Somatório: n=20 até 30, n^2+4n
n<-20
soma1<-0
while(n<=30){
  soma1<-soma1+(n*n)+(4*n)
  n<-n+1
}
soma1
#b)Somatório: n=10 até 20, (3^n/n)+(2^n/n^2)
n<-10
soma2<-0
while(n<=20){
  soma2<-soma2+((3**n)/n)+((2**n)/(n**2))
  n<-n+1
}
soma2

#Exercício 4 - Numa urna há bolas idênticas numeradas de 1 até 100. Serão extraídas 40 bolas com reposição desta urna. Simule este experimento e guarde o resultado dos sorteios em um vetor.
v9<-sample(x=1:100,size=40,replace=TRUE)
v9
#a)Quantas bolas pares foram sorteadas?
sum(v9%%2==0)
#b)Quantas bolas maiores do que 70 foram sorteadas?
sum(v9>70)
#c)Em quais retiradas (posições) foram sorteadas as bolas ímpares?
which(v9%%2!=0)

#Exercício 5 - Crie um função no R que irá simular sucessivos lançamentos de um dado até que o número 4 seja obtido pela segunda vez. A função deverá retornar o número de lançamentos que foram necessários até o 4 ser obtido pela segunda vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7.
cont<-0
cont2<-0
while(cont!=2){
  dado<-sample(x=1:6,size=1)
  if(dado==4) cont<-cont+1
  cont2<-cont2+1
}
cont2

#Exercício 6 - Utilize a função do exercício anterior para replicar o experimento dez mil vezes. Para cada replicação, guarde o número de lançamentos num vetor chamado quantidades. Por fim, calcule a média de quantidades. Interprete o resultado obtido.
vetor<-c()
for(j in 1:10000){
  cont<-0
  cont2<-0
  while(cont!=2){
    dado<-sample(x=1:6,size=1)
    if(dado==4) cont<-cont+1
    cont2<-cont2+1
  }
  vetor[j]<-cont2
}
vetor

#Exercício 7 - Os dois primeiros termos da sequência de Fibonacci são iguais a 1. Os termos subsequentes da sequência são encontrados somando os dois termos imediatamente anteriores. Escreva uma função com parâmetro de entrada n chamada fibonacci que retornará os primeiros n termos da sequência de Fibonacci para qualquer 𝑛 ≥ 3. Exemplo:
n<-20
cont<-3
fibo<-c(1,1)
while(length(fibo)!=n){
  fibo[cont]<-fibo[cont-1]+fibo[cont-2]
  cont<-cont+1
}
fibo

#Exercício 8 - Michael Scott é gerente regional da empresa Dunder Mufflin. Para as festividades de fim de ano, Michael propôs aos funcionários Dwight Schrute, Jim Halpert, Kevin Malone e Creed Bratton a realização de um amigo oculto entre eles. Consideraremos que o sorteio do amigo oculto deu errado quando uma pessoa sortear ela mesma (Michael tira Michael, por exemplo). Simule o sorteio do amigo oculto. Se ele deu certo, atribua o valor 1; caso contrário, atribua o valor 0 (zero). Em seguida, replique este experimento cem mil vezes e calcule a proporção de vezes que o amigo oculto deu errado.
erro<-0
acerto<-0
for(i in 1:100000){
  nomes<-c('Dwight','Jim','Kevin','Creed','Michael')
  sorteio<-sample(nomes)
  for(j in 1:5){
    if(nomes[j]==sorteio[j]){
        cont<-0
        break
    }else{
      cont<-1
    }
  }
  if(cont==1) acerto<-acerto+1
  if(cont==0) erro<-erro+1
}
erro/100000

#Exercício 9 - No jogo de Craps dois dados são lançados:
#• se a soma for 7 ou 11, você ganha o jogo;
#• se a soma for 2,3 ou 12, você perde o jogo;
#• caso contrário, os dois dados são rolados novamente até obter-se 7 (você perde) ou até obter-se a soma inicial (você ganha).
#Simule uma partida do jogo de Craps. Em seguida, replique este experimento 100 mil vezes; para cada experimento, atribua 1 para uma vitória e zero para uma derrota. Calcule a proporção de vezes que você ganhou o jogo.
ganhos<-c()
for(j in 1:100000){
    soma<-sample(x=1:6,size=1,replace=TRUE)+sample(x=1:6,size=1,replace=TRUE)
    aux<-0
    if((soma==7)||(soma==11)){
        ganhos<-c(ganhos,1)
        aux<-1
    }
    if((soma==2)||(soma==3)||(soma==12)){
        ganhos<-c(ganhos,0)
        aux<-1
    }
    while(aux==0){
        soma1<-sample(x=1:6,size=1,replace=TRUE)+sample(x=1:6,size=1,replace=TRUE)
        if(soma1==7){
            ganhos<-c(ganhos,0)
            aux<-1
        }
        if(soma1==soma){
            ganhos<-c(ganhos,1)
            aux<-1
        }
    }
}
proporção<-sum(ganhos)/100000
proporção

#Exercício 10 - Luke Skywalker realizará o seguinte passeio aleatório na reta: a reta do passeio é formada pelos números inteiros de zero até 𝑁; Luke está em um ponto 𝐿 que é maior do que zero e menor do que 𝑁; Luke lança uma moeda honesta; se sair coroa, ele dá um passo para a esquerda (e termina na posição 𝐿 − 1 da reta); se sair cara, ele dá um passo para a direita (e termina na posição 𝐿 + 1 da reta). Luke continuará a lançar a moeda e se deslocará até que ele chegue em sua casa (e lá ele vai dormir e o passeio acaba) ou até que ele chegue (caia) no precipício (e, óbvio, o passeio também acaba nesse caso).

#a)Para 𝑁 = 20, crie uma função cuja entrada seja 𝐿 (um número maior do que zero e menor do que 20) e que retorne 1 se Luke terminou um passeio em sua casa ou retorne zero se Luke caiu no precipício.
destino<-c()
luke = function(l){
    n<-20
    while(l!=n&&l!=0){
        moeda<-sample(0:1,size=1) #utilizando 1 para cara e 0 para coroa
        if(moeda==1) l<-l+1
        else l<-l-1
    }
    if(l==n) return(1)
    if(l==0) return(0)
}
destino<-c(destino,luke(10))
destino

#b)Crie uma função cuja entrada seja 𝐿; esta função deverá replicar o passeio da letra (a) 10 mil vezes e retornar a proporção de vezes que Luke chegou em sua casa. Sugestão: crie um vetor que, para cada replicação, guardará o resultado de um passeio; cada entrada deste vetor será zero ou 1; zero se Luke caiu no precipício e 1 se Luke chegou em casa.
destino<-c()
luke1 = function(l){
    for(j in 1:10000){
        n<-20
        aux=l
        while(aux!=n&&aux!=0){
            moeda<-sample(0:1,size=1) #utilizando 1 para cara e 0 para coroa
            if(moeda==1) aux<-aux+1
            else aux<-aux-1
        }
        if(aux==n) destino<-c(destino,1)
        if(aux==0) destino<-c(destino,0)
    }
    return(sum(destino)/10000)
}
proporcao<-luke1(10)
proporcao

#c)Use a função criada em (b) para 𝐿 = 1, 2, … , 19 e, em seguida, use esses valores para plotar um gráfico de 𝑥 = 1 ∶ 19 por 𝑦, em que 𝑦 são as proporções retornadas pela função criada em (b) para cada 𝑥.

estatisticas<-c()
for(j in 1:19){
    estatisticas<-c(estatisticas,luke1(j))
}
plot(1:19,estatisticas)

