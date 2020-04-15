
#gerando o quantil a distribuição normal com média 33 e desvio padrão 109 para alpha = 95%
x = qnorm(p = .95, 33, 109)
#Supondo meses de trinta dias, simulando trinta variáveis aleatórias normais com os mesmos parâmetros
y = rnorm(30, mean = 33, sd = 109)
#Calcula os quantis do vetor simulado na linha anterior, retornando o quantil referente a probabilidade de 95%
quantile(y, probs = c(0.95))
# agora vamos demonstrar através da lei dos grandes números que o var empírico converge para o teórico
#vamos simular as perdas mensais em um loop de n = 5000
n = 5000
n2 = 5
memory = c()
y_mean = c()

for (i in 1:n) {
    
  
    y = rnorm(n2, mean = 33, sd = 109)
    #registra o quantil empírico em cada simulação
    memory[i] = quantile(y, probs = c(0.95))
    #incrementa o tamanho da amostra
    n2 = n2 + 5
}


plot(seq(1,n,1),rep(x,n), type='l', ylab="Value at Risk Médio", lwd=1, ylim = c(0.5*x, 1.3*x), col="white", xlab="Tamanho da amostra")

#lines(memory, col='dark blue', type='l', lwd=1)

#calcula a média conforme o tamanho da amostra aumenta
estint = cumsum(memory)/(1:n) #média acumulada
#calcula o desvio padrão conforme o tamanho da amostra aumenta
esterr = sqrt(cumsum((memory-estint)^2))/(1:n) #Desvio padrão acumulado
#plota a linha que representa a média
lines(estint,type="l",lwd=2)
#plota os limites que envelopam a média com dois desvios.
lines(estint+3*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)
