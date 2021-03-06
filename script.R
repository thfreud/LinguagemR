
#gerando o quantil a distribui��o normal com m�dia 33 e desvio padr�o 109 para alpha = 95%
x = qnorm(p = .95, 33, 109)
#Supondo meses de trinta dias, simulando trinta vari�veis aleat�rias normais com os mesmos par�metros
y = rnorm(30, mean = 33, sd = 109)
#Calcula os quantis do vetor simulado na linha anterior, retornando o quantil referente a probabilidade de 95%
quantile(y, probs = c(0.95))
# agora vamos demonstrar atrav�s da lei dos grandes n�meros que o var emp�rico converge para o te�rico
#vamos simular as perdas mensais em um loop de n = 5000
n = 5000
n2 = 5
memory = c()
y_mean = c()

for (i in 1:n) {
    
  
    y = rnorm(n2, mean = 33, sd = 109)
    #registra o quantil emp�rico em cada simula��o
    memory[i] = quantile(y, probs = c(0.95))
    #incrementa o tamanho da amostra
    n2 = n2 + 5
}


plot(seq(1,n,1),rep(x,n), type='l', ylab="Value at Risk M�dio", lwd=1, ylim = c(0.5*x, 1.3*x), col="white", xlab="Tamanho da amostra")

#lines(memory, col='dark blue', type='l', lwd=1)

#calcula a m�dia conforme o tamanho da amostra aumenta
estint = cumsum(memory)/(1:n) #m�dia acumulada
#calcula o desvio padr�o conforme o tamanho da amostra aumenta
esterr = sqrt(cumsum((memory-estint)^2))/(1:n) #Desvio padr�o acumulado
#plota a linha que representa a m�dia
lines(estint,type="l",lwd=2)
#plota os limites que envelopam a m�dia com dois desvios.
lines(estint+3*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)
