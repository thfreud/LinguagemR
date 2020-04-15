x = qnorm(p = .95, 33, 109)
x
y = rnorm(30, mean = 33, sd = 109)
quantile(y, probs = c(0.95))
#vamos simular as perdas mensais em um loop de n = 10000
n = 5000
n2 = 5
memory = c()
y_mean = c()

for (i in 1:n) {
    
  
    y = rnorm(n2, mean = 33, sd = 109)
    
    memory[i] = quantile(y, probs = c(0.95))
  
    n2 = n2 + 5
}


plot(seq(1,n,1),rep(x,n), type='l', ylab="Mean VaR", lwd=1, ylim = c(0.5*x, 1.3*x), col="white")

#lines(memory, col='dark blue', type='l', lwd=1)


estint = cumsum(memory)/(1:n) #média acumulada
esterr = sqrt(cumsum((memory-estint)^2))/(1:n) #Desvio padrão acumulado
lines(estint,type="l",lwd=2)
#plot(estint, xlab="Mean VaR",type="l",lwd=2)
lines(estint+3*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)
