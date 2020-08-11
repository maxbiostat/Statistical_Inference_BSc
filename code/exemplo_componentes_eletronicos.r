### Distribuição a priori
## ! Dica: brinque com os hiperparâmetros da priori e olhe o que acontece com o segundo gráfico.
alpha <- 1
beta <- 2
theta <- rgamma(1e6, shape = alpha, rate = beta)

curve(dgamma(x, shape = alpha, rate = beta), min(theta), max(theta), 
      xlab = expression(theta), ylab = expression(xi(theta)), lwd = 3,  cex.lab=1.5)

## Gerando alguns dados

n <- 10
## ! Dica: mude o número de observações e veja o que acontece com a posteriori no segundo gráfico.

theta.vdd <- 0.65 # theta "verdadeiro"

X <- rexp(n = n, rate = theta.vdd)
mean(X); sd(X)

S <- sum(X)

curve(dgamma(x, shape = alpha, rate = beta), min(theta), max(theta), 
      xlab = expression(theta), ylab = "Densidade", lwd = 3,  cex.lab = 1.5)
curve(dgamma(x, shape = alpha + n + 1, rate = beta + S), min(theta), max(theta), 
      lwd = 3, lty = 2, col = "grey50", add = TRUE)
legend(x = "topright", legend = c("Priori", "Posteriori"), bty = 'n', lwd = 2, lty = 1:2, col = c("black", "grey50"))

## Agora, o aprendizado sequencial

curve(dgamma(x, shape = alpha, rate = beta), min(theta), max(theta), 
      xlab = expression(theta), ylab = "Densidade", lwd = 4,  cex.lab = 1.5)
for(i in 1:n){
  curve(dgamma(x, shape = alpha + n + 1, rate = beta + sum(X[1:i])), min(theta), max(theta), 
        lwd = 4, lty = 2, col = rev(heat.colors(n))[i], add = TRUE)
}
legend(x = "topright",
       legend = c("Priori", paste("Posteriori até x_", 1:n, sep = "")), bty = 'n', lwd = 3, lty = c(1, rep(2, n)),
       col = c("black", rev(heat.colors(n)[1:n])))
