## Vamos ilustrar o exemplo da proporção de itens defeitusos dado em De Groot, seção 7.4

n <- 100 #! Dica: mude o tamanho de amostra e veja o que acontece
y <- 10 #! Dica: mude o número de itens defeituosos e veja o que acontece com as posterioris

prior1 <- function(theta) dbeta(x = theta, shape1 = 1, shape2 = 1)
prior2 <- function(theta) dbeta(x = theta, shape1 = 1, shape2 = 2)

posterior1 <- function(theta) dbeta(x = theta, shape1 = 1 + y, shape2 = 1 + (n - y))
posterior2 <- function(theta) dbeta(x = theta, shape1 = 1 + y, shape2 = 2 + (n -y))

curve(prior1, xlab = expression(theta), ylab = "Density", lwd = 3, lty  = 1, ylim = c(0, 15))
curve(prior2, lwd = 3, lty = 2, col = "red", add = TRUE)
curve(posterior1, lwd = 3, add = TRUE)
curve(posterior2, lwd = 3, col = "red", lty = 2, add = TRUE)
abline(v = y/n, lty = 3, lwd = 3)
legend(x = "topright", legend = c("Prior/Posterior 1", "Prior/Posterior 2"),
       col = c("black", "red"), lty = 1:2, bty = 'n')