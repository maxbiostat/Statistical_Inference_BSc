## De Groot (4th ed.) exemplo 7.6.11

gera_dados <- function(n, theta){
  X <- rexp(n = n, rate = theta)
  return(X)  
}

computa_emv <- function(x){
  theta.chapeu <- 1/mean(x)
}

estima <- function(n, theta){
  dados <- gera_dados(n = n, theta = theta)
  est <- computa_emv(dados)
  return(est)
}

dinvgamma <- function(x, alpha, beta, log = FALSE){
  ## densidade de uma gama inversa com parametros alpha e beta
  ## esta é a distribuição **exata** do EMV neste caso
  lconst <- alpha * log(beta) - lgamma(alpha)
  ldens <- -(alpha + 1)*log(x) - beta/x
  ans <- lconst + ldens
  if(!log) ans <- exp(ans)
  return(ans)
}
dinvgamma <- Vectorize(dinvgamma)
#############

M <- 1E4 ## repetições

theta.vdd <- .234

par(mfrow=c(2, 2))

## n = 10
n10 <- 10

ests.n10 <- sapply(1:M, function(i) estima(n = n10, theta = theta.vdd) )

hist(ests.n10, probability = TRUE, xlab = expression(hat(theta)), main = "Tamanho de amostra = 10")
curve(dnorm(x, mean = theta.vdd, sd = sqrt(theta.vdd^2/n10)), lwd = 2, add = TRUE)
curve(dinvgamma(x, alpha = n10, beta = n10*theta.vdd), lwd = 2, lty = 2, col = 2, add = TRUE)
abline(v = theta.vdd, lwd = 2, lty = 3)

## n = 50
n50 <- 50

ests.n50 <- sapply(1:M, function(i) estima(n = n50, theta = theta.vdd) )

hist(ests.n50, probability = TRUE, xlab = expression(hat(theta)), main = "Tamanho de amostra = 50")
curve(dnorm(x, mean = theta.vdd, sd = sqrt(theta.vdd^2/n50)), lwd = 2, add = TRUE)
curve(dinvgamma(x, alpha = n50, beta = n50*theta.vdd), lwd = 2, lty = 2, col = 2, add = TRUE)
abline(v = theta.vdd, lwd = 2, lty = 3)

## n = 100
n100 <- 100

ests.n100 <- sapply(1:M, function(i) estima(n = n100, theta = theta.vdd) )

hist(ests.n100, probability = TRUE, xlab = expression(hat(theta)), main = "Tamanho de amostra = 100")
curve(dnorm(x, mean = theta.vdd, sd = sqrt(theta.vdd^2/n100)), lwd = 2, add = TRUE)
curve(dinvgamma(x, alpha = n100, beta = n100*theta.vdd), lwd = 2, lty = 2, col = 2, add = TRUE)
abline(v = theta.vdd, lwd = 2, lty = 3)

## n = 100
n1000 <- 1000

ests.n1000 <- sapply(1:M, function(i) estima(n = n1000, theta = theta.vdd) )

hist(ests.n1000, probability = TRUE, xlab = expression(hat(theta)), main = "Tamanho de amostra = 1000")
curve(dnorm(x, mean = theta.vdd, sd = sqrt(theta.vdd^2/n1000)), lwd = 2, add = TRUE)
curve(dinvgamma(x, alpha = n1000, beta = n1000*theta.vdd), lwd = 2, lty = 2, col = 2, add = TRUE)
abline(v = theta.vdd, lwd = 2, lty = 3)
