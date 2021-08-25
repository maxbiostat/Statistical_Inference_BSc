### X1, ..., Xn ~ exponencial(lambda)

## Estimadores
delta <- function(samp) samp[1]
delta0 <- function(samp){
  s <- sum(samp)
  x1 <- samp[1]
  n <- length(samp)
  # est <- (n-1)/s * (1 - x1/s)^(n-2)
  est <- exp( ## versão numericamente estável
    log(n-1)-log(s) + (n-2)*log1p(- x1/s)
  )
  return(est)
}

## Simulando dados
Nsim <- 1000
N <- 20
lambda <- .3

amostras <- matrix(rexp(n = Nsim*N, rate = lambda),
                   ncol = N, nrow = Nsim)

### Aplicando os estimadores

Ds <- apply(amostras, 1, delta)
D0s <- apply(amostras, 1, delta0)

par(mfrow = c(1, 2))
hist(Ds, probability = TRUE,
     xlab = expression(delta))
abline(v = lambda, lwd = 2, lty = 2)

hist(D0s, probability = TRUE,
     xlab = expression(delta[0]))
abline(v = lambda, lwd = 2, lty = 2)

## EQM

mean((Ds-lambda)^2)
mean((D0s-lambda)^2)

mean(Ds)
mean(D0s)
