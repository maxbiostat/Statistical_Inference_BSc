xbar <- 8.307849
s2bar <- 7.930452 ## 1/n * Delta^2; Delta^2 = sum((x-xbar)^2)
n <- 10

gama <- .99
cc <- qt(p = (1  + gama)/2, df = n-1) ## inv_cdf_T_{n-1}((1 + gamma)/2)
sigma.prime <- sqrt(n*s2bar/(n-1))
d <- cc * sigma.prime/sqrt(n)

a <- xbar - d
b <- xbar + d

c(a, b)
c(xbar, a, b)

### Supondo que sigma é conhecida
truesd <- 2

# (xbar - mu)/sqrt(sigma^2/n) ~ N(0, 1)

# Pr( -k < Z < k) = gamma
# Pr( xbar - k*sigma/sqrt(n) < mu < xbar + k*sigma/sqrt(n)) = gamma

# a > 0
# 2*Phi(k)-1 = gamma => a = Phi_inv((1+gamma)/2)

k <- qnorm(p = (1  + gama)/2, mean = 0, sd = 1)

d2 <- k*truesd/sqrt(n)
a2 <- xbar - d2
b2 <- xbar + d2

c(xbar, a, b)
c(xbar, a2, b2)

# Função poder

mu0 <- 7

poder_pamonha <- function(mu){
  x <- sqrt(n) * (mu0 + cc - mu)/truesd
  y <- sqrt(n) * (mu0 - cc - mu)/truesd
  res <- pnorm(x, mean = 0, sd = 1, lower.tail = FALSE) + pnorm(y, mean = 0, sd = 1)
  return(res)
}
poder_pamonha <- Vectorize(poder_pamonha)

curve(poder_pamonha, 0, 14, xlab = expression(mu), ylab = expression(pi(mu*"|"*delta)), lwd = 3)
abline(v = mu0, lty = 2, lwd = 2)

### Testando hipóteses (APG)
#### Unilateral
## H0: mu >= 7
## H1: mu < 7
### U > c => reijeito H0
### U < c => não rejeito H0
# X ~n(mu0, sigma^2)
U <- (sqrt(n) * (xbar-mu0))/sigma.prime
## Se H0 = vdd então U ~ T(n-1)
(p.valor <- pt(q = U, df = n-1)) ## T^{-1}(U; n-1)

## install.packages("BSDA")
BSDA::tsum.test(mean.x = xbar, s.x = sigma.prime, n.x = n,  mu = mu0, alternative = "less")
BSDA::tsum.test(mean.x = xbar, s.x = sigma.prime, n.x = n,  mu = mu0)  
