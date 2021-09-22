library(invgamma)

computa_emv <- function(x){
  1/mean(x)
}
alpha_emv <- function(x, c){
 exp(-c*x)
}

theta0 <- 2
cc <- 1
alpha.vdd <- exp(-cc*theta0)
M <- 10000
n <- 100

amostras <- matrix(NA, ncol = n, nrow = M)
for (j in 1:M){
  amostras[j, ] <- rexp(n = n, rate = theta0)
}

### Estimating theta
EMVs <- apply(amostras, 1, computa_emv)

mean(EMVs)
n/(n-1) * theta0

theo_eqm <- function(n, theta){
  theta^2 * (n^2+ n -2)/((n-1)^2*(n-2))
}
mean((EMVs-theta0)^2)
theo_eqm(n = n, theta = theta0)


var(EMVs)
(mean(EMVs)-theta0)^2
1/(n * theta0^2)

hist(EMVs, probability = TRUE)  
abline(v = theta0, lwd = 2, lty = 2)
curve(invgamma::dinvgamma(x, shape = n, rate = n*theta0),
      min(EMVs), max(EMVs), add = TRUE, lwd = 2)

## Estimating of alpha
alpha.EMVs <- sapply(EMVs, alpha_emv, c = cc)
hist(alpha.EMVs)
abline(v = alpha.vdd, lwd = 2, lty = 2)

mean((alpha.EMVs-alpha.vdd)^2)
var(alpha.EMVs)
(mean(alpha.EMVs)-alpha.vdd)^2

(alpha.vdd * log(alpha.vdd)^2)/(n *cc)
