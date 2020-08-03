#### Suponha que x_1, ..., x_n ~ exponencial(theta)
#### A MLE de theta é theta_hat = n/sum(x)
#### Vamos derivar a distribuição amostral de theta_hat
#### Note que a soma de x_i, S, tem distribuição Gamma com parametros n e theta.
#### Depois note que 1/S tem distribuição gama inversa com parametros n e theta.
#### Por ultimo note que se X tem distribuição gama inversa com parametros a e b.
##### cX tem distribuição gama inversa com parametros a e cb.


library(invgamma)

computa_emv <- function(x){
  1/mean(x)
}

theta.vdd <- 2
M <- 10000
n <- 100

amostras <- matrix(NA, ncol = n, nrow = M)
for (j in 1:M){
  amostras[j, ] <- rexp(n = n, rate = theta.vdd)
}

EMVs <- apply(amostras, 1, computa_emv)

hist(EMVs, probability = TRUE)  
curve(invgamma::dinvgamma(x, shape = n, rate = n*theta.vdd),
      min(EMVs), max(EMVs), add = TRUE, lwd = 2)