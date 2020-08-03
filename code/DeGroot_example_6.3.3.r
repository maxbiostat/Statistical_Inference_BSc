#########################################################################
## Problema Amostre 12 variaveis aleatorias independentes uniforme(0, 1) 
## Defina x_bar = soma(X)/12
## Quanto vale Pr(|x_bar - 0.5| < 0.1)?
## Para a solução exata, notar que a soma de n v.a.s U(0, 1) tem 
### distribuição Irwin-Hall (https://en.wikipedia.org/wiki/Irwin%E2%80%93Hall_distribution)
### e que Pr(|X - 1/2| < 0.1) = Pr(0.4 < X < 0.6) = Pr(X<0.6)-Pr(X<0.4).
#########################################################################
## Funcoes auxiliares
IW_cdf <- function(x, n){
  if(x > n) return(1)
  ks <- 0:floor(x)
  vals <- sapply(ks, function(k) (-1)^k * choose(n, k) * (x-k)^n)
  S <- sum(vals)
  ans <- exp(log(S)-lfactorial(n))
  return(ans)
}
IW_cdf <- Vectorize(IW_cdf)
######################
## Usando TCL
aproximacao.normal <- pnorm(1.2)-pnorm(-1.2)

## Usando Monte Carlo
M <- 1E6
amostras <- matrix(NA, nrow = M, ncol = 12)
for(i in 1:M) amostras[i, ] <- runif(12)

medias <- apply(amostras, 1, mean)

aproximacao.monte.carlo <- mean(abs(medias-0.5) < 0.1)

## Exato
curve(IW_cdf(x, n = 12), 0, 12)
abline(v = 0.4 * 12, lwd = 2, lty = 2)
abline(v = 0.6 * 12, lwd = 2, lty = 2)

exato <- IW_cdf(0.6*12, n = 12)-IW_cdf(0.4*12, n = 12)

#### Comparando
aproximacao.normal
aproximacao.monte.carlo
exato

aproximacao.normal - exato
aproximacao.monte.carlo - exato