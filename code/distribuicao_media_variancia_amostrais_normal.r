mu <- pi
sigmaSq <- (1.3)^3

n <- 10
M <- 10000

svar <- function(x){
  n <- length(x)
  xb <- mean(x)
  sv <- sum((x-xb)^2)/n
  return(sv)
}

data.sets <- matrix(rnorm(n = n*M, mean = mu, sd = sqrt(sigmaSq)), ncol = n, nrow = M)

xbars <- apply(data.sets, 1, mean)
sbars <- apply(data.sets, 1, svar)

par(mfrow=c(1, 2))

hist(xbars, probability = TRUE, main = "Média amostral", xlab = expression(bar(X[n])))
curve(dnorm(x, mean = mu, sd = sqrt(sigmaSq/n)), min(xbars), max(xbars), lwd = 2, add = TRUE)

hist(sbars, probability = TRUE, main = "Variância amostral", xlab = expression(bar(S[n]^2)))
curve(dgamma(x, shape = (n-1)/2, rate = n/(2*sigmaSq) ), min(sbars), max(sbars), lwd = 2, add = TRUE)


