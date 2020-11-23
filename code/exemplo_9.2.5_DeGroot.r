r <- function(x, log = TRUE){
  xbar <- mean(x, na.rm = TRUE)
  ans <- n*(xbar - .5)
  if(!log) ans <- exp(ans)
  return(ans)
}

theta0 <- 0
theta1 <- 1
n <- 100
alpha0 <- 0.05
cprime <- qnorm(1-alpha0, sd = 1/sqrt(n))
cprime
logc <- n*(cprime-.5)

X0 <-rnorm(n, mean = theta0, sd = 1)   
X1 <-rnorm(n, mean = theta1, sd = 1)   

## Uma observação: este teste não é ruim:
cat("Rejeita H0 para X0?", r(X0) > logc, "\n")
cat("Rejeita H0 para X1?", r(X1) > logc, "\n")

## Porém...
type_II_prob <- function(n){
    ans <- pnorm(q = qnorm(1-alpha0, sd = 1/sqrt(n)), mean = theta1, sd = 1/sqrt(n), lower.tail = TRUE)
} 
ns <- c(1, 25, 100)
data.frame(n = ns, beta = type_II_prob(ns), logc = exp(ns*(cprime-.5)))

curve(type_II_prob, 1, 100, xlab = "Tamanho de amostra (n)", ylab = expression(beta(delta)))
