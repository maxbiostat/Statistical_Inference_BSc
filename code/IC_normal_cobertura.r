is_in <- function(x, l, u){
  below <- x >= l
  above <- x <= u
  result <- as.logical(below * above)
  return(result)
}

get_ci <- function(x, gamma = 0.95){
  n <- length(x)
  cc <- qt(p = (1+gamma)/2, df = n-1)
  x_bar <- mean(x)
  sigma_prime <- sqrt(sum((x-x_bar)^2)/(n-1))
  # sigma_prime <- sqrt(var(x))
  Z <- cc*sigma_prime/sqrt(n)
  A <- x_bar - Z
  B <- x_bar + Z
  return(
    data.frame(xbar = x_bar, lwr = A, upr = B)
  )
}
#####

mu <- 42
sigma_sq <- 30^2
n <-  919 
M <- 10000

amostras <- matrix(rnorm(n*M, mean = mu,
                         sd = sqrt(sigma_sq)),
                   ncol = n, nrow = M)

hist(amostras[1, ])
get_ci(amostras[1, ])

intervalos <- data.frame(mean = rep(NA, M),
                         L = rep(NA, M),
                         U = rep(NA, M),
                         contem = rep(NA, M))

for( i in 1:M){
  intervalos[i, 1:3] <- get_ci(amostras[i, ])
  intervalos$contem[i] <- is_in(x = mu,
                             l = intervalos[i, 2], 
                             u = intervalos[i, 3])
}

tail(intervalos)

mean(intervalos$contem)

### Exervicio: amostrar cada amostra aleatoria de uma
#### distribuição normal com média mu[i] e variancia v[i]
#### e repetir o experimento.´
