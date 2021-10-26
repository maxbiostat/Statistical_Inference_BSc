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
  sigma_prime <- sqrt(var(x))
  Z <- cc*sigma_prime/sqrt(n)
  A <- x_bar - Z
  B <- x_bar + Z
  return(
    data.frame(xbar = x_bar, lwr = A, upr = B)
  )
}

get_ci_fixedVar <- function(x, v, gamma = 0.95){
  n <- length(x)
  zz <- qnorm(p = (1+gamma)/2)
  x_bar <- mean(x)
  Z <- zz*sqrt(v/n)
  A <- x_bar - Z
  B <- x_bar + Z
  return(
    data.frame(xbar = x_bar, lwr = A, upr = B)
  )
}
#####
mu <- 42
sigma_sq <- 30^2
n <-  5# 919 
M <- 5000

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
intervalos <- data.frame(amostra = 1:M, intervalos)

tail(intervalos)

mean(intervalos$contem)

######## Variancia conhecida

intervalos.vc <- data.frame(mean = rep(NA, M),
                         L = rep(NA, M),
                         U = rep(NA, M),
                         contem = rep(NA, M))

for( i in 1:M){
  intervalos.vc[i, 1:3] <- get_ci_fixedVar(amostras[i, ], v = sigma_sq)
  intervalos.vc$contem[i] <- is_in(x = mu,
                                l = intervalos.vc[i, 2], 
                                u = intervalos.vc[i, 3])
}
intervalos.vc <- data.frame(amostra = 1:M, intervalos.vc)

tail(intervalos.vc)
mean(intervalos.vc$contem)


##### Plots
library(ggplot2)


p1 <- ggplot() + 
  geom_pointrange(data = intervalos,
                  mapping = aes(x = amostra, y = mean,
                                ymin = L, ymax = U, colour = contem)) +
  geom_hline(yintercept = mu, linetype = "longdash") +
  ggtitle("Variância desconhecida") +
  theme_bw(base_size = 20)


p2 <- ggplot() + 
  geom_pointrange(data = intervalos.vc,
                  mapping = aes(x = amostra, y = mean,
                                ymin = L, ymax = U, colour = contem)) +
  geom_hline(yintercept = mu, linetype = "longdash") +
  ggtitle("Variância conhecida") +
  theme_bw(base_size = 20)


p1 
p2 
# gridExtra::grid.arrange(p1, p2, ncol=2)

larguras.desc <- intervalos$U-intervalos$L
larguras.vc <- intervalos.vc$U-intervalos.vc$L

hist(larguras.desc, probability = TRUE)
abline(v = larguras.vc[1], lwd = 3, lty = 2)

mean(larguras.desc)
larguras.vc[1]
