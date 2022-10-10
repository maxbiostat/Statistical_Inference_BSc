is_in <- function(x, l, u){
  below <- x >= l
  above <- x <= u
  result <- as.logical(below * above)
  return(result)
}

get_ci <- function(x, gamma = 0.95){
  n <- length(x)
  cc <- qnorm(p = (1+gamma)/2)
  x_bar <- mean(x)
  Z <- cc*sqrt(x_bar/n)
  A <- x_bar - Z
  B <- x_bar + Z
  return(
    data.frame(xbar = x_bar, lwr = A, upr = B)
  )
}
#####
mu <- 2
n <-  50# 919 
M <- 5000

amostras <- matrix(rpois(n*M, lambda = mu),
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


##### Plots
library(ggplot2)


p1 <- ggplot() + 
  geom_pointrange(data = intervalos,
                  mapping = aes(x = amostra, y = mean,
                                ymin = L, ymax = U, colour = contem)) +
  geom_hline(yintercept = mu, linetype = "longdash") +
  ggtitle("Poisson aproximado") +
  theme_bw(base_size = 20)

p1 
# gridExtra::grid.arrange(p1, p2, ncol=2)

larguras.desc <- intervalos$U-intervalos$L
hist(larguras.desc, probability = TRUE)
mean(larguras.desc)
