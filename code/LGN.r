dist <- "beta"

## Beta
alpha <- 3
beta <- 2

## Log-normal
mu <- 0
sigma <- 2

if(dist == "ln"){
  EX <- exp(mu + sigma^2/2)  
}else{
  EX <- alpha / (alpha + beta)
}

computa_media <- function(n){
  if(dist == "ln"){
    mean(rlnorm(n = n, meanlog = mu, sdlog = sigma))
  }else{
    mean(rbeta(n = n, shape1 = alpha, shape2 = beta))
  }
}
ns <- seq(2, 1E5, by = 500)
medias <- sapply(ns, computa_media)

library(ggplot2)

forplot <- data.frame(n = ns, x_bar = medias)

ggplot(forplot, aes(x = n, y = x_bar))+
  geom_line() +
  scale_x_continuous("Tamanho de amostra", expand = c(0, 0)) + 
  scale_y_continuous(expression(bar(X[n])), expand = c(0, 0)) +
  geom_hline(yintercept = EX, linetype = "longdash", size = 2) +
  theme_bw(base_size = 16)

