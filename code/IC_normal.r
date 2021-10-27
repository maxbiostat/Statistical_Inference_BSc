truemu <- -.5
truesd <- 200

n <- 10
M <- 10000
gama <- .90
cc <- qt(p = (1  + gama)/2, df = n-1)

computa_D <- function(x) {
  n <- length(x)
  Delta.sq <- sum((x - mean(x))^2)
  sigma.prime <- sqrt(Delta.sq/(n-1)) 
  d <- cc * sigma.prime/sqrt(n)
  return(d)
}

ta_no_intervalo <- function(intervalo){
  (intervalo[1] <= truemu) * (truemu <= intervalo[2])
}
#######

data.sets <- matrix(rnorm(n = n*M, mean = truemu, sd = truesd),
                    ncol = n, nrow = M)


Xbars <- apply(data.sets, 1, mean)
Ds <- apply(data.sets, 1, computa_D)
As <- Xbars - Ds
Bs <- Xbars + Ds  

Intervalos <- data.frame(lwr = As, upr = Bs)
Intervalos$contem <- apply(Intervalos, 1, ta_no_intervalo)

mean(Intervalos$contem) ## cobertura dos intervalos


########### Figura
praplotar <- data.frame(lwr = As, media = Xbars, upr = Bs,
                        inclui = as.factor(Intervalos$contem), replicata = 1:M)
library(ggplot2)

p0 <- ggplot(data = praplotar, aes(x = replicata, y = media, colour = inclui)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = truemu, linetype = "longdash", size = 1.5) +
  scale_y_continuous(expression(bar(X[n]) %+-%  D), expand = c(0, 0)) +
  theme_bw(base_size = 16)

p0
