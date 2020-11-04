n <- 10
p0 <- 1/2
alpha0 <- 0.05

# c(p0)
# delta_c é um teste de tamanho alpha0=0.05

tamanho_c <- function(c){
  ## Pr(Y >= c | p = p0)
  res <-  1- sum(dbinom(0:(c-1), n, p0))
  return(res)
}
tamanho_c <- Vectorize(tamanho_c)

# Agora vamos plotar para tentar achar c* tal que Pr(Y >= c | p = p0) <= alpha0

curve(tamanho_c, 1, n, lwd = 2, xlab = expression(c))
abline(h = alpha0, lty = 2)

## Agora vamos expressar o poder como função da probabilidade postulada p0

Y <- 5

poder_p <- function(p){
  res <- 1- sum(dbinom(0:(Y-1), n, p))
  return(res)
}
poder_p <- Vectorize(poder_p)

curve(poder_p, xlab = expression(p[0]))
abline(h = alpha0, lty = 2)

# LRT

Lambda <- function(y, log = FALSE){
  l1 <- y * (log(n*p0)-log(y))
  if(y==0) l1 <- 0
  l2 <- (n-y)*(log(n*(1-p0))-log(n-y))
  if(y==n) l2 <- 0
  ans <- l1 + l2
  if(!log) ans <- exp(ans)  
  return(ans)
}
Lambda <- Vectorize(Lambda)

tabProb <- data.frame(y = 0:n, Lambda = Lambda(0:n), Pr = dbinom(x = 0:n, size = n, prob = p0))
round(tabProb, 3)
plot(0:n, Lambda(0:n), xlab = expression(y), ylab = expression(Lambda(y)))


findSet <- function(tab, level = alpha0){
  K <- nrow(tab)
  set <- NA
  accpr <- 0
  for(i in 1:K){
    accpr.tent <- accpr + tab$Pr[i]
    if(accpr.tent < level){
      accpr <- accpr.tent
      set <- c(set, tab$y[i])
    }else{
      next
    }
  }
  set <- na.omit(set)
  return(
    list(
      confidence_set = as.vector(set),
      test_size = accpr
    )
  )
}

findSet(tab = tabProb)