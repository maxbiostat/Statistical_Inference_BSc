############
## Funções auxiliares
# Exemplo 9.1.18 De Groot
Lambda <- function(y, n, theta0, log = FALSE){
  l1 <- y * (log(n*theta0)-log(y))
  if(y==0) l1 <- 0
  l2 <- (n-y)*(log(n*(1-theta0))-log(n-y))
  if(y==n) l2 <- 0
  ans <- l1 + l2
  if(!log) ans <- exp(ans)  
  return(ans)
}
Lambda <- Vectorize(Lambda)
#
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

############
sample.size <- 10
p0 <- 0.01
alpha0 <- 1/5

# LRT: razão de verossimilhanças

tabProb <- data.frame(y = 0:sample.size,
                      Lambda = Lambda(y = 0:sample.size,
                                      n = sample.size,
                                      theta0 = p0),
                      Pr = dbinom(x = 0:sample.size,
                                  size = sample.size, prob = p0))
round(tabProb, 3)

plot(0:sample.size, Lambda(y = 0:sample.size,
                 n = sample.size,
                 theta0 = p0), xlab = expression(y),
     ylab = expression(Lambda(y)), type = "b")

ConjuntoRejeicao <- findSet(tab = tabProb)
ConjuntoRejeicao

TabRejeicao <- 
tabProb[match(ConjuntoRejeicao$confidence_set, tabProb$y), ]


TabRejeicao[which.max(TabRejeicao$Lambda),]

######

simula_e_testa <- function(N, theta0){
  Y <- rbinom(n = 1, size = N, prob = theta0)
  ttab <- data.frame(y = 0:N,
                     Lambda = Lambda(0:N, n = N, theta0 = theta0),
                     Pr = dbinom(x = 0:N, size = N, prob = theta0))
  test.set <- findSet(tab = ttab)
  the.test <- Y %in% test.set$confidence_set
  return(
    list(
      res = the.test,
      attained.size = test.set$test_size
    )
  )
}

M <- 10000
results <- sizes <- rep(NA, M)
for(i in 1:M){
  temp <- simula_e_testa(N = sample.size,
                         theta0 = p0)
  results[i] <- temp$res
  sizes[i] <- temp$attained.siz
} 
mean(results)
mean(sizes)



