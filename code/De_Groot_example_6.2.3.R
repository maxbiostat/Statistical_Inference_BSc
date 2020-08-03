### X_1, \ldots, X_n are i.i.d Bernoulli with p = 1/2
### S = sum(X_i)
### p = Pr(4n/10 < S < 6n/ 10)
## We want n such that p > 0.7

calcula_p <- function(n){
  prob <- pbinom(q = round(0.6*n), size = n, p = .5) - pbinom(q = round(0.4*n)-1, size = n, p = .5)
  return(prob)
}
n <- 10
p <- calcula_p(n)
alvo <- 0.7
erro <- (p-alvo)^2
while(erro > .001){
  n <- n + 1
  p <- calcula_p(n)
  erro <- (p-alvo)^2
  if(n > 10000) break
}
n
p
