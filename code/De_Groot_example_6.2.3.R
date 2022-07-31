### X_1, \ldots, X_n are i.i.d Bernoulli with p = 1/2
### S = sum(X_i)
### p = Pr(4n/10 < S < 6n/ 10)
## We want n such that p > 0.7
## Chebychev approx
computa_n_Chebychev <- function(p, theta = 0.5, a = .4){
  ans <- ceiling(
    1/(
      4 * (1-p) * (a-theta)^2
    )
  )
  return(ans)
}
calcula_p_exata <- function(n, theta = 0.5, a = .4){
  prob <- 
    pbinom(q = round((1-a)*n), size = n, p = theta) -
    pbinom(q = round(a*n)-1, size = n, p = theta)
  return(prob)
}
aa <- .4
tt <- .5
n.chute <- 2 
p.chute <- calcula_p_exata(n.chute, theta =  tt, a = aa)
alvo <- 0.7
cat("Tamanho de amostra é:", n.chute, " Alvo é: ", alvo, " p é:", p.chute, "\n")
fn_erro <- function(n){
  (calcula_p_exata(n = n, theta = tt, a = aa)-alvo)^2
}
if(p.chute < alvo){
  M <- 10000
  ns <- 2:M
  erros <- sapply(ns, fn_erro)
  print( n.opt <- ns[which.min(erros)] )
  calcula_p_exata(n.opt, theta = tt, a = aa)
}
computa_n_Chebychev(p = alvo, theta = tt, a = aa)
