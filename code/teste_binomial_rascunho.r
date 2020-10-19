n <- 10
alpha0 <- .05
p0 <- 1/2

poder_c <- function(c){
  ## Pr(Y >= c | p = p0)
  res <- pbinom(q = c, size = n, prob = p0, lower.tail = FALSE)
  return(res)
}
poder_c <- Vectorize(poder_c)

# Agora vamos plotar para tentar achar c* tal que Pr(Y >= c | p = p0) <= alpha0

curve(poder_c, 1, n, lwd = 2)
abline(h = alpha0, lty = 2)
