alpha0 <- 3
beta0  <- 1/3
theta.gen <- pi^2/6
nobs <- 1
X.data <- rpois(n = nobs, lambda = theta.gen)
S <- sum(X.data)
alphan <- alpha0 + S
betan <- beta0 + nobs

xm1 <- 0
xM1 <- 5*theta.gen 
iters <- 20
for(i in 1:iters){
  new.X <- rpois(n = 1, lambda = theta.gen)
  alphan <- alphan +  new.X
  betan <- betan + 1
  curve(dgamma(x, shape = alphan, rate = betan),
        xm1, xM1, lwd = 3,
        xlab = expression(theta),
        ylab = expression(pi(theta)),
        main = paste("Mais ", i, " amostras"))
  abline(v = theta.gen, lwd = 3, lty = 2)
}
