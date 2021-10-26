N <- 1e6
b <- 10

X <- runif(n = N, min = 0, max = b)
Y <- runif(n = N, min = 0, max = b)


U <- (X-b/2)^2
V <- (Y-b/2)^2
W <- U + V

ecc <- ecdf(W)

hist(W)

F_T <- function(t){
  b <- b + 0i
  t <- t + 0i
  Re(
    (b*sqrt(4*t-b^2) - 4*t*atan(sqrt(4*t-b^2)/b) + 2*pi*t)/(2*b^2)
  )
}

r <- runif(1, 0, b/2)
ecc(r^2)
F_T(r^2)
(pi*r^2)/b^2

thN <- sample(1:N, min(1000, N), replace = FALSE)
plot(X[thN], Y[thN], col = as.numeric(W[thN] <= r^2) + 1)
plotrix::draw.circle(x = b/2, y = b/2, radius = r, lwd = 4)

var_delta <- function(A, u, n){
  sapply(A, function(x) {
    x*(u^2-x)/n  
  })
}
# var_delta <- Vectorize(var_delta)
uu <- 1
curve(var_delta(x, u = uu, n = 10), 0, pi*uu^2/4, lwd = 3,
      ylab = expression(Var(delta[1])), xlab = expression(A))
curve(var_delta(x, u = uu, n = 30), 0, pi*uu^2/4,
      lty = 2, lwd = 3, add = TRUE)
curve(var_delta(x, u = uu, n = 100), 0, pi*uu^2/4,
      lwd = 3, lty = 4, add = TRUE)
abline(v = uu^2/2, lwd = 2, lty = 5)
legend(x = "topleft",
       legend = c("n=10", "n=30", "n=100"),
       lwd = 2, lty = 1:4, col = 1, bty = 'n')

