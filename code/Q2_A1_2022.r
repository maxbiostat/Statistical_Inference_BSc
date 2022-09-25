delta_1 <- function(x) mean(x)
delta_2 <- function(x) var(x)
###########
set.seed(666)
samplesize <- 2
nrep <- 5E3
theta0 <- 1.2021 ## Zeta(3)

the.data <- matrix(rpois(n = samplesize * nrep, lambda = theta0),
                   nrow = nrep, ncol = samplesize)

D1s <- apply(the.data, 1, delta_1)
D2s <- apply(the.data, 1, delta_2)

hist(D1s, probability = TRUE, breaks = 50,
     main = "",
     xlab = expression(delta[i]),
     cex.axis = 1.5)
hist(D2s, probability = TRUE, col = 2, add = TRUE, breaks = 50)
# axis(1, cex.axis = 1.2)
# axis(2, cex.axis = 1.2)
abline(v = theta0, lwd = 3, lty = 2)
legend(
  x = "top",
  cex = 1.5,
  legend = c(expression(delta[1]), expression(delta[2])),
  col = c("grey50", "red"),
  pch = 16,
  bty = 'n'
)

mean((D1s-theta0)^2)
mean((D2s-theta0)^2)

var(D1s)
theta0/samplesize
var(D2s)
2*theta0^2 + theta0/2