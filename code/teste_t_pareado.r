sigma_sq <- 20^2
mu.antes <- 140
mu.depois <- 120
n <- 50
set.seed(666)
X <- rnorm(n, mean = mu.antes, sd = sqrt(sigma_sq))
Y <- rnorm(n, mean = mu.depois, sd = sqrt(sigma_sq))

plot(X, Y, xlab = "Pressão arterial antes (mmHg)", ylab = "Pressão arterial depois (mmHg)")

t.test(X, Y, paired = TRUE)