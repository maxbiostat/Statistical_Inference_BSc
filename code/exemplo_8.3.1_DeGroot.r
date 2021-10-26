dados.nuvens <- read.csv("cloud_seeding.csv")

## Y ~ lognormal(mu, sigma_sq)
## log(Y) ~ normal(mu, sigma_sq)
hist(dados.nuvens$Seeded)
hist(log(dados.nuvens$Seeded))

X <- log(dados.nuvens$Seeded)

mean(X)
var(X)
sum((X-mean(X))^2)/26
sum((X-mean(X))^2)/25

x_bar <- mean(X)
S2_bar <- sum((X-mean(X))^2)/26

sqrt(26)/5

### P1 = Pr(|U| < 1/5 * sqrt(n))
### = Pr(U < 1/5 * sqrt(n))
#### U  = n * (mu_h - mu)^2/sigma_sq
a <- 1/5
p1 <- function(n){
 pchisq(q = a * sqrt(n), df = 1)
}
### P2 = Pr(0.64n < V < 1.44 n)
#### Pr(V < 1.44n) - Pr(V < 0.64n)
p2 <- function(n){
  pchisq(q = 1.44*n, df = n-1)-
    pchisq(q = 0.64*n, df = n-1)
}

par(mfrow = c(1, 2))
curve(p1, 1, 20, xlab = "n", main = "P_1(n)")
curve(p2, 1, 20, xlab = "n", main = "P_2(n)")

p3 <- function(n) p1(n)*p2(n)
ns <- 1:1500
tabelinha <- data.frame(
  n = ns,
  prob  = p3(ns)
)

head(tabelinha)

head(subset(tabelinha, prob >= 0.99))
