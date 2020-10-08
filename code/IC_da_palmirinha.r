xbar <- 8.307849
s2bar <- 7.930452 ## 1/n * Delta^2; DElta^2 = sum((x-xbar)^2)
n <- 10

gama <- .99
cc <- qt(p = (1  + gama)/2, df = n-1) ## inv_cdf_T_{n-1}((1 + gamma)/2)
sigma.prime <- sqrt(n*s2bar/(n-1))
d <- cc * sigma.prime/sqrt(n)

a <- xbar - d
b <- xbar + d

c(a, b)
c(xbar, a, b)

### Supondo que sigma é conhecida
truesd <- 2

# (xbar - mu)/sqrt(sigma^2/n) ~ N(0, 1)

# Pr( -k < Z < k) = gamma
# Pr( xbar - k*sigma/sqrt(n) < mu < xbar + k*sigma/sqrt(n)) = gamma

# a > 0
# 2*Phi(k)-1 = gamma => a = Phi_inv((1+gamma)/2)

k <- qnorm(p = (1  + gama)/2, mean = 0, sd = 1)

d2 <- k*truesd/sqrt(n)
a2 <- xbar - d2
b2 <- xbar + d2

c(xbar, a, b)
c(xbar, a2, b2)
