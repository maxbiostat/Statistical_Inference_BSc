delta_donald <- function(x){
  x[1]
}
#
delta_huguinho <- function(x){
  kH <- (n+3)/(2*n + 2)
  (1/kH)*min(x)
}
#
delta_zezinho <- function(x){
  kZ <- (3*n+1)/(2*n + 2)
  (1/kZ) * max(x)
}
#
delta_luisinho <- function(x){
  (min(x) + max(x))/2
}
#
computa_vies_qdd <- function(deltas, theta){
  (mean(deltas) - theta)^2
}
computa_eqm <- function(deltas, theta){
  mean((deltas-theta)^2)
}
#
theta.vdd <- 10
n <- 30
M <- 1e5
amostras <- matrix(NA, ncol = n, nrow = M)
for (j in 1:M){
  amostras[j, ] <- runif(n = n, min = theta.vdd/2, max = (3/2)*theta.vdd)
}

Ds <- apply(amostras, 1, delta_donald)
Hs <- apply(amostras, 1, delta_huguinho)
Zs <- apply(amostras, 1, delta_zezinho)
Ls <- apply(amostras, 1, delta_luisinho)

par(mfrow = c(2, 2))
hist(Ds, probability = TRUE, main = "Pato Donald",
     xlab = expression(delta[D]))
abline(v = theta.vdd, lwd = 3, lty = 2)
hist(Hs, probability = TRUE, main = "Huguinho",
     xlab = expression(delta[UH]))
abline(v = theta.vdd, lwd = 3, lty = 2)
hist(Zs, probability = TRUE, main = "Zezinho",
     xlab = expression(delta[UZ]))
abline(v = theta.vdd, lwd = 3, lty = 2)
hist(Ls, probability = TRUE, main = "Luisinho",
     xlab = expression(delta[L]))
abline(v = theta.vdd, lwd = 3, lty = 2)

###
#### Variância
# Donald
var(Ds)
theta.vdd^2/12
# Huguinho
var(Hs)
(4*n)/((n+3)^2*(n+2)) * theta.vdd^2
# Zezinho
var(Zs)
(4*n)/((3*n+1)^2*(n+2)) * theta.vdd^2
# Luisinho
var(Ls)
1/(2*(n+1)*(n+2)) * theta.vdd^2

#### Viés
# Donald
computa_vies_qdd(Ds, theta.vdd)
# Huguinho
computa_vies_qdd(Hs, theta.vdd)
# Zezinho
computa_vies_qdd(Zs, theta.vdd)
# Luisinho
computa_vies_qdd(Ls, theta.vdd)

#### EQM
# Donald
computa_eqm(Ds, theta.vdd)
theta.vdd^2/3
# Huguinho
computa_eqm(Hs, theta.vdd)
(4*n)/((n+3)^2*(n+2)) * theta.vdd^2
# Zezinho
computa_eqm(Zs, theta.vdd)
(4*n)/((3*n+1)^2*(n+2)) * theta.vdd^2
# Luisinho
computa_eqm(Ls, theta.vdd)
1/(2*(n+1)*(n+2)) * theta.vdd^2

#### Agora, o Tio Patinhas

var_alpha <- function(x){
  a <- (n+3)/(2*n + 2)
  b <-  (3*n+1)/(2*n + 2)
  gamma <- theta.vdd^2 /( (n+1)^2*(n+2))
  obj <- (n*x^2)/b^2+(2*(1-x)*x)/(a*b)+(n*(1-x)^2)/a^2
  return(gamma*obj)
}

par(mfrow = c(1, 1))
curve(var_alpha)


alpha_opt <- function(n){
  a <- (n+3)/(2*n + 2)
  b <-  (3*n+1)/(2*n + 2)
  (b^2*n-a*b)/((b^2+a^2)*n-2*a*b)
}
alpha.opt <- alpha_opt(n)

alpha.numopt <- optimise(var_alpha, interval = c(0, 1))

alpha.numopt$minimum
alpha.opt
(1/2) * (n/(n^2-n+1))

delta_patinhas <- function(x, w){
    (1-w)*delta_huguinho(x) + w*delta_zezinho(x)
}  
  
Ps <- apply(amostras, 1,
            function(x) delta_patinhas(x = x, w = alpha.opt))

computa_eqm(Ps, theta.vdd)
var(Ps)
var_alpha(alpha.opt)
var_alpha(alpha.numopt$minimum)
2/((5*n+3)*(n+2))*theta.vdd^2

computa_vies_qdd(Ps, theta.vdd)

par(mfrow = c(1, 2))
hist(Zs, probability = TRUE, main = "Zezinho",
     xlab = expression(delta[UZ]))
abline(v = theta.vdd, lwd = 3, lty = 2)
hist(Ps, probability = TRUE, main = "Tio Patinhas",
     xlab = expression(delta[P]))
abline(v = theta.vdd, lwd = 3, lty = 2)

###
## Comparando todo mundo em relação ao EQM
# Donald
theta.vdd^2/3
# Huguinho
(4*n)/((n+3)^2*(n+2)) * theta.vdd^2
# Zezinho
(4*n)/((3*n+1)^2*(n+2)) * theta.vdd^2
# Luisinho
1/(2*(n+1)*(n+2)) * theta.vdd^2
# Tio Patinhas
2/((5*n+3)*(n+2))*theta.vdd^2