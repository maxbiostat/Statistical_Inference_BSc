mle <- function(x) max(x) # W1
unbiased <- function(x) mle(x) * (n+1)/(n) # W2, unbiased
moments <- function(x) 2*mean(x)
f_M <- function(t) n/theta * (t/theta)^(n-1) 
f_M <- Vectorize(f_M)
#

theta <- pi^2/6
n <- 10
M <- 10000
###########

data.sets <- matrix(runif(n = n*M, min = 0, max = theta), ncol = n, nrow = M)

##### MLE
W1 <- apply(data.sets, 1, mle)
hist(W1, probability = TRUE)
mean(W1)
n/(n+1) * theta ## the actual E[W1]
var(W1)
c <- n/(n+1)
( VarW1 <- (n/(n+2) - c^2)*theta^2   )
( MSE1 <- mean((theta-W1)^2) )
(2/((n+1)*(n+2)))*theta^2 ## true MSE1

##### Unbiased

W2 <- apply(data.sets, 1, unbiased)
hist(W2, probability = TRUE)
mean(W2)
theta ## the actual E[W2]
var(W2)
( VarW2 <- (1/(n*(n+2)))*theta^2   )
( MSE2 <- mean((theta-W2)^2) )

##### 

W3 <- apply(data.sets, 1, moments)
hist(W3, probability = TRUE)
mean(W3)
theta ## the actual E[W2]
var(W3)
( VarW3 <- theta^2/(3*n)   )
( MSE3 <- mean((theta-W3)^2) )

###

MSE1
MSE2
MSE3

