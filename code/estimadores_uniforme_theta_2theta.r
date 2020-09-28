mle <- function(x) max(x)/2 # W1
unbiased <- function(x) mle(x) * (2*n+2)/(2*n +1) # W2, unbiased
f_M <- function(t) n/theta * (t/theta - 1)^(n-1) 
f_M <- Vectorize(f_M)
####
theta <- pi^2/6
n <- 2
M <- 10000
####
data.sets <- matrix(runif(n = n*M, min = theta, max = 2*theta), ncol = n, nrow = M)

W1 <- apply(data.sets, 1, mle)
hist(W1, probability = TRUE)
mean(W1)
(2*n +1)/(2*(n+1)) * theta ## the actual E[W1]
theta
var(W1)
( VarW1 <- theta^2/4 * ((4*n^2 + 8*n + 2)/(n^2 + 3*n + 2) - ((2*n +1)/(n+1))^2) )
( biasW1 <- (1-(2*n +1)/(2*n+2))* theta )
VarW1 + biasW1^2 ## MSE W1

W2 <- apply(data.sets, 1, unbiased)
hist(W2, probability = TRUE)
mean(W2)
theta
var(W2) ## MSE W2
## E[W1] = c* theta => W2 = W1/c => Var(W2) = Var(W1)/c^2
## c = 1-biasW1/theta
(1-biasW1/theta)^-2 *  VarW1


# ### Extra
# eccdf <- function(x)
# {
#   x <- sort(x)
#   n <- length(x)
#   if (n < 1)
#     stop("'x' must have 1 or more non-missing values")
#   vals <- sort(unique(x))
#   rval <- approxfun(vals, 1-cumsum(tabulate(match(x, vals)))/n, #[CHANGED]
#                     method = "constant", yleft = 1, yright = 0, f = 0, ties = "ordered")
#   class(rval) <- c("eccdf", "stepfun", class(rval)) #[CHANGED]
#   attr(rval, "call") <- sys.call()
#   rval
# }
# 
# Ff2 <- function(y)  1- (y/theta - 1)^n
# 
# Ms <- apply(data.sets, 1, max)
# mean(Ms)
# plot(eccdf(Ms))
# curve(Ff2, theta, 2*theta, lwd = 2, col = 2, lty = 2, add = TRUE)
# 
# 
# (2*n + 1)/(n + 1) * theta
# n/(n + 1)* theta
# 
# var(Ms)
