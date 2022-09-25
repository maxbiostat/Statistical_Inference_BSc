## Exercise 2, section 7.2, DeGroot
## Omega, the parameter space
theta1 <- .1
theta2 <- .2
n <- 8

## P_theta(X = x)
f1 <- function(x) dbinom(x = x, size = n, prob = theta1)
f1 <- Vectorize(f1)
f2 <- function(x) dbinom(x = x, size = n, prob = theta2)
f2 <- Vectorize(f2)

## Plotting
tb <- tibble::tibble(f1 = f1(0:n), f2  = f2(0:n))
tb
matplot(tb,
        ylab = expression(P[theta](X==x)), 
        xlab = expression(x))

ratio <- function(x, n) (4/9)^x * (9/8)^n

ratio(0:n, n)
f1(0:n)/f2(0:n)

n/(0:n)
-log(4/9)/log(9/8)
## Prior
pr1 <- .7
pr2 <- .3

## Posterior
p1 <- function(x) f1(x) * pr1
p2 <- function(x) f2(x) * pr2

post <- function(x) {
  v1 <- p1(x)
  v2 <- p2(x)
  return(
    v1/(v1 + v2)
  )
}

post(0:n)/(1-post(0:n))
p1(0:n)/p2(0:n)