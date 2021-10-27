n <- 68
Fmax <- function(y, theta, log = FALSE){
  ans <- n*(log(y)-log(theta))
  if(y > theta) ans <- 0
  if(!log) ans <- exp(ans)
  return(ans)
}
poder <- function(theta){
  return(Fmax(2.9, theta) + 1 - Fmax(4, theta))
}
poder <- Vectorize(poder)

curve(poder, 1, 5, xlab = expression(theta),
      ylab = expression(pi(theta*"|"*delta)), lwd = 2)
curve(poder, 2.9, 4, lwd = 6, col = 2, add = TRUE)
abline(v = c(3, 4), lwd = 2, lty = 2)

curve(poder, 3, 4, xlab = expression(theta),
 ylab = expression(pi(theta*"|"*delta)), lwd = 2)
