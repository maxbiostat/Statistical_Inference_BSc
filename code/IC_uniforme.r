is_in <- function(x, l, u){
  below <- x >= l
  above <- x <= u
  result <- as.logical(below * above)
  return(result)
}
get_ci <- function(x){
  y1 <- min(x)
  y2 <- max(x)
  return(
    data.frame(
      lwr = y1,
      upr = y2,
      width = y2-y1
    )
  )
}

######################
theta <- 10 # -pi^2/6
ll <- 1/2
M <- 10000

amostras <- matrix(runif(2*M, min = theta-ll, max = theta + ll),
                   nrow = M, ncol = 2)
cis <- apply(amostras, 1, get_ci)
cis.dt <- do.call(rbind, cis) 
inclusion <- unlist(
  lapply(cis,
         function(x) is_in(x = theta, l = x[1], u = x[2]))
)
mean(inclusion)
sum(inclusion)
head(cis.dt)

theta

hist(cis.dt$width)
abline(v = ll, lwd = 3, lty = 2)

boxplot(cis.dt$width~inclusion)
abline(h = ll, lwd = 3, lty = 2)

head(cis.dt)
theta

smaller <- which(cis.dt$width < 1/2)
includes <- which(inclusion)

length(
  intersect(smaller, includes)  
)

