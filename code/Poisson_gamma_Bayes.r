alpha0 <- 3
beta0  <- 1/3
xm <- qgamma(p = 1-.9999, shape = alpha0, rate = beta0)
xM <- qgamma(p = .9999, shape = alpha0, rate = beta0) 

curve(dgamma(x, shape = alpha0, rate = beta0),
      xm, xM, lwd = 3,
      xlab = expression(theta),
      ylab = expression(pi(theta)))

### Prior predictive
M <- 1E4
theta.prior.draws <- rgamma(n = M,
                            shape = alpha0,
                            rate = beta0)

X.prior.draws <- rpois(n = M, lambda = theta.prior.draws)
hist(X.prior.draws, probability = TRUE)  
mean(X.prior.draws)
var(X.prior.draws)

### Posterior calculations
theta.gen <- pi^2/6
nobs <- 20
X.data <- rpois(n = nobs, lambda = theta.gen)
S <- sum(X.data)
alpha1 <- alpha0 + S
beta1 <- beta0 + nobs

xm1 <- qgamma(p = 1-.9999, shape = alpha1, rate = beta1)
xM1 <- qgamma(p = .9999, shape = alpha1, rate = beta1) 

xm2 <- min(xm, xm1)
xM2 <- max(xM, xM1)

curve(dgamma(x, shape = alpha1, rate = beta1),
      xm2, xM2, lwd = 3, col = 2, lty = 2,
      xlab = expression(theta),
      ylab = expression(pi(theta)))
curve(dgamma(x, shape = alpha0, rate = beta0),
      xm2, xM2, lwd = 3, add = TRUE)
legend(
  x = "topright",
  legend = c("priori", "posteriori"),
  col = 1:2,
  lty = 1:2,
  bty = 'n',
  lwd = 2
)

### Posterior predictive
theta.posterior.draws <- rgamma(n = M,
                            shape = alpha1,
                            rate = beta1)

X.posterior.draws <- rpois(n = M, lambda = theta.posterior.draws)
hist(X.posterior.draws, probability = TRUE)  
mean(X.posterior.draws)
var(X.posterior.draws)

ppc.draws <- rbind(
  data.frame(x_pred = X.prior.draws, distribution = "prior"),
  data.frame(x_pred = X.posterior.draws, distribution = "posterior")
)
  
library(ggplot2)

p1 <- ggplot(
  data = ppc.draws,
  aes(x = x_pred,
      colour = distribution,
      fill = distribution
      )
  ) +
  geom_histogram(alpha = .4) +
  scale_x_continuous(expand = c(0, 0),
                     expression(x[pred])) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 16)

p1
