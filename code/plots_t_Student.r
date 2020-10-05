f1 <- function(x) dnorm(x)
f2 <- function(x) dcauchy(x)
f3 <- function(x) dt(x, df = 1)
f4 <- function(x) dt(x, df = 5)
f5 <- function(x) dt(x, df = 30)

par(mfrow = c(1, 2))
curve(f1, -5, 5, xlab = expression(x), ylab = "Densidade", lwd = 2)
curve(f2, lwd = 2, col = "grey50", add = TRUE)
curve(f3, lwd = 2, col = "red", lty = 2, add = TRUE)
curve(f4, lwd = 2, col = "blue", lty = 3, add = TRUE)
curve(f5, lwd = 2, col = "red", lty = 4, add = TRUE)

curve(f1, 2, 5, xlab = expression(x), ylab = "Densidade", lwd = 2)
curve(f2, lwd = 3, col = "grey50", add = TRUE)
curve(f3, lwd = 3, col = "red", lty = 2, add = TRUE)
curve(f4, lwd = 3, col = "blue", lty = 3, add = TRUE)
curve(f5, lwd = 3, col = "red", lty = 4, add = TRUE)

legend(x = "topright",
       legend = c(
         "N(0,1)",
         "Cauchy",
         "T(1)",
         "T(5)",
         "T(30)"
       ),
       col = c("black", "grey50", "red", "blue", "red", "blue"),
       lty = c(1, 1, 2, 3, 4, 5),
       lwd = 2,
       bty = 'n')