library(MASS)
library(mvtnorm)
library(e1071)
library(klaR)

S1 <- matrix(c(4,0,0,4),2,2)
S2 <- matrix(c(2,0,0,2),2,2)

mt1 <- c(-3, -1)
mt2 <- c(2, 2)

n1 <- 40
n2 <- 30
n <- n1 + n2

X1 <- mvrnorm(n1, mt1, S1)
X2 <- mvrnorm(n2, mt2, S2)

X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
X2 <- data.frame(X2); colnames(X2) <- c("x", "y")

X1$class <- 1; X2$class <- 2

data <- rbind(X1, X2); data$class <- factor(data$class)

with(data, drawparti(class, x, y, method = "naiveBayes", xlab = "X", ylab = "Y", font = 2))

xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

gr <- expand.grid(x = xp, y = yp)

# Funkcja do wyznaczania prawdopodobieñstw a posteriori w metodzie Naive Bayes
f.nb <- function(X, m1x, m1y, m2x, m2y, sd1x, sd1y, sd2x, sd2y, pi1, pi2) {
  return(pi1 * dnorm(X[,1], m1x, sd1x) * dnorm(X[,2], m1y, sd1y) / (pi1 * dnorm(X[,1], m1x, sd1x) * dnorm(X[,2], m1y, sd1y) + pi2 * dnorm(X[,1], m2x, sd2x) * dnorm(X[,2], m2y, sd2y)))
}

m1x = mean(X1$x)
m1y = mean(X1$y)

m2x = mean(X2$x)
m2y = mean(X2$y)

sd1x = sd(X1$x)
sd1y = sd(X1$y)

sd2x = sd(X2$x)
sd2y = sd(X2$y)

# Prawdopodbieñstwa a priori
pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)

contour(xp, yp, matrix(f.nb(gr, m1x, m1y, m2x, m2y, sd1x, sd1y, sd2x, sd2y, pi1, pi2), length(xp)), add = T, levels = 0.5, lwd = 2, lty = 2, col = "blue")
