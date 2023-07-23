library(MASS)

S <- matrix(c(1,0,0,1),2,2)

mt1 <- c(-1,1)
mt2 <- c(2,4)
mt3 <- c(-2,2)

n1 <- 30
n2 <- 30
n3 <- 30

n <- n1 + n1 + n3

X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S)
X3 <- mvrnorm(n3, mt3, S)

plot(X1, ylim = c(-2,7), xlim = c(-4,5), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "orange")
points(X3, pch = 19, col = "red")

m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)
m3 <- apply(X3, 2, mean)

S1 <- cov(X1)
S2 <- cov(X2)
S3 <- cov(X3)

m <- apply(rbind(X1, X2, X3), 2, mean)
g <- 3

B <- (n1*(m1 - m) %*% t(m1 - m) + n2*(m2 - m) %*% t(m2 - m) + n3*(m3 - m) %*% t(m3 - m))/(g-1)

W <- 1/(n - g)*((n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3)

U <- ginv(W) %*% B
lambda <- eigen(U)

a <- lambda$vectors[,which.max(lambda$values)]
abline(0, a[2] / a[1], col = "violet", lwd = 2)

rzutowanie <- function(X, A) {
  Xz <- (X[,2] * A + X[,1]) / (A**2 + 1)
  Yz <- A * Xz
  data.frame(x = Xz, y = Yz)
}

A <- a[2] / a[1]

points(rzutowanie(X1, A), col = "blue", pch = 19, cex = 1.2)
points(rzutowanie(X2, A), col = "orange", pch = 19, cex = 1.2)
points(rzutowanie(X3, A), col = "red", pch = 19, cex = 1.2)
