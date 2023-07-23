library(MASS)

draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Prosta funkcja do wyznaczania przynale¿nosci
# metoda k-nn
find.knn <- function(class, M, k) {
  
  k.nodes <- order(M)[1:k]
  
  tab <- table(class[k.nodes])
  
  return(rownames(tab)[which.max(tab)])
}

# Macierz odleg³oœci
M <- as.matrix(dist(data[,1:2]))

# Klasyfikacja za pomoc¹ prostej funkcji
class.own <- sapply(1:nrow(data), function(i) find.knn(data$class, M[i,], 1))

library(class)

# Funkcja knn
class.knn <- knn(data[,1:2], data[,1:2], data$class, 1)

# Porównanie
table(class.knn, class.own)

# Rysowanie punktów
plot.data <- function(data) {
  
  cols <- c("blue", "orange")
  
  plot(data[,1:2], col = cols[data$class], cex = 2)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.6)
  
}

# Rozpinanie siatki
xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

gr <- expand.grid(x = xp, y = yp)

# Klasyfikacja na siatce za pomoc¹ prostej funkcji
gr.k <- sapply(1:nrow(gr), function(i) find.knn(data$class, sqrt((data$x-gr$x[i])^2 + (data$y - gr$y[i])^2), 1))

# Klasyfikacja na siatce za pomoc¹ funkcji knn
k1 <- knn(data[,1:2], gr, data$class, 1)

# Wykreœlanie punktów
plot.data(data)

# Granica klasyfikacyjna za pomoc¹ prostej funkcji
contour(xp, yp, matrix(gr.k == "1", length(xp)), add = T, levels = 0.5, lwd = 2, col = "blue")

# Granica za pomoc¹ funkcji knn
contour(xp, yp, matrix(k1 == "1", length(xp)), add = T, levels = 0.5, col = "orange", lty = 2, lwd = 2)

# Rysujemy punkty
plot.data(data)

# Uruchamiamy wstêpn¹ funkcjê
nodes.cond <- condense(data[,1:2], data$class)

# Zaznaczamy punkty
points(data[nodes.cond,1:2], cex = 3, col = "red", pch = 22, lwd = 2)

# Uruchamiamy drug¹ funkcjê
nodes.red <- reduce.nn(data[,1:2], nodes.cond, data$class)

# Rysujemy punkty od nowa i zaznaczamy je
plot.data(data)
points(data[nodes.red,1:2], cex = 3, col = "red", pch = 22, lwd = 2)

# Testujemy skutecznoœæ
table(data$class, knn(data[nodes.red,1:2], data[,1:2], data$class[nodes.red]))

draw.data.gauss3 <- function(S1, S2, S3, m1, m2, m3, n1, n2, n3) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  X3 <- mvrnorm(n3, m3, S3)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  X3 <- data.frame(X3); colnames(X3) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2; X3$class <- 3
  
  data <- rbind(X1, X2, X3); data$class <- factor(data$class)
  
  return(data)
}

S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)
m3 <- c(-2, 2)

n1 <- 30
n2 <- 20
n3 <- 30

data <- draw.data.gauss3(S1, S2, S2, m1, m2, m3, n1, n2, n3)

xp <- with(data, seq(min(x), max(x), length = 50))
yp <- with(data, seq(min(y), max(y), length = 50))

gr <- expand.grid(x = xp, y = yp)

gr.knn <- knn(data[,1:2], gr, data$class, 1)
image(x = xp, y = yp, matrix(as.numeric(gr.knn), 50), xlab = "x", ylab = "y")
points(data[,1:2], col = data$class, pch = 19)

par(mfrow = c(2,2))

gr.knn <- knn(data[,1:2], gr, data$class, 1)
image(x = xp, y = yp, matrix(as.numeric(gr.knn), 50), xlab = "x", ylab = "y")
title("k=1")
points(data[,1:2], col = data$class, pch = 19)

gr.knn <- knn(data[,1:2], gr, data$class, 2)
image(x = xp, y = yp, matrix(as.numeric(gr.knn), 50), xlab = "x", ylab = "y")
title("k=2")
points(data[,1:2], col = data$class, pch = 19)

gr.knn <- knn(data[,1:2], gr, data$class, 3)
image(x = xp, y = yp, matrix(as.numeric(gr.knn), 50), xlab = "x", ylab = "y")
title("k=3")
points(data[,1:2], col = data$class, pch = 19)

gr.knn <- knn(data[,1:2], gr, data$class, 5)
image(x = xp, y = yp, matrix(as.numeric(gr.knn), 50), xlab = "x", ylab = "y")
title("k=5")
points(data[,1:2], col = data$class, pch = 19)

# PD zrobiæ wykres zale¿noœci acc od knn dla próby testowej i ucz¹cej (dla nowej PT i PU te 10 punktow) i to samo dla TP i TN. (czyli 3 wykresy) 