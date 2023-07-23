library(MASS)

X1 <- data.frame(x = c(2,2,2,1,3), y = c(2,1,3,2,2))
X2 <- data.frame(x = c(6,6,6,5,7), y = c(0,1,-1,0,0))

m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)

S1 <- cov(X1)
S2 <- cov(X2)

n1 <- nrow(X1)
n2 <- nrow(X2)
n <- n1 + n2

W <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n - 2)
a <- ginv(W) %*% (m2 - m1)

plot(X1, xlim = c(-2,7), ylim = c(-2,4), pch = 21, col = "#AAAAAA", bg = "#EEEEEE", cex = 3, xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")
text(X1, "1", col = "blue", font = 2)
text(X2, "2", col = "orange", font = 2)

b <- 0.5 * t(a) %*% (m1 + m2)
abline(0, a[2] / a[1], col = "red", lty = 2)
abline(b / a[2], -a[1] / a[2], col = "red", lwd = 2)

S <- matrix(c(1,0,0,1),2,2)
mt1 <- c(2,2)
mt2 <- c(6,0)
n1 <- 60
n2 <- 60
n <- n1 + n2

X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S)

W <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n - 2)
a <- ginv(W) %*% (m2 - m1)

plot(X1, xlim = c(-2,7), ylim = c(-2,4), pch = 21, col = "#AAAAAA", bg = "#EEEEEE", cex = 3, xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")
text(X1, "1", col = "blue", font = 2)
text(X2, "2", col = "orange", font = 2)

b <- 0.5 * t(a) %*% (m1 + m2)
abline(0, a[2] / a[1], col = "red", lty = 2)
abline(b / a[2], -a[1] / a[2], col = "red", lwd = 2)

X1 <- data.frame(x = c(2, 2, 2, 1, 3, 2, 2), y = c(2, 1, 3, 2, 2, 2, 2), z = c(2, 2, 2, 2, 2, 1, 3))
X2 <- data.frame(x = c(4, 4, 4, 3, 5, 4, 4), y = c(4, 3, 5, 4, 4, 4, 4), z = c(4, 4, 4, 4, 4, 3, 5))

# Wyznaczenie wartoœci œrednich
m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)

# Wyznaczenie macierzy kowariancji
S1 <- cov(X1)
S2 <- cov(X2)

# Liczba elementów klas 1 i 2 oraz ich ca³kowitej liczba
n1 <- nrow(X1)
n2 <- nrow(X2)
n <- n1 + n2

# Wyznaczenie macierzy zmiennoœci wewn¹trzgrupowej W
W <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n - 2)

# Wyznaczenie wektora a
a <- ginv(W) %*% (m2 - m1)

# Wyznaczanie wyrazu wolnego
b <- -0.5 * t(a) %*% (m1 + m2)

library(scatterplot3d)

s3d <- scatterplot3d(X1, pch = 21, color = "#AAAAAA", bg = "#EEEEEE", cex.symbols = 3, xlim = c(0, 5), ylim = c(0, 5), zlim = c(0, 5), angle = 123)

# Rzutowanie wspó³przêdnych punktów z 3D na 2D
X1.coords <- s3d$xyz.convert(X1)
X2.coords <- s3d$xyz.convert(X2)

# Tekst dla punktów klasy 1
text(X1.coords, "1", col = "blue", font = 2)

# Kreslenie hiperplaszczyzny
s3d$plane3d(-b / a[3], -a[1] / a[3], -a[2] / a[3], draw_lines = F, draw_polygon = T, polygon_args = list(col = rgb(1, 1, 0, 0.4)))

# Rysowanie punktów klasy 2
s3d$points3d(X2, pch = 21, col = "#AAAAAA", bg = "#EEEEEE", cex = 3)

# Tekst dla punktów klasy 2
text(X2.coords, "2", col = "orange", font = 2)

# Przypisanie wartosci do klas
X1 <- data.frame(x = c(1, 2, 2, 2, 3), y = c(2, 3, 2, 1, 2))
X2 <- data.frame(x = c(3, 4, 4, 4, 5), y = c(4, 5, 4, 3, 4))
X3 <- data.frame(x = c(4, 5, 5, 5, 6), y = c(6, 7, 6, 5, 6))
X4 <- data.frame(x = c(8, 9, 9, 9, 10), y = c(8, 9, 8, 7, 8))
X5 <- data.frame(x = c(9, 10, 10, 10, 11), y = c(10, 11, 10, 9, 10))

# Nanoszenie punktow na wykres
plot(X1, xlim = c(0,12), ylim = c(0,12), pch = 21, col = "#AAAAAA", bg = "#EEEEEE", cex = 3, xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")
points(X3, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")
points(X4, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")
points(X5, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")

text(X1, "1", col = "blue", font = 2)
text(X2, "2", col = "red", font = 2)
text(X3, "3", col = "darkgreen", font = 2)
text(X4, "4", col = "black", font = 2)
text(X5, "5", col = "orange", font = 2)

# Wyznaczanie srednich w klasach
m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)
m3 <- apply(X3, 2, mean)
m4 <- apply(X4, 2, mean)
m5 <- apply(X5, 2, mean)

# Wyznaczanie macierzy kowariancji
S1 <- cov(X1)
S2 <- cov(X2)
S3 <- cov(X3)
S4 <- cov(X4)
S5 <- cov(X5)

# Licznoœci klas
n1 <- nrow(X1)
n2 <- nrow(X2)
n3 <- nrow(X3)
n4 <- nrow(X4)
n5 <- nrow(X5)

# Ogólna licznoœæ oraz liczba klas
n <- n1 + n2 + n3 + n4 + n5
g <- 5

# Œrednia dla wszystkich punktów
m <- apply(rbind(X1, X2, X3, X4, X5), 2, mean)

# Macierz zmiennoœci miêdzygrupowej
B <- (n1*(m1 - m) %*% t(m1 - m) + n2*(m2 - m) %*% t(m2 - m) + n3*(m3 - m) %*% t(m3 - m) + n4*(m4 - m) %*% t(m4 - m) + n5*(m5 - m) %*% t(m5 - m))/(g-1)

# Macierz zmiennoœci wewn¹trzgrupowej
W <- 1/(n - g)*((n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3 + (n4 - 1) * S4 + (n5 - 1) * S5)

U <- ginv(W) %*% B

# Wyznaczenia wartosci i wektorow w³asnych
lambda <- eigen(U)

# Wektor w³asny odpowiadaj¹cy maksymalnej wartoœci w³asnej
a <- lambda$vectors[,which.max(lambda$values)]

# Rysowanie kierunku a
abline(0, a[2] / a[1], col = "violet", lwd = 2)

# Funkcja do rzutowania obserwacji na kierunek a
rzutowanie <- function(X, A) {
  Xz <- (X$y * A + X$x) / (A**2 + 1)
  Yz <- A * Xz
  data.frame(x = Xz, y = Yz)
}

# Wyznaczenie wspó³czynnika kierunkowego prostej
A <- a[2] / a[1]

# Wykreœlanie zrzutowanych punktów
points(rzutowanie(X1, A), col = "blue", pch = 19, cex = 1.5)
points(rzutowanie(X2, A), col = "red", pch = 19, cex = 1.5)
points(rzutowanie(X3, A), col = "darkgreen", pch = 19, cex = 1.5)
points(rzutowanie(X4, A), col = "black", pch = 19, cex = 1.5)
points(rzutowanie(X5, A), col = "orange", pch = 19, cex = 1.5)
