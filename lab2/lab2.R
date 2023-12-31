library(MASS)
library(mvtnorm)

# Macierze kowariancji dla klas
S1 <- matrix(c(4,0,0,4),2,2)
S2 <- matrix(c(2,0,0,2),2,2)

# Warto�ci oczekiwane
mt1 <- c(-3, -1)
mt2 <- c(2, 2)

# Liczba punkt�w w klasach
n1 <- 40
n2 <- 30
n <- n1 + n2

# Generowanie rozk�ad�w
X1 <- mvrnorm(n1, mt1, S1)
X2 <- mvrnorm(n2, mt2, S2)

# Zamiana na ramki danych
X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
X2 <- data.frame(X2); colnames(X2) <- c("x", "y")

# Nanoszenie punkt�w na wykres
plot(X1$x, X1$y, ylim = c(-8,8), xlim = c(-8,8), xlab = "X", ylab = "Y", pch = 19, col = "blue", font = 2)
abline(v = 0, h = 0, col = "gray")
points(X2$x, X2$y, pch = 19, col = "orange")

# Definiowanie zakres�w wsp�prz�dnych
xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

# Rozpi�cie siatki na wsp�prz�dnych
gr <- expand.grid(x = xp, y = yp)

# Wyznaczenie g�sto�ci prawdopodobie�stwa dla ka�dego punktu siatki
# i zapisanie efekt�w do macierzy
X1.pdf <- matrix(dmvnorm(gr, mt1, S1), length(xp))
X2.pdf <- matrix(dmvnorm(gr, mt2, S2), length(xp))

# Wykre�lenie warstwic
contour(xp, yp, X1.pdf, add = TRUE, col = "blue")
contour(xp, yp, X2.pdf, add = TRUE, col = "orange")

# Przypisanie klas
X1$class <- 1; X2$class <- 2

# "Sklejenie" danych do jednej ramki
# oraz przekszta�cenie typu zmiennej przechowuj�cej klasy
data <- rbind(X1, X2); data$class <- factor(data$class)

# Metoda LDA
data.lda <- lda(class ~ x + y, data)

# Przewidywanie klas za pomoc� metody LDA
# korzystamy z tych samych danych co przy uczeniu
# czyli powt�rne podstawienie
data.lda.pred <- predict(data.lda, data)

CM <- table(data$class, data.lda.pred$class)

# Funkcja do wyznaczania prawdopodobie�stw a posteriori w metodzie LDA
f.lda <- function(X, m1, m2, S, pi1, pi2) {
  return(pi1 * dmvnorm(X, m1, S) / (pi1 * dmvnorm(X, m1, S) + pi2 * dmvnorm(X, m2, S)))
}

# Estymowane warto�ci oczekiwane
me1 <- apply(X1[,1:2], 2, mean)
me2 <- apply(X2[,1:2], 2, mean)

# Estymowane macierze kowariancji
Se1 <- cov(X1[,1:2])
Se2 <- cov(X2[,1:2])

# Macierz kowariancji do metody LDA
# to�sama z macierz� W
Se <- ((n1 - 1) * Se1 + (n2 - 1) * Se2) / (n1 + n2 - 2)

# Prawdopodbie�stwa a priori
pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)

# Por�wnanie prawdopodobie�stw a posteriori
data.comp <- cbind(f.lda(data[,1:2], me1, me2, Se, pi1, pi2), data.lda.pred$posterior[,1])

library(klaR)

# Rysowanie prostej rozdzielaj�cej, punkt�w etc
# Dla wi�kszej dok�adno�ci u�y� opcji prec = 200
with(data, drawparti(class, x, y, xlab = "X", ylab = "Y", font = 2))

# Por�wnanie z warto�ciami otrzymanymi z rozk�ad�w
contour(xp, yp, matrix(f.lda(gr, me1, me2, Se, pi1, pi2), length(xp)), add = T, levels = 0.5, lwd = 2, lty = 2, col = "blue")

# Wyznaczenie wektora a oraz wyrazu b
a <- t(me1 - me2) %*% ginv(Se)
b <- log(pi1 / pi2) - 0.5 * a %*% (me1 + me2)

# Wyznaczenie wyrazu wolnego i wsp�czynnika kierunkowego
B <- -b / a[2]
A <- -a[1] / a[2]

# Wykre�lanie prostej
abline(B, A, col = "red", lwd = 2)

# Metoda QDA
data.qda <- qda(class ~ x + y, data)

# Przewidywanie klas za pomoc� metody QDA
data.qda.pred <- predict(data.qda, data)

# Budowanie macierzy pomy�ek
CM <- table(data$class, data.qda.pred$class)

# Rysowanie prostej rozdzielaj�cej, punkt�w etc
with(data, drawparti(class, x, y, method = "qda", xlab = "X", ylab = "Y", font = 2))
# Funkcja do wyznaczania prawdopodobie�stw a posteriori w metodzie QDA
f.qda <- function(X, m1, m2, S1, S2, pi1, pi2) {
  return(pi1 * dmvnorm(X, m1, S1) / (pi1 * dmvnorm(X, m1, S1) + pi2 * dmvnorm(X, m2, S2)))
}

# Por�wnanie z warto�ciami otrzymanymi z rozk�ad�w
contour(xp, yp, matrix(f.qda(gr, me1, me2, Se1, Se2, pi1, pi2), length(xp)), add = T, levels = 0.5, lwd = 2, lty = 2, col = "blue")

library(e1071)
# Klasyfikator naiwnego Bayesa
data.nb <- naiveBayes(class ~ x + y, data)

# Przewidywanie klas za pomoc� klasyfikatora naiwnego Bayesa
# domy�lnie zwraca klasy
data.nb.pred <- predict(data.nb, data)

# opcja method = "raw" daje prawdopodobie�stwa a posteriori
data.nb.pred <- predict(data.nb, data, type = "raw")

# Rysowanie prostej rozdzielaj�cej, punkt�w etc
with(data, drawparti(class, x, y, method = "naiveBayes", xlab = "X", ylab = "Y", font = 2))

S1 <- matrix(c(4, 0, 0, 4), 2, 2)
S2 <- matrix(c(2, 0, 0, 2), 2, 2)
S3 <- matrix(c(2, 1, 1, 2), 2, 2)

mt1 <- c(-3, -1)
mt2 <- c(2, 2)
mt3 <- c(-2, 3)

n1 <- 40
n2 <- 30
n3 <- 30

X1 <- mvrnorm(n1, mt1, S1)
X2 <- mvrnorm(n2, mt2, S2)
X3 <- mvrnorm(n3, mt3, S3)

X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
X3 <- data.frame(X3); colnames(X3) <- c("x", "y")
X1$class <- 1; X2$class <- 2; X3$class <- 3

plot(X1$x, X1$y, ylim = c(-8,8), xlim = c(-8,8), pch = 19, col = "blue", font = 2)
abline(v = 0, h = 0, col = "gray")
points(X2$x, X2$y, pch = 19, col = "orange")
points(X3$x, X3$y, pch = 19, col = "darkgreen")
data <- rbind(X1, X2, X3); data$class <- factor(data$class)

# Rysunek dla LDA
with(data, drawparti(class, x, y, xlab = "X", ylab = "Y", font = 2))

# Rysunek dla QDA
with(data, drawparti(class, x, y, method = "qda", xlab = "X", ylab = "Y", font = 2))

#PD zrobi� to samo co w przyppadku 2.2 - 2.4 ale w przypadku naiwnego Bayesa