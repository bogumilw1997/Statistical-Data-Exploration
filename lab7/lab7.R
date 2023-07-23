library(e1071)
# Dane
x <- c(1.0, 1.5, 1.0, 1.5, 2.5, 3.5, 2.0, 3.0, 3.5, 4.5, 5.0, 5.0)
y <- c(2.0, 1.0, 4.0, 3.0, 2.0, 2.0, 5.5, 5.0, 4.0, 4.5, 2.0, 3.5)

cl <- c(1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1)

data <- data.frame(x = x, y = y, class = as.factor(cl))

# Rysowanie danych
plot(data[,1:2], pch = 19, cex = 1.5, col = c("blue", "red")[data$class], xlim = c(0, 6), ylim = c(0, 6), xlab = "X", ylab = "Y", asp = 1)

# Wywo³anie metody SVM
data.svm <- svm(class ~ x + y, type = "C-classification", data = data, cost = 10, scale = F, kernel = "linear")

# Wektory noœne
print(data.svm$SV)

# Wartoœci wspó³czynników alfa
print(data.svm$coefs)

# Rozpinanie siatki
xp <- seq(-1, 6, 0.01)
yp <- seq(-1, 6, 0.01)

gr <- expand.grid(x = xp, y = yp)

# Predykcja klas na siatce
M <- matrix(as.numeric(predict(data.svm, gr)), length(xp))

# Rysowanie obszarów przynale¿noœci do klas
image(xp, yp, M, col = c("lightgreen", "lightblue"), xlim = c(0, 6), ylim = c(0, 6), xlab = "X", ylab = "Y")

# Dodanie punktów
points(data[,1:2], pch = 19, cex = 1.5, col = c("blue", "red")[data$class])

# Wyznaczenie wektora w
w <- t(data.svm$SV) %*% data.svm$coefs
b <- data.svm$rho

xx <- 0:10

# Hiperp³aszczyzna rodzielaj¹ca
lines(xx, -w[1] / w[2] * xx + b / w[2], lty = 2, lwd = 2)

# Hiperp³aszczyzny marginesów
lines(xx, -w[1] / w[2] * xx + (b + 1) / w[2], lty = 2, lwd = 2, col = "red")
lines(xx, -w[1] / w[2] * xx + (b - 1) / w[2], lty = 2, lwd = 2, col = "blue")


library(MASS)

# Generowanie
draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

# Rysowanie punktów
plot.data <- function(data) {
  
  cols <- c("blue", "red")
  
  plot(data[,1:2], col = cols[data$class], cex = 2.5, pch = 19)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.8, col = "white", font = 2)
  
}

# Parametry danych z rozk³adu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Ustawienie ziarna dla losowania danych
set.seed(128)

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Rysowanie danych
plot.data(data)

# Wywo³anie metody SVM
data.svm <- svm(class ~ x + y, type = "C-classification", data = data, cost = 10, scale = F, kernel = "linear")

# Wypisywanie wektorów noœnych i wartoœci alfa
print(with(data.svm, cbind(SV, coefs)))

# Wyznaczenie wektora w w <- t(data.svm$SV) %*% data.svm$coefs
b <- data.svm$rho

xx <- -5:5
# Hiperp³aszczyzna rodzielaj¹ca
lines(xx, -w[1] / w[2] * xx + b / w[2], lty = 2, lwd = 2)

# Hiperp³aszczyzny marginesów
lines(xx, -w[1] / w[2] * xx + (b + 1) / w[2], lty = 2, lwd = 2, col = "blue")
lines(xx, -w[1] / w[2] * xx + (b - 1) / w[2], lty = 2, lwd = 2, col = "red")


# J¹dro funkcji
K <- function(xi, xj, gamma) {
  exp(-gamma*(sum((xi - xj)^2)))
}

# "Wnêtrze" funkcji Lagrange'a
f <- function(xi, x, y, gamma) {
  
  l <- length(xi$x)
  
  sum(sapply(1:l, function(i) xi$alfa[i] * K(xi[i,c(1,2)], c(x,y), gamma)))
}

# Sta³a C oraz parametr gamma
C <- 1
gamma <- 2

set.seed(129)

# Generowanie rozk³adu
data.radial <- data.frame(x = runif(20, -1.5, 1.5), y = runif(20, -1.5, 1.5))

data.radial$class <- with(data.radial, ifelse(sqrt(x^2+y^2) < 1, "klasa1", "klasa2"))
kolor <- ifelse(data.radial$class == "klasa1", "red", "blue")
data.radial$class <- as.factor(data.radial$class)

data.radial.svm <- svm(class ~ x + y, type = "C-classification", data = data.radial, cost = C, gamma = gamma, scale = F)

data.radial$alfa <- 0
data.radial$alfa[data.radial.svm$index] <- data.radial.svm$coefs

# Punkty o wspó³czynnikach |alfa| > 0
data.radial1 <- data.radial[abs(data.radial$alfa) > 0.001,]

# Rysowanie punktów
plot(data.radial[,1:2], col = kolor, cex = 2.5, pch = 19, xlim = c(-2,2), ylim = c(-2,2))
text(data.radial[,1:2], labels = 1:nrow(data.radial), cex = 0.8, col = "white", font = 2)

print(data.radial1)

# Wyznaczanie wspó³czynnika b
b <- data.radial.svm$rho

# Rozpinanie siatki
xp <- seq(-2, 2, 0.05)
yp <- seq(-2, 2, 0.05)

gr <- expand.grid(x = xp, y = yp)

# Predykcja klas na siatce
M <- sapply(1:nrow(gr), function(i) f(data.radial1, gr$x[i], gr$y[i], gamma))
M <- matrix(M, length(xp))
Mx <- matrix(as.numeric(predict(data.radial.svm, gr)), length(xp))

# Rysowanie obszarów przynale¿noœci do klas
image(xp, yp, Mx, col = c("lightgreen", "lightblue"))

# Hiperp³aszczyzna rodzielaj¹ca
contour(xp, yp, M, add = T, levels = b, lwd = 2, lty = 2, labels = "")

# Hiperp³aszczyzny marginesów contour(xp, yp, M, add = T, levels = b+1, lwd = 2, col = ifelse(b < 0, "blue", "red"), labels = "")
contour(xp, yp, M, add = T, levels = b-1, lwd = 2, col = ifelse(b < 0, "red", "blue"), labels = "")
contour(xp, yp, M, add = T, levels = b+1, lwd = 2, col = ifelse(b < 0, "blue", "red"), labels = "")

# Dodanie punktów
points(data.radial[,1:2], col = kolor, cex = 2.5, pch = 19)
text(data.radial[,1:2], labels = 1:nrow(data.radial), cex = 0.8, col = "white", font = 2)
