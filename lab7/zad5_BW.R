library(MASS)
library(e1071)

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

find.acc.svm <- function(model, data) {
  
  predict.class <- predict(model, data)
    
  CM <- table(data$class, predict.class)
  
  N <- sum(CM)
  ACC <- sum(diag(CM))/N
  
  return(ACC)
}


find.acc.lda <- function(model, data) {
  
  predict.class <- predict(model, data)$class
  
  CM <- table(data$class, predict.class)
  
  N <- sum(CM)
  ACC <- sum(diag(CM))/N
  
  return(ACC)
}

model.lda <- lda(class ~ x + y, data = data)

acc.lda <- find.acc.lda(model.lda, data)

c.vec <- seq(0.01, 0.1, 0.002)

acc.vec <- c()

for (c in c.vec){
  
  model.svm <- svm(class ~ x + y, type = "C-classification", data = data, cost = c, scale = F, kernel = "linear")
  acc.vec <- append(acc.vec,find.acc.svm(model.svm, data))
}

plot(c.vec, acc.vec, xlab = 'C', ylab = 'skutecznoœæ', main = 'Porównanie skutecznoœci metody SVM z LDA',pch = 19, col = 'black')
lines(c.vec, rep(acc.lda, length(c.vec)), col = 'red', lty = 1)
legend(0.08, 0.88, legend=c("SVM", "LDA"),
       col=c("black", "red"),lty=c(NA,1),pch=c(19,NA), cex=0.8)
