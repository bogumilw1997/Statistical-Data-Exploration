library(MASS)
library(klaR)
library(e1071)

draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

# Parametry danych z rozkladu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 80
n2 <- 60

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Trenowanie klasyfikatorów na PU
class.lda <- lda(class ~ x + y, data)
class.qda <- qda(class ~ x + y, data)
class.nb <- naiveBayes(class ~ x + y, data)

CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skutecznoœæ klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Wartoœci true positive i true negative
  # zak³adamy, ¿e klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  sums <- apply(CM, 1, sum)
  
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, TPR = round(TPR, 4), FPR = round(FPR, 4), row.names = NULL))
}

# Powtórne podstawienie
data.lda.old <- predict(class.lda, data)
data.qda.old <- predict(class.qda, data)
data.nb.old <- predict(class.nb, data)

# G³ówne wartoœci z macierzy pomy³ek dla powtórnego podstawienia
res.old <- CM.large(data$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(data$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(data$class, data.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB")

val <- draw.data.gauss(S1, S2, m1, m2, 30, 30)

# Predykcja na zbiorze walidacyjnym
data.lda.val <- predict(class.lda, val)
data.qda.val <- predict(class.qda, val)
data.nb.val <- predict(class.nb, val)
data.nb.val.p <- predict(class.nb, val, type = "raw")

res.val <- CM.large(val$class, data.lda.val$class)
res.val <- rbind(res.val, CM.large(val$class, data.qda.val$class))
res.val <- rbind(res.val, CM.large(val$class, data.nb.val))
rownames(res.val) <- c("LDA", "QDA", "NB")

CM.values <- function(org.class, pred.posterior, threshold) {
  # Funkcja do wyznaczania wartoœci TPR (true positive rate) i FPR (false positive rate)
  # jej argumenty to oryginalne klasy, wartosci prawdopodobienstw a posteriori dla klasyfikatora
  # oraz wektor progow
  
  # Korzystamy z prawdopodobieñstw a posteriori
  pred.class <- factor(ifelse(pred.posterior >= threshold, 2, 1))
  
  # Wartoœci true positive i true negative
  # zak³adamy, ¿e klasa "2" jest "pozytywna"
  TP <- sum(pred.class == 2 & org.class == 2)
  TN <- sum(pred.class == 1 & org.class == 1)
  
  # Licznoœæ oryginalnych klas
  sum.neg <- sum(org.class == 1)
  sum.pos <- sum(org.class == 2)
  
  TPR <- TP / sum.pos
  FPR <- 1 - TN / sum.neg
  
  return(c(FPR,TPR))
}

get.roc.values <- function(class, posterior) {
  
  progi <- c(-Inf, sort(unique(posterior)), +Inf)
  
  z <- t(sapply(1:length(progi), function(i) CM.values(class, posterior, progi[i])))
  
  return(z)
}

# Wartoœci FPR i TPR dla metody LDA
# w przypadku PU i PW
roc.lda.old <- get.roc.values(data$class, data.lda.old$posterior[,2])
roc.lda.val <- get.roc.values(val$class, data.lda.val$posterior[,2])

# Porównanie LDA
plot(roc.lda.old, t="l", xlab = "FPR", ylab = "TPR", asp = 1)
abline(0, 1, col = "gray")
lines(roc.lda.val, col = "red")
legend(0.9, 0.2, c("PU", "PW"), col = c("black", "red"), text.col = c("black", "red"), lty = 1, title = "Metoda LDA")
points(res.old[1,5], res.old[1,4], pch = 19, cex = 0.5)
points(res.val[1,5], res.val[1,4], pch = 19, cex = 0.5, col = "red")

roc.lda.val <- get.roc.values(val$class, data.lda.val$posterior[,2])
roc.qda.val <- get.roc.values(val$class, data.qda.val$posterior[,2])
roc.nb.val <- get.roc.values(val$class, data.nb.val.p[,2])

plot(roc.lda.val, t="l", xlab = "FPR", ylab = "TPR", asp = 1)
abline(0, 1, col = "gray")
lines(roc.qda.val, col = "red")
lines(roc.nb.val, col = "blue")
legend(0.7, 0.3, c("LDA", "QDA", "NB"), col = c("black", "red", "blue"), text.col = c("black", "red", "blue"), lty = 1, title = "Próba walidacyjna")

CV <- function(data, K) {
  
  N <- nrow(data)
  
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudoprób
  sets <- sapply(1:K, function(i) ((i-1) * (N/K) + 1):(i * (N/K)))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla ka¿dej pseudopróby wyznaczamy liczbê pomy³ek
  res <- t(sapply(1:K, function(k) CV.main(data.rnd[-c(sets[,k]),], data.rnd[sets[,k],])))
  
  res
}

# G³ówna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudoprób) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- lda(class ~ ., data = learn)
  test.pred <- predict(learn.classifier, newdata = test)
  
  # Macierz pomy³ek
  CM <- table(test$class, test.pred$class)
  
  # Liczba b³êdów
  sum(CM) - sum(diag(CM))
}

N <- nrow(data)

# Dzielniki N
div <- which(!(N %% 1:N))

# Wykonanie wielokrotnej CV dla ró¿nych dzielników
mat <- sapply(div[-1], function(d) replicate(10, sum(CV(data, d)) / N))

# Wyznaczanie statystyk
cv.res <- as.data.frame(t(apply(mat, 2, function(x) c(mean(x), sd(x)))))
colnames(cv.res) <- c("mean", "sd")
cv.res$K <- div[-1]

library(Hmisc)

with(cv.res, errbar(K, mean, mean + sd, mean - sd))