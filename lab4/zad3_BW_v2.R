library(MASS)
library(class)
library(ggplot2)

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

k.vec <- seq(1, 21, 1)

acc.vec <- rep(0, 21)
acc.sd.vec <- rep(0, 21)

TP.vec <- rep(0, 21)
TP.sd.vec <- rep(0, 21)

TN.vec <- rep(0, 21)
TN.sd.vec <- rep(0, 21)

for (val in 1:10){
  
  acc <- c()
  TP <- c()
  TN <- c()
  
  data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)
  
  for (k in k.vec){
    
    class.knn <- knn(data[,1:2], data[,1:2], data$class, k)
    CM <- table(class.knn, data$class)
    
    TP <- append(TP, CM[1,1])
    TN <- append(TN, CM[2,2])
    ACC <- sum(diag(CM)) / sum(CM)
    acc <- append(acc, ACC)
  }
  
  acc.vec[val] <- mean(acc)
  acc.sd.vec[val] <- sd(acc)
  
  TP.vec[val] <- mean(TP)
  TP.sd.vec[val] <- sd(TP)
  
  TN.vec[val] <- mean(TN)
  TN.sd.vec[val] <- sd(TN)
}

par(mfrow=c(3,1))
plot(k.vec, acc.total/10, xlab = 'k-nn', ylab = 'ACC', col= 'black', pch=19, main = 'Zale¿noœæ ACC/TP/TN od iloœci NN uœrednione po 10 symulacjach')
plot(k.vec, TP.total/10, col= 'blue', pch=19, xlab = 'k-nn', ylab = 'TP')
plot(k.vec, TN.total/10, col= 'red', pch=19, xlab = 'k-nn', ylab = 'TN')

acc.total <- rep(0, 21)
TP.total <- rep(0, 21)
TN.total <- rep(0, 21)

for (val in 1:10){
  
  acc.vec <- c()
  TP <- c()
  TN <- c()
  
  test.set <- draw.data.gauss(S1, S2, m1, m2, 10, 5)
  
  for (k in k.vec){
    
    class.knn <- knn(data[,1:2], test.set[,1:2], data$class, k)
    CM <- table(class.knn, test.set$class)
    
    TP <- append(TP, CM[1,1])
    TN <- append(TN, CM[2,2])
    ACC <- sum(diag(CM)) / sum(CM)
    acc.vec <- append(acc.vec, ACC)
  }
  
  acc.total <- acc.total + acc.vec
  TP.total <- TP.total + TP
  TN.total <- TN.total + TN
}

par(mfrow=c(3,1))
plot(k.vec, acc.total/10, xlab = 'k-nn', ylab = 'ACC', col= 'black', pch=19, main = 'Zale¿noœæ ACC/TP/TN od NN uœredn. po 10 sym. z losowym setem testowym')
plot(k.vec, TP.total/10, col= 'blue', pch=19, xlab = 'k-nn', ylab = 'TP')
plot(k.vec, TN.total/10, col= 'red', pch=19, xlab = 'k-nn', ylab = 'TN')