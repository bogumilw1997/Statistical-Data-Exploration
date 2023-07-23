library(MASS)
library(rpart)
library(rpart.plot)

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
  
  cols <- c("blue", "orange")
  
  plot(data[,1:2], col = cols[data$class], cex = 2)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.6)
  
}

# Parametry danych z rozk³adu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 60
n2 <- 40

# Ustawienie ziarna dla losowania danych
set.seed(1280)

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Rysowanie danych
plot.data(data)

tree <- rpart(class ~ y + x, data, minsplit = 1, minbucket = 1, cp = 0)
# Rysowanie drzewa
rpart.plot(tree, type = 1, extra = 1)

printcp(tree)
plotcp(tree)

# Przycinanie drzewa
tree1 <- prune(tree, cp = 0.094)

# Nowe dane
newdata <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Macierz pomy³ek dla pe³nego drzewa
table(newdata$class, predict(tree, newdata, type = "class"))

# Macierz pomy³ek dla drzewa przyciêtego
table(newdata$class, predict(tree1, newdata, type = "class"))

# Rysowanie drzewa

rpart.plot(tree1, type = 1, extra = 1)

# Uczenie drzewa
tree <- rpart(Species ~ ., iris, minsplit = 0, cp = 0)

# Rysowanie
rpart.plot(tree, type = 1, extra = 1)
