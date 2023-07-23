library(MASS)

#Pamietac o tym zeby klasy by³y jako factor nie INT!!!!

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

n1 <- 30
n2 <- 20

# Ustawienie ziarna dla losowania danych
set.seed(128)

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Rysowanie danych
plot.data(data)

# Funkcja do wspó³czynnika Gini'ego
gini <- function(tab) 2 * tab[1] * tab[2]

# Normalizacja prostego histogramu
norm.tab <- function(tab, tab.s) if(tab.s) { tab / tab.s } else { tab }

# Funkcja do wyznaczania ró¿norodnoœci
get.Q <- function(data, name, threshold) {
  
  # Rozk³ad klas w oryginalnej probie
  tab.all <- table(data$class)
  tab.all.s <- sum(tab.all)
  
  # Rozk³ad klas dla warunku tj. po "lewej" stronie przedzialu
  tab.left <- table(data$class[data[,name] <= threshold])
  tab.left.s <- sum(tab.left)
  
  # Rozk³ad klas po "prawej" stronie
  tab.right <- tab.all - tab.left
  tab.right.s <- tab.all.s - tab.left.s
  
  # Normalizacja rozk³adów
  tab.all <- norm.tab(tab.all, tab.all.s)
  tab.left <- norm.tab(tab.left, tab.left.s)
  tab.right <- norm.tab(tab.right, tab.right.s)
  
  # Wyznaczanie wspó³czynników Gini'ego
  # czyli ró¿norodnoœci w wêz³ach
  Q.all <- gini(tab.all)
  Q.left <- gini(tab.left)
  Q.right <- gini(tab.right)
  
  # U³amki elementów w wêz³ach
  p.left <- tab.left.s / tab.all.s
  p.right <- 1 - p.left
  
  # Ca³kowita ró¿norodnoœæ w wêz³ach-dzieciach
  Q.children <- p.left * Q.left + p.right * Q.right
  
  # Zwracamy ró¿nicê ró¿norodnoœci
  return(Q.all - Q.children)
}

# Tworzenie wektorów podzia³ów
threshold.x <- sort(data$x)
threshold.y <- sort(data$y)

# Obliczanie ró¿nic ró¿norodnoœci
# dla podzia³ów na argumencie x oraz y
Q.x <- sapply(threshold.x, function(t) get.Q(data, "x", t))
Q.y <- sapply(threshold.y, function(t) get.Q(data, "y", t))

# Wartoœæ ró¿norodnoœci w funkcji progu podzia³u
plot(threshold.x, Q.x, t = "o", pch = 19, xlab = "prog podzialu (wartosc X lub Y)", ylab = "wartosc Q")
points(threshold.y, Q.y, t = "o", pch = 19, col = "blue")

threshold.x.max <- (threshold.x[which.max(Q.x)] + threshold.x[which.max(Q.x) + 1]) / 2

abline(v = threshold.x.max, lty = 2, col = "red")

plot.data(data)
rect(-10, -10, threshold.x.max, 10, col = rgb(0, 0, 1, 0.2), border = NA)

split.data <- function(data) {
  
  threshold.x <- sort(data$x)
  threshold.y <- sort(data$y)
  
  Q.x <- sapply(threshold.x, function(t) get.Q(data, "x", t))
  Q.y <- sapply(threshold.y, function(t) get.Q(data, "y", t))
  
  if(max(Q.x) > max(Q.y)) {
    
    threshold.x.max <- (threshold.x[which.max(Q.x)] + threshold.x[which.max(Q.x) + 1]) / 2
    
    data.left <- data[data$x <= threshold.x.max,]
    data.right <- data[data$x > threshold.x.max,]
    
    return(list(var = "x", var.val = threshold.x.max, data.left = data.left, data.right = data.right))
  } else {
    
    threshold.y.max <- (threshold.y[which.max(Q.y)] + threshold.y[which.max(Q.y) + 1]) / 2
    
    data.left <- data[data$y <= threshold.y.max,]
    data.right <- data[data$y > threshold.y.max,]
    
    return(list(var = "y", var.val = threshold.y.max, data.left = data.left, data.right = data.right))
  }
}

blue.op <- rgb(0, 0, 1, 0.2)
orange.op <- rgb(1, 0.66, 0, 0.2)

plot.data(data)
  
s0 <- split.data(data)
rect(-10, -10, s0$var.val, 10, col = blue.op)

s1.r <- split.data(s0$data.right)
rect(s0$var.val, s1.r$var.val, 10, 10, col = orange.op)

s2.r <- split.data(s1.r$data.right)
rect(s2.r$var.val, s1.r$var.val, 10, 10, col = orange.op) 
rect(s0$var.val, s1.r$var.val, s2.r$var.val, 10, col = orange.op)

s2.l <- split.data(s1.r$data.left)
rect(s2.l$var.val, -10, 10, s1.r$var.val, col = orange.op)
rect(s0$var.val, -10, s2.l$var.val, s1.r$var.val, col = blue.op)

# Rekurencyjna prymitywna funkcja implementuj¹ca drzewa
split.data <- function(data, sp) {
  
  tab <- table(data$class)
  
  if(gini(tab) < 1e-6) {
    
    cat(sp, "Lisc drzewa, klasa A: ", tab[1], "klasa B:", tab[2], "\n")
  } else {
    
    threshold.x <- sort(data$x)
    threshold.y <- sort(data$y)
    
    Q.x <- sapply(threshold.x, function(t) get.Q(data, "x", t))
    Q.y <- sapply(threshold.y, function(t) get.Q(data, "y", t))
    
    if(max(Q.x) > max(Q.y)) {
      
      threshold.x.max <- (threshold.x[which.max(Q.x)] + threshold.x[which.max(Q.x) + 1]) / 2
      
      data.left <- data[data$x <= threshold.x.max,]
      data.right <- data[data$x > threshold.x.max,]
      
      cat(sp, "Podzial na X", threshold.x.max, "\n")
    } else {
      
      threshold.y.max <- (threshold.y[which.max(Q.y)] + threshold.y[which.max(Q.y) + 1]) / 2
      
      data.left <- data[data$y <= threshold.y.max,]
      data.right <- data[data$y > threshold.y.max,]
      
      cat(sp, "Podzial na Y", threshold.y.max, "\n")
    }
    
    sp <- paste(sp," ")
    
    split.data(data.left, sp)
    split.data(data.right, sp)
  }
}

split.data(data, "")

library(rpart)

# Wyuczenie drzewa na danych
tree <- rpart(class ~ y + x, data, minsplit = 1, minbucket = 1)

# Rysowanie drzewa
plot(tree, uniform = TRUE)
text(tree, use.n = TRUE, all = TRUE, font = 2)

# Biblioteka rpart.plot
library(rpart.plot)

# Rysowanie drzewa
rpart.plot(tree, type = 1, extra = 1)

# Przewidywanie
tree.class <- predict(tree, newdata = data, type = "class")

# Porównanie z oryginalnymi klasami
table(data$class, tree.class)

# Domyœlne wywo³anie rpart
# minsplit = 20, minbucket = round(minsplit/3)
tree.def <- rpart(class ~ y + x, data)
rpart.plot(tree.def, type = 1, extra = 1)

# Porównanie z oryginalnymi klasami
tree.class <- predict(tree.def, newdata = data, type = "class")
table(data$class, tree.class)

# Generacja nowych danych
data.new <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Przewidywanie za pomoc¹ "przeuczonego" klasyfikatora
tree.class <- predict(tree, newdata = data.new, type = "class")
table(data.new$class, tree.class)

# Przewidywanie za pomoc¹ "przyciêtego" klasyfikatora
tree.class <- predict(tree.def, newdata = data.new, type = "class")
table(data.new$class, tree.class)
