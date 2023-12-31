library(MASS)
library(rpart)
library(rpart.plot)

df <- read.table("wine.data",sep=",", header = F)

colnames(df) <- c("class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids",
                  "Nonflavanoid.phenols","Proanthocyanins", "Color.intensity", "Hue", "OD280.OD315", "Proline")

df$class <- factor(df$class)
N <- nrow(df)

df <- df[sample(N),]

tree <- rpart(class ~ ., df, minsplit = 1, minbucket = 1, cp = 0)
# Rysowanie drzewa
rpart.plot(tree, type = 1, extra = 1, main = 'Pe�ne drzewo dla klasyfikatora u�ywaj�cego wszystkich danych')

find.acc <- function(tree, data) {
  
  predict.class <- predict(tree, newdata = data, type = "class")
  
  CM <- table(data$class, predict.class)
  
  ACC <- sum(diag(CM))/N
  
  return(ACC)
}

cat("Skuteczno�� pe�nego drzewa przy powt�rnym podstawieniu:", find.acc(tree, df))

CV <- function(data, K) {
  
  N <- nrow(data)
  
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudopr�b
  sets <- sapply(1:K, function(i) ((i-1) * (N/K) + 1):(i * (N/K)))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla ka�dej pseudopr�by wyznaczamy liczb� pomy�ek
  res <- t(sapply(1:K, function(k) CV.main(data.rnd[-c(sets[,k]),], data.rnd[sets[,k],])))
  
  #return(1 - mean(res))
  return(1 - mean(res)/N)
}

# G��wna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudopr�b) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- rpart(class ~ ., learn, minsplit = 1, minbucket = 1, cp = 0)
  test.pred <- predict(learn.classifier, newdata = test, type = "class")
  
  # Macierz pomy�ek
  CM <- table(test$class, test.pred)
  
  # Liczba b��d�w
  sum(CM) - sum(diag(CM))
}

ACC <- CV(df,10)

cat("Skuteczno�� pe�nego drzewa przy 10-fold Xval:", ACC)

find.optimal.cp <- function(tree) {
  
  cp.vec <- tree$cptable[,1]
  err.vec <- tree$cptable[,4]
  std.vec <- tree$cptable[,5]
  
  min.error.val <- min(err.vec)
  min.error.indx <- which.min(err.vec)
  min.error.std <- std.vec[min.error.indx]
  
  optimal.cp <- max(cp.vec[err.vec < (min.error.val + min.error.std)]) 
  
  return(optimal.cp)
}

optimal.cp <- find.optimal.cp(tree)

tree.pruned <- prune(tree, cp = optimal.cp)
rpart.plot(tree.pruned, type = 1, extra = 1, main = 'Przyci�te drzewo dla klasyfikatora u�ywaj�cego wszystkich danych')

cat("Skuteczno�� drzewa przyci�tego przy powt�rnym podstawieniu:", find.acc(tree.pruned, df))

k.vec <- seq(2, ncol(df))
acc.vec <- rep(0, length(k.vec))
full.tree.size <- rep(0, length(k.vec))
pruned.tree.size <- rep(0, length(k.vec))

#quit(save="ask")
for (k in k.vec){
  
  cut.df <- df[, 1:k]
  
  tree <- rpart(class ~ ., cut.df, minsplit = 1, minbucket = 1, cp = 0)
  
  full.tree.size[k-1] <- tail(tree$cptable[,"nsplit"], 1) + 1
  
  optimal.cp <- find.optimal.cp(tree)
  tree.pruned <- prune(tree, cp = optimal.cp)
  
  pruned.tree.size[k-1] <- tail(tree.pruned$cptable[,"nsplit"], 1) + 1
  
  acc.vec[k-1] <- find.acc(tree.pruned, cut.df)
}

plot(k.vec, acc.vec, main = 'Zale�no�� skuteczno�ci drzewa od ilo�ci zmiennych obja�niaj�cych', 
     xlab = 'ilo�� zmiennych', ylab = 'skuteczno��', pch = 16)

plot(k.vec, full.tree.size - pruned.tree.size, main = 'R�nica mi�dzy rozmiarem drzewa pe�nego a optymalnego', 
     xlab = 'ilo�� zmiennych', ylab = 'r�nica rozmiaru', pch = 16)
