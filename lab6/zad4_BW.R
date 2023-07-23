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
rpart.plot(tree, type = 1, extra = 1, main = 'Pe³ne drzewo dla klasyfikatora u¿ywaj¹cego wszystkich danych')

find.acc <- function(tree, data) {
  
  predict.class <- predict(tree, newdata = data, type = "class")
  
  CM <- table(data$class, predict.class)
  
  ACC <- sum(diag(CM))/N
  
  return(ACC)
}

cat("Skutecznoœæ pe³nego drzewa przy powtórnym podstawieniu:", find.acc(tree, df))

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
  
  #return(1 - mean(res))
  return(1 - mean(res)/N)
}

# G³ówna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudoprób) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- rpart(class ~ ., learn, minsplit = 1, minbucket = 1, cp = 0)
  test.pred <- predict(learn.classifier, newdata = test, type = "class")
  
  # Macierz pomy³ek
  CM <- table(test$class, test.pred)
  
  # Liczba b³êdów
  sum(CM) - sum(diag(CM))
}

ACC <- CV(df,10)

cat("Skutecznoœæ pe³nego drzewa przy 10-fold Xval:", ACC)

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
rpart.plot(tree.pruned, type = 1, extra = 1, main = 'Przyciête drzewo dla klasyfikatora u¿ywaj¹cego wszystkich danych')

cat("Skutecznoœæ drzewa przyciêtego przy powtórnym podstawieniu:", find.acc(tree.pruned, df))

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

plot(k.vec, acc.vec, main = 'Zale¿noœæ skutecznoœci drzewa od iloœci zmiennych objaœniaj¹cych', 
     xlab = 'iloœæ zmiennych', ylab = 'skutecznoœæ', pch = 16)

plot(k.vec, full.tree.size - pruned.tree.size, main = 'Ró¿nica miêdzy rozmiarem drzewa pe³nego a optymalnego', 
     xlab = 'iloœæ zmiennych', ylab = 'ró¿nica rozmiaru', pch = 16)
