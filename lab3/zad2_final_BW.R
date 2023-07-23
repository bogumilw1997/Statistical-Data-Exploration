library(MASS)
library(klaR)
library(e1071)

df <- read.table("wine.data",sep=",", header = F)
colnames(df) <- c("class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids",
                  "Nonflavanoid.phenols","Proanthocyanins", "Color.intensity", "Hue", "OD280.OD315", "Proline")

df$class <- factor(df$class)
N <- nrow(df)
df <- df[sample(N),]

CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skutecznoœæ klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  TP1 <- CM[1,1]
  TP2 <- CM[2,2]
  TP3 <- CM[3,3]
  
  FP1 <- CM[2,1] + CM[3,1]
  FP2 <- CM[1,2] + CM[3,2]
  FP3 <- CM[1,3] + CM[2,3]
  
  return(c(ACC = round(ACC,4),TP1 = TP1,TP2 = TP2,TP3 = TP3,FP1 = FP1,FP2 = FP2,FP3 = FP3, row.names = NULL))
}

parameters.func <- function(df) {
  
  class.lda <- lda(class ~ ., df)
  class.qda <- qda(class ~ ., df)
  class.nb <- naiveBayes(class ~ ., df)
  
  data.lda.old <- predict(class.lda, df)
  data.qda.old <- predict(class.qda, df)
  data.nb.old <- predict(class.nb, df)
  
  res.old <- CM.large(df$class, data.lda.old$class)
  res.old <- rbind(res.old, CM.large(df$class, data.qda.old$class))
  res.old <- rbind(res.old, CM.large(df$class, data.nb.old))
  rownames(res.old) <- c("LDA", "QDA", "NB")
  
  return(res.old)
}

params.table <- parameters.func(df)

cat('Parametry klasyfikatorów LDA, QDA i NB na pe³nym zbiorze danych (wszystkie sk³adowe obserwacji):\n'); params.table

params.table <- parameters.func(df[,1:3])
cat('Skutecznoœæ klasyfikatorów LDA, QDA i NB dla pierwszych 2 sk³adowych:\n'); params.table[,1]

params.table <- parameters.func(df[,1:5])
cat('Skutecznoœæ klasyfikatorów LDA, QDA i NB dla pierwszych 5 sk³adowych:\n'); params.table[,1]

params.table <- parameters.func(df[,1:11])
cat('Skutecznoœæ klasyfikatorów LDA, QDA i NB dla pierwszych 10 sk³adowych:\n'); params.table[,1]

pu <- df[1:floor(N*0.5), 1:3]
pw <- df[ceiling(N*0.5):floor(N*0.75), 1:3]
pt <- df[ceiling(N*0.75):N, 1:3]

# Uczenie na secie uczacym
class.lda <- lda(class ~ ., pu)
class.qda <- qda(class ~ ., pu)
class.nb <- naiveBayes(class ~ ., pu)

# Podstawienie setu walidacyjnego
data.lda.pw <- predict(class.lda, pw)
data.qda.pw <- predict(class.qda, pw)
data.nb.pw <- predict(class.nb, pw)

res.pw <- CM.large(pw$class, data.lda.pw$class)
res.pw <- rbind(res.pw, CM.large(pw$class, data.qda.pw$class))
res.pw <- rbind(res.pw, CM.large(pw$class, data.nb.pw))
rownames(res.pw) <- c("LDA", "QDA", "NB") 

classifier.names <- c("LDA", "QDA", "NB")

cat('Skutecznoœæ klasyfikatorów LDA, QDA i NB dla setu walidacyjnego:\n'); res.pw[,1]

best.classifier <- classifier.names[which.max(res.pw[,1])]

cat('Najwy¿sz¹ skutecznoœæ równ¹', max(res.pw[,1]) ,'dla setu walidacyjnego mia³ klasyfikator:', best.classifier)

if (best.classifier == 'LDA') {
  pred <- predict(class.lda, pt)$class
} else if (best.classifier == 'QDA') {
  pred <- predict(class.qda, pt)$class
} else if (best.classifier == 'NB') {
  pred <- predict(class.nb, pt)
} 

res.pt <- CM.large(pt$class, pred)

cat('Skutecznoœæ klasyfikatora', best.classifier, 'dla danych testowych wynosi:'); unname(res.pt[1])

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

k = 10
acc.xval <- 1-sum(CV(df[1:3], k))/N

cat('Skutecznoœæ klasyfikatora LDA dla 2 pierwszych sk³adowych przy',k,"- krotnej kroswalidacji:", round(acc.xval, 4))
