library(MASS)
library(klaR)
library(e1071)

df <- read.table("wine.data",sep=",", header = F)
colnames(df) <- c("class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids","Nonflavanoid.phenols","Proanthocyanins", "Color.intensity", "Hue", "OD280.OD315", "Proline")
df_cut <- df[df$class != 3, ]
df_cut$class <- factor(df_cut$class)

class.lda <- lda(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315 + Proline, df_cut)
class.qda <- qda(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315 + Proline, df_cut)
class.nb <- naiveBayes(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315 + Proline, df_cut)

CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skuteczno�� klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Warto�ci true positive i true negative
  # zak�adamy, �e klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  sums <- apply(CM, 1, sum)
  
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, TPR = round(TPR, 4), FPR = round(FPR, 4), row.names = NULL))
}

# Powt�rne podstawienie
data.lda.old <- predict(class.lda, df_cut)
data.qda.old <- predict(class.qda, df_cut)
data.nb.old <- predict(class.nb, df_cut)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
res.old <- CM.large(df_cut$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(df_cut$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(df_cut$class, data.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB")

res.full <- res.old

class.lda <- lda(class ~ Alcohol + Malic.acid, df_cut)
class.qda <- qda(class ~ Alcohol + Malic.acid, df_cut)
class.nb <- naiveBayes(class ~ Alcohol + Malic.acid, df_cut)

# Powt�rne podstawienie
data.lda.old <- predict(class.lda, df_cut)
data.qda.old <- predict(class.qda, df_cut)
data.nb.old <- predict(class.nb, df_cut)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
res.old <- CM.large(df_cut$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(df_cut$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(df_cut$class, data.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB")

res2 <- res.old[,1]

class.lda <- lda(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium, df_cut)
class.qda <- qda(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium, df_cut)
class.nb <- naiveBayes(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium, df_cut)

# Powt�rne podstawienie
data.lda.old <- predict(class.lda, df_cut)
data.qda.old <- predict(class.qda, df_cut)
data.nb.old <- predict(class.nb, df_cut)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
res.old <- CM.large(df_cut$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(df_cut$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(df_cut$class, data.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB")

res5 <- res.old[,1]

class.lda <- lda(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity, df_cut)
class.qda <- qda(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity, df_cut)
class.nb <- naiveBayes(class ~ Alcohol + Malic.acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity, df_cut)

# Powt�rne podstawienie
data.lda.old <- predict(class.lda, df_cut)
data.qda.old <- predict(class.qda, df_cut)
data.nb.old <- predict(class.nb, df_cut)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
res.old <- CM.large(df_cut$class, data.lda.old$class)
res.old <- rbind(res.old, CM.large(df_cut$class, data.qda.old$class))
res.old <- rbind(res.old, CM.large(df_cut$class, data.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB")

res10 <- res.old[,1]

N <- nrow(df_cut)

# Dane przetasowane
df_cut.rnd <- df_cut[sample(1:N),1:3]

pu <- df_cut.rnd[1:floor(N*0.5), ]
pw <- df_cut.rnd[ceiling(N*0.5):floor(N*0.75), ]
pt <- df_cut.rnd[ceiling(N*0.75):N, ]

class.lda <- lda(class ~ Alcohol + Malic.acid, pu)
class.qda <- qda(class ~ Alcohol + Malic.acid, pu)
class.nb <- naiveBayes(class ~ Alcohol + Malic.acid, pu)

# Podstawienie setu waliduj�cego
data.lda.pw <- predict(class.lda, pw)
data.qda.pw <- predict(class.qda, pw)
data.nb.pw <- predict(class.nb, pw)

# G��wne warto�ci z macierzy pomy�ek dla setu waliduj�cego
res.pw <- CM.large(pw$class, data.lda.pw$class)
res.pw <- rbind(res.pw, CM.large(pw$class, data.qda.pw$class))
res.pw <- rbind(res.pw, CM.large(pw$class, data.nb.pw))
rownames(res.pw) <- c("LDA", "QDA", "NB")

respw <- res.pw[,1]

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
  
  res
}

# G��wna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudopr�b) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- lda(class ~ ., data = learn)
  test.pred <- predict(learn.classifier, newdata = test)
  
  # Macierz pomy�ek
  CM <- table(test$class, test.pred$class)
  
  # Liczba b��d�w
  sum(CM) - sum(diag(CM))
}

k = 10
acc.xval <- 1-sum(CV(df_cut.rnd, k))/N

cat("Parametry klasyfikator�w LDA, QDA i NB na pe�nym zbiorze danych:\n"); res.full
cat("Skuteczno�� klasyfikator�w LDA, QDA i NB dla 2 pierwszych sk�adowych:\n"); res2
cat("Skuteczno�� klasyfikator�w LDA, QDA i NB dla 5 pierwszych sk�adowych:\n"); res5
cat("Skuteczno�� klasyfikator�w LDA, QDA i NB dla 10 pierwszych sk�adowych:\n"); res10
cat("Skuteczno�� klasyfikator�w LDA, QDA i NB dla 2 pierwszych sk�adowych dla danych walidacyjnych:\n"); respw
cat('Skuteczno�� LDA dla 2 pierwszych sk�adowych przy',k,"- krotnej kroswalidacji:", round(acc.xval, 4))