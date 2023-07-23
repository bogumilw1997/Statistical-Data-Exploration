# Zale¿noœæ y = x
x <- seq(-5, 5, by=.1)
y <- x

# Dodanie szumu
eta <- runif(101, 0, 1)
dzeta <- runif(101, 0, 1)

x <- x + eta
y <- y + dzeta

# Wykres
plot(x, y, pch=19, xlab="Test 1", ylab="Test 2", font=2, font.lab=2, xlim=c(-5,5), ylim=c(-5,5), asp = 1)
abline(h=0, v=0, lwd=2, col="gray") 
abline(0,1,lwd=2,col="red")
abline(0,-1,lwd=2,col="green")
text(4.5,-0.5,expression(x[1]),cex=2)
text(-0.5,4.5,expression(x[2]),cex=2)
text(4.7,4,expression(y[1]),cex=2, col="red")
text(-4.5,4,expression(y[2]),cex=2, col="green") 
title("Dane", cex.main=1.4)

# Zapisanie wspolrzednych x i y
# do struktury dataframe

test <- data.frame(x, y)

# Macierz kowariancji
S <- cov(test)

# Macierz korelacji
Sc <- cor(test)

# Wyznaczenie wartoœci i wektorów w³asnych
eS <- eigen(S)
eSc <- eigen(Sc)

print(eS)
print(eSc)

# Wykonanie analizy sk³adowych glownych
test.pc <- princomp(~., cor=T, data=test)

# Wykreœlenie wariancji zwiaz¹nych ze sk³adowymi
plot(test.pc, main="")
title("Wariancja", cex.main=1.4)

print(test.pc$sdev^2)

# Wykres we wspó³rzêdnych sk³adowych g³ównych
plot(test.pc$scores, xlim=c(-2,2), ylim=c(-2,2), xlab="Sk³adowa 1", ylab="Sk³adowa 2")
title("Sk³adowe", cex.main=1.4)

# Dane irysów
data(iris)

# Liniowe PCA
iris.pca <- princomp(~ ., data = iris[,-5], cor = TRUE)
plot(iris.pca$scores, pch = 19, col = as.factor(iris[,5]), xlab="Sk³adowa 1", ylab="Sk³adowa 2")

library(kernlab)

# PCA nieliniowe z j¹drem radialnym
iris.kpca <- kpca(~ ., data=iris[,-5], kernel="rbfdot", kpar = list(sigma=0.1))
plot(rotated(iris.kpca), pch = 19, col = as.factor(iris[,5]), xlab = "Sk³adowa 1", ylab = "Sk³adowa 2")

# MDS dla miast Europy
euro.cmd = cmdscale(eurodist, k = 2)

# Tworzenie rysunku
plot(euro.cmd[,1], euro.cmd[,2], type = "n", xlab = "", ylab = "", axes = FALSE, main = "MDS Europa")
text(euro.cmd[,1], euro.cmd[,2], labels(eurodist), cex = 0.9, xpd = TRUE)

# Dane dotycz¹ce g³osowañ na prezydenta w stanach USA
votes.repub <- cluster::votes.repub

# Pozbywamy siê wartoœci NA
dates <- c(1:15)
ind <- apply(is.na(votes.repub[,-dates]), 1, any)
votes <- votes.repub[!ind, -dates]

votes.cmd <- cmdscale(dist(votes))

plot(votes.cmd, type = "n", xlab = "", ylab = "", axes = FALSE, main = "Stany USA w wyborach prezydenckich")
text(votes.cmd, labels = rownames(votes.cmd), cex = 0.9, xpd = TRUE)
