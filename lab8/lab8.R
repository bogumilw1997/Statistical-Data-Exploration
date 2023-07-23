library(MASS)
library(cluster)

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
  
  plot(data[,1:2], col = cols[data$class], cex = 3, pch = 19)
  text(data[,1:2], labels = 1:nrow(data), cex = 1, col = "white", font = 2)
  
}

# Parametry danych z rozk³adu Gaussa
S1 <- matrix(c(2, 0, 0, 2), 2, 2)
S2 <- matrix(c(2, 0, 0, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(1, 1)

n1 <- 10
n2 <- 10

# Ustawienie ziarna dla losowania danych
set.seed(1290)

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Definiowanie kolorów
blueop <- "#0000ff55"; orangeop <- "#ffa50055"; redop <- "#00ff0055"

# Algorytm k-œrednich dla K=2
par(mfrow = c(1,2))
plot.data(data)
title("Oryginalne dane")

km2 <- kmeans(data[,1:2], 2)
plot.data(data)
title("Oryginalne dane + efekty k-œrednich")
points(data[,1:2], pch = 19, cex = 4, col = c(blueop, orangeop)[km2$cluster])


# Algorytm k-œrednich dla K=3
plot.clust <- function(n, data, K) {
  
  km <- kmeans(data[,1:2], K)
  
  plot(data[,1:2], pch = 19, cex = 3, col = c("blue", "orange", "green")[km$cluster])
  text(data[,1:2], labels = 1:nrow(data), cex = 1, col = "white", font = 2)
  title(n)
  return(km$tot.withinss)
}

par(mfrow = c(3,3))
W <- sapply(1:9, function(n) plot.clust(n, data, 3))

print(W)

library(Hmisc)

par(mfrow=c(1,2))

# Macierz kowariancji
S <- matrix(c(1,0,0,1),2,2)
# Wartosci œrednie dla poszczególnych skupieñ
m1 <- c(4, 4)
m2 <- c(1, 1)
m3 <- c(4,-1)

kolory <- c(rep("orange", 30), rep("blue", 30), rep("green", 30))

# Losowanie punktów do klastrów
clust1 <- mvrnorm(30, m1, S)
clust2 <- mvrnorm(30, m2, S)
clust3 <- mvrnorm(30, m3, S)

# Zebranie punktów razem
all_points <- rbind(clust1, clust2, clust3)

# Wyznaczenie wartoœci min i max dla wspó³rzêdnych
xrange <- range(all_points[,1])
yrange <- range(all_points[,2])

xmin = xrange[1]; xmax = xrange[2]
ymin = yrange[1]; ymax = yrange[2]

# Rysowanie danych par(mfrow = c(1,2))
plot(all_points, col=kolory, pch=19, cex=2, xlab="X", ylab="Y", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
title("Klastry", cex.main=1.4, font=2)

# Wykonanie metody k-œrednich dla k=3
cl <- kmeans(all_points, 3)

# Nadrukowanie na klastrach przynale¿nosci do klas
# otrzymanej z metody k-œrednich
plot(all_points, col=kolory, pch=19, cex = 3, xlab="X", ylab="Y", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
text(all_points[,1], all_points[,2], cl$cluster, col = "white", font=2) 
title("Metoda 3-œrednich", cex.main=1.4, font = 2)

# Dane losowe
rnd_points <- all_points

# Losowanie danych z rozkladu jednostajnego
rnd_points[,1] <- runif(90, xmin, xmax)
rnd_points[,2] <- runif(90, ymin, ymax)

par(mfrow=c(1,1))
# Rysowanie danych losowych
plot(rnd_points, pch=19, cex = 2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), xlab="X", ylab="Y")
title("Dane losowe", cex.main=1.4, font=2)


# Obliczanie statystyki odstepu
# Mamy od 1 do K skupien
# Dla kazdego skupienia przeprowadzamy N prob

K = 10
N = 20

# Deklaracja macierzy, w ktorych trzymamy wyniki
# z kolejnych interacji

dane <- array(0, dim = c(K, N))
dane_rnd <- array(0, dim = c(K, N))

# G³ówna pêtla
for(c in 1:K) {
  for(n in 1:N) {
    cl <- kmeans(dist(all_points), c)
    
    rnd_points[,1] <- runif(90, xrange[1], xrange[2])
    rnd_points[,2] <- runif(90, yrange[1], yrange[2])
    
    cl_rnd <- kmeans(dist(rnd_points), c)
    
    dane[c,n] <- cl$tot.withinss
    dane_rnd[c,n] <- cl_rnd$tot.withinss
  }
}

# Wyznaczenie wartoœci œrednich dla danych i danych losowych
# oraz odchyleñ dla danych losowych

avgW <- apply(dane, 1, mean)
avgWrnd <- apply(dane_rnd, 1, mean)
sdWrnd <- apply(dane_rnd, 1, sd)

# Rysowanie statystki odstêpu
par(mfrow = c(1,2))
plot(1:K, avgWrnd, pch = 19, t="o", lty = 2, ylab = "W", xlab = "K")
points(1:K, avgW, pch = 19, t="o", lty = 2, col = "blue")
title("Wartoœæ W", cex.main=1.4, font=2)
legend("topright", c("oryginalne dane", "dane losowe"), col = c("blue", "black"), pch = 19)

errbar(1:K, log(avgWrnd/avgW), log(avgWrnd+sdWrnd/sqrt(20))-log(avgW), log(avgWrnd-sdWrnd/sqrt(20))-log(avgW), xlab="K", ylab=expression(lnW[rnd]-lnW)) 
title("Statystyka odstêpu", cex.main=1.4, font=2)

# Parametry danych z rozk³adu Gaussa
S1 <- matrix(c(2, 0, 0, 2), 2, 2)
S2 <- matrix(c(2, 0, 0, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(1, 1)

n1 <- 10
n2 <- 10

# Ustawienie ziarna dla losowania danych
set.seed(1290)

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Rysowanie danych
par(mfrow = c(1,2))
plot.data(data); title("Dane")

# Tworzenie dendrogramu
hc <- hclust(dist(data[,1:2]))

# Rysowanie dendrogramu
plot(hc, hang = -1, ylab = "Odleg³oœæ", xlab = "Numer obserwacji", main = "Dendrogram", sub = NA, cex = 0.9, font = 2)
# Obwódka klastrów
rect.hclust(hc, 2, border = c("blue", "orange"))

print(hc$height)

abline(h = hc$height[1], col = "red", lty = 2)

print(hc$merge)

# Tworzenie gradientu kolorów
colfunc <- colorRampPalette(c("blue", "gray", "red"))

# Pobranie danych
votes.repub <- cluster::votes.repub

# Tworzenie wykresu
heatmap(as.matrix(votes.repub), col = colfunc(60), Colv = NA)

# W PD K = 3 
combn(4, 3)