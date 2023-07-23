df <- read.table("wine.data",sep=",", header = F)

colnames(df) <- c("class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids",
                  "Nonflavanoid.phenols","Proanthocyanins", "Color.intensity", "Hue", "OD280.OD315", "Proline")

df$class <- factor(df$class)
N <- nrow(df)

wine.pca <- princomp(~., cor=T, data=df[,-1])

x.vec <- 1:length(df[,-1])
std.sum <- sapply(x.vec, function(i) {
  sum(wine.pca$sdev[1:i])/sum(wine.pca$sdev)
})

plot(x.vec, std.sum, pch = 19, xlab='liczba sk³adowych', ylab='skumulowane odchylenie std.', 
     main='Zale¿noœæ skumulowanego odchylenia std. od liczby sk³adowych' )

plot(wine.pca$scores[,1], wine.pca$scores[,2], pch = 19, col = df[,1], xlab = '1st PC', ylab = '2nd PC', main = 'Punkty w nowych zmiennych dla 1 i 2 sk³adowej')
plot(wine.pca$scores[,2], wine.pca$scores[,3], pch = 19, col = df[,1], xlab = '2nd PC', ylab = '3rd PC', main = 'Punkty w nowych zmiennych dla 2 i 3 sk³adowej')
