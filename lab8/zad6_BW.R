animals <- cluster::animals

colnames(animals) <- c("warm-blooded", 
                       "can fly",
                       "vertebrate",
                       "endangered",
                       "live in groups",
                       "have hair")


heatmap(as.matrix(animals), col = c("red", "green"))

# Interpretacja:
# Je�li chodzi o wiersze to na heatmapie wida� podzia� na owady (fly, bee, spid, cpl, hom, ant) - pozosta�e typy zwierz�t (reszta)
# Je�li chodzi o kolumny to wida� �e cechy vertebrate i warm-blooded s� blisko ze sob� skorelowane (kr�gowce s� zwykle ciep�okrwiste),
# r�wnie� cechy can fly oraz endangered s� powi�zane aczkolwiek ju� mniej �ci�le.


col.list <- list()

for (i in 2:4) {
  
  cmb <- combn(4, i)
  
  for (c in 1:ncol(cmb)){
    
    col.list <- append(col.list, list(cmb[,c]))
  }
}

df <- iris

w.vec <- c()

for (i in 1:length(col.list)) {

  w.vec[i] <- kmeans(dist(df[col.list[[i]]]), 3)$tot.withinss
}

string.col.names <- lapply(col.list, toString)

names(w.vec) <- string.col.names

w.vec <- sort(w.vec, decreasing = T)

column.names <- colnames(iris)

plot(w.vec, pch = 19,
     xlab = "U�yte kolumny", 
     ylab = "W", main = "Warto�� wsp�czynnika W w zale�no�ci od u�ytych kolumn", xaxt = "n")
axis(1, at = 1:length(w.vec),
     labels = names(w.vec), las = 2)
legend("topright", legend=c(paste("1 - ", column.names[1]), paste("2 - ", column.names[2]), paste("3 - ", column.names[3]), paste("4 - ", column.names[4])))