library(MASS)
library(mvtnorm)
library(pROC)
library(caret)

draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- "A"; X2$class <- "B"
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

plot.data <- function(data) {
  
  cols <- c("blue", "orange")
  
  plot(data[,1:2], col = cols[data$class], cex = 2, tcl = .25)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.6)
  
}
# Parametry danych z rozkladu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)
plot.data(data)

input <- createDataPartition(data$class, p = 0.6, list = F)

data.train <- data[input,]
data.test <- data[-input,]

plot.data(data)
points(data.train[,1:2], cex = 4, pch = 22)
points(data.test[,1:2], cex = 4, pch = 23)

table(data.train$class) / sum(table(data.train$class))
table(data.test$class) / sum(table(data.test$class))

input2 <- createDataPartition(data$class, p = 0.2, times = 5)

fit <- trainControl(method = "none", classProbs = TRUE)
model <- train(class ~ ., data = data.train, 
               method = "lda", trControl = fit)
model

predict(model, data.test)
predict(model, data.test, type = "prob")

fit <- trainControl(method = "cv", number = 10)
model <- train(class ~ ., data = data, 
               method = "lda", trControl = fit)
model

grid <- expand.grid(k = 2:30)
model <- train(class ~ ., data = data, 
               method = "knn", trControl = fit,
               tuneGrid = grid)
model
plot(model)

model1 <- train(class ~ ., data = data, method = "lda", trControl = fit)
model2 <- train(class ~ ., data = data, method = "qda", trControl = fit)
model3 <- train(class ~ ., data = data, method = "nb", trControl = fit)
model4 <- train(class ~ ., data = data, method = "knn", trControl = fit, tuneGrid = grid)

res <- resamples(list(LDA = model1, QDA = model2, NB = model3, KNN = model4))
bwplot(res)
dotplot(res)

CM.values <- function(org.class, pred.posterior, threshold) {
  
  pred.class <- factor(ifelse(pred.posterior >= threshold, 2, 1))
  
  TP <- sum(pred.class == 2 & org.class == "B")
  TN <- sum(pred.class == 1 & org.class == "A")
  
  sum.neg <- sum(org.class == "A")
  sum.pos <- sum(org.class == "B")
  
  TPR <- TP / sum.pos
  FPR <- 1 - TN / sum.neg
  
  return(c(FPR,TPR))
}

get.roc.values <- function(class, posterior) {
  
  progi <- c(-Inf, sort(unique(posterior)), +Inf)
  
  z <- t(sapply(1:length(progi), function(i) CM.values(class, posterior, progi[i])))
  
  return(z)
}

model1.pred <- predict(model1, data, type = "prob")

plot.roc(data$class, model1.pred$A)

roc.own <- get.roc.values(data$class, model1.pred$A)
points(roc.own[,1], 1 - roc.own[,2], col = "red")

plot.roc(data$class, model1.pred$A, percent = T, ci = TRUE, of="thresholds", thresholds="best", print.thres="best", print.auc = TRUE)

featurePlot(x = data[,1:2], y = data$class, plot = "density", pch = "|", auto.key = list(columns = 2))

plot.roc(data$class, data$x, percent = T, col = "red")
lines.roc(data$class, data$y, percent = T, col = "blue")
legend("bottomright", legend=c("x", "y"), col=c("red", "blue"), lwd=2)
