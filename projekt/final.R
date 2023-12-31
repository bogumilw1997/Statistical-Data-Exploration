rm(list=ls())

library(Hmisc)
library(ggplot2)
library(caret)

df <- read.csv('owid-covid-data.csv', header = T)

df <- df[df$location == 'Poland',]
df$date <- as.Date(df$date, "%Y-%m-%d")

df <- df[df$date >= '2021-08-31',]

df <- df[df$date <= '2022-05-05',]

colnames(df)

df <- df[c('date',"new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", 
           'new_vaccinations_smoothed_per_million', 'new_tests_smoothed_per_thousand',
           'new_people_vaccinated_smoothed_per_hundred',
           'tests_per_case')]

describe(df)

df$new_cases_diff <- 0
df$new_cases_diff_now <- 0
df$new_deaths_diff <- 0
df$new_vaccinations_diff <- 0
df$new_tests_diff <- 0
df$new_people_vaccinated_diff <- 0

df$new_cases_diff[-nrow(df)] <- tail(df$new_cases_smoothed_per_million, -1) - head(df$new_cases_smoothed_per_million, -1)
df$new_cases_diff_now[-1] <- tail(df$new_cases_smoothed_per_million, -1) - head(df$new_cases_smoothed_per_million, -1)
df$new_deaths_diff[-1] <- tail(df$new_deaths_smoothed_per_million, -1) - head(df$new_deaths_smoothed_per_million, -1)
df$new_vaccinations_diff[-1] <- tail(df$new_vaccinations_smoothed_per_million, -1)- head(df$new_vaccinations_smoothed_per_million, -1)
df$new_tests_diff[-1] <- tail(df$new_tests_smoothed_per_thousand, -1) - head(df$new_tests_smoothed_per_thousand, -1)
df$new_people_vaccinated_diff[-1] <- tail(df$new_people_vaccinated_smoothed_per_hundred, -1) - head(df$new_people_vaccinated_smoothed_per_hundred, -1)

df <- tail(df, -1)
df <- head(df, -1)

df$class <- 1
df$class[df$new_cases_diff <= 0] <- 2
df$class <- as.factor(df$class)

table(df$class)/ sum(table(df$class))

N <- nrow(df)
#df[,c('date','new_cases_smoothed_per_million', 'new_cases_diff', 'class')]
#old.loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English") 

train.index <- createDataPartition(df$class, p = 0.75, list = F)

train.set <- df[train.index,]
test.set <- df[-train.index,]

df$train.test <- 0

df$train.test[train.index] <- 'train'
df$train.test[-train.index] <- 'test'

df$train.test <- as.factor(df$train.test)

table(train.set$class)/ sum(table(train.set$class))

g1 <- ggplot(df, aes(x=date, y=new_cases_smoothed_per_million, color=class, group=train.test)) + 
  geom_point(aes(shape=train.test)) + labs(title = 'New COVID-19 cases per million', y = "new cases", x = "date") + 
  scale_color_hue(labels = c("increase", "decrease")) + scale_x_date(date_labels = "%b/%Y",date_breaks = "months")

print(g1)

#data <- df[, -c(1, length(df)-1)]

fit <- trainControl(method = "cv", number = 10)

train.set <- train.set[sample(nrow(train.set)),]

data <- train.set[, c('class', "new_deaths_smoothed_per_million", 
                      'new_vaccinations_smoothed_per_million', 'new_tests_smoothed_per_thousand',
                      'new_people_vaccinated_smoothed_per_hundred','new_deaths_diff', 
                      'new_vaccinations_diff', 'new_tests_diff', 'new_people_vaccinated_diff', 'tests_per_case')]

grid <- expand.grid(k = 2:20)

model1 <- train(class ~ ., data = data, method = "lda", trControl = fit)
model2 <- train(class ~ ., data = data, method = "qda", trControl = fit)
model3 <- train(class ~ ., data = data, method = "nb", trControl = fit)
model4 <- train(class ~ ., data = data, method = "knn", trControl = fit, tuneGrid = grid)
model5 <- train(class ~ ., data = data, method = "rpart", trControl = fit)

res <- resamples(list(LDA = model1, QDA = model2, NB = model3, KNN = model4, CART = model5))

# bwplot(res)
dotplot(res)

df$model.predictions <- predict(model1, df[,c('class',"new_deaths_smoothed_per_million", 
                                              'new_vaccinations_smoothed_per_million', 'new_tests_smoothed_per_thousand',
                                              'new_people_vaccinated_smoothed_per_hundred','new_deaths_diff', 
                                              'new_vaccinations_diff', 'new_tests_diff', 'new_people_vaccinated_diff', 'tests_per_case')])

df$model.predictions_diff <- as.numeric(as.character(df$class)) - as.numeric(as.character(df$model.predictions))
df$model.predictions_diff[df$model.predictions_diff != 0] <- 1

df$model.predictions_diff <- as.factor(df$model.predictions_diff)

g2 <- ggplot(df, aes(x=date, y=new_cases_smoothed_per_million, color=model.predictions_diff, group=train.test)) + 
  geom_point(aes(shape=train.test)) + labs(title = 'Prediction results for LDA model', y = "new cases", x = "date", color = "Prediction\n", shape = 'Test/train\n') + 
  scale_color_manual(labels = c("correct", "wrong"), values = c("blue", "red")) + scale_x_date(date_labels = "%b/%Y",date_breaks = "months")
print(g2)

find.acc <- function(data) {
  
  CM <- table(data$class, data$model.predictions)
  
  N <- sum(CM)
  ACC <- sum(diag(CM))/N
  
  return(ACC)
}

find.acc(df[df$train.test == 'train',])
find.acc(df)
find.acc(df[df$train.test == 'test',])