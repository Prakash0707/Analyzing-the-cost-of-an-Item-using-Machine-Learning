library(openxlsx)
library(dplyr)
library(tidyverse)
library(caret)
library(boot)


df1 <- read.xlsx(xlsxFile = "MBA6636_SM21_Professor_Proposes_Data.xlsx", sheet = 1, skipEmptyRows = FALSE,colNames = TRUE)
summary(df1)


set.seed(10)
train_control <- trainControl(method = "repeatedcv",
                              number = 5, repeats = 3)
model <- train(Price ~., data = df1,
               method = "lm",
               trControl = train_control)
print(model)

set.seed(11)
train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)
model_10 <- train(Price ~., data = df1,
               method = "lm",
               trControl = train_control)
print(model_10)

set.seed(12)
train_control <- trainControl(method = "LOOCV")
model <- train(Price ~., data = df1,
               method = "lm",
               trControl = train_control)
print(model)


# Creating Function to obtain R-Squared from the data
r_squared <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$r.square)
}
output <- boot(data=df1, statistic=r_squared, 
               R=5000, formula=Price~.)

output

boot.ci(output, type=c("norm","perc","bca"))



df2 <- read.csv("bank-full.csv",sep = ";",header = T)
summary(df2)
df2$y <- as.factor(df2$y)
set.seed(20)
train_control <- trainControl(method = "cv",
                              number = 5, classProbs = TRUE)
model <- train(y ~., data = df2,
               method = "glm",
               trControl = train_control)
print(model)

set.seed(21)
train_control <- trainControl(method = "cv",
                              number = 10, classProbs = TRUE)
model <- train(y ~., data = df2,
               method = "glm",
               trControl = train_control)
print(model)


coef_function <- function(formula, data, indices) {
  d <- data[indices,] #allows boot to select sample
  fit <- lm(formula, data=d) #fit regression model
  return(coef(fit)) #return coefficient estimates of model
}
reps <- boot(data=df2, statistic=coef_function, R=200,formula=y~.)
reps
boot.ci(boot.out = reps, type=c("norm","basic","percentile"), index = 1)



