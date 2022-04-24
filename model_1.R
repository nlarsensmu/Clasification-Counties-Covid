library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")
library("dplyr")
library("qgcomp")
library("rpart")
library("caret")
library(rpart.plot)

source("./helpers/helpers.R")

data <- read.csv("train.csv")
test <- read.csv("validation.csv")
test <- get_subset_all(test)
data_all <- get_subset_all(data)


tree_default_all <- data_all %>% 
  rpart(risk_cases_numbers ~ ., data = .,control = rpart.control(minsplit = 2, cp = cp))

yhat_all <- predict(tree_default_all, test, type="class")

test <- select(test, -risk_cases_numbers)

print(confusionMatrix(yhat_all, reference = test$risk_cases_numbers)$overall[[1]])

train_index <- createFolds(data_all$risk_cases_numbers, k = 10)

ctreeFit <- data_all %>% train(risk_cases_numbers ~ .,
                                method = "ctree",
                                data = .,
                                tuneLength = 5,
                                trControl = trainControl(method = "cv", indexOut = train_index))


ctreeFit


