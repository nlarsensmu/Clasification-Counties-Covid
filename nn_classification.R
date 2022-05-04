library("neuralnet")
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
library("e1071")
library("pROC")
library("ROSE")
library(randomForest)
library(caret)
source("./helpers/helpers.R")
library(tensorflow)
library("keras")


train_original <- read.csv("train.csv")
test_original <- read.csv("validation.csv")

X_train <- get_subset_all(train_original)
X_test <- get_subset_all(test_original)


input <- layer_input_from_dataset(X_train %>% select(-risk_cases_numbers))

output <- input %>% 
  layer_dense_features(dense_features(spec)) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) 

model <- keras_model(input, output)
summary(model)






set.seed(1)

fun <- function(arg) {
  ret <- 0
  if (arg == "high"){
    ret <- 1
  }
  ret
}

X_train$risk_cases_numbers <- sapply(X_train$risk_cases_numbers, fun)

n <- neuralnet(risk_cases_numbers~ .,
               data = X_train,
               hidden = 3,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 5,
               algorithm = "rprop+",
               threshold = 0.3,
               stepmax = 100000)
Predict=compute(n,test)
