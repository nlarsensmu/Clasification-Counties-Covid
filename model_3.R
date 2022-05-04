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
set.seed(1)
train_original <- read.csv("train.csv")
test_original <- read.csv("validation.csv")

## We have a class imbalance on our hands. To help solve this we will 
## to sovle this we will use over sampling 

data_1 <- get_subset_1(train_original)
test_1 <- get_subset_1(test_original)
data_2 <- get_subset_2(train_original)
test_2 <- get_subset_2(test_original)
data_3 <- get_subset_3(train_original)
test_3 <- get_subset_3(test_original)
data_all <- get_subset_all(train_original)
test_all <- get_subset_all(test_original)

count <- as.integer(count(train_original)[[1]] * 1.4)
typeof(count)
data_1_over <- ovun.sample(risk_cases_numbers ~., data = data_1, method = "over", N = count)$data
data_2_over <- ovun.sample(risk_cases_numbers ~., data = data_2, method = "over", N = count)$data
data_3_over <- ovun.sample(risk_cases_numbers ~., data = data_3, method = "over", N = count)$data
data_all_over <- ovun.sample(risk_cases_numbers ~., data = data_all, method = "over", N = count)$data

#### NOTE method=
#tuneLength = 20
#method = "cv"
#number_of_folds = 10
#ctrl <- trainControl(
#  method = "repeatedcv", 
#  repeats = 3,
#  classProbs = TRUE, 
#  summaryFunction = twoClassSummary
#)

tuneLength = 2
number_of_folds = 10
ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 10,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

method = "cforest"
model_1 <- data_1_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )

model_2 <- data_2_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )


model_3 <- data_3_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )

model_all <- data_all_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )

#create the confusion matrix as well as calculate other data
conf_1 <- print_accuracy(model_1, data_1, test_1)
conf_2 <- print_accuracy(model_2, data_2, test_2)
conf_3 <- print_accuracy(model_3, data_3, test_3)
conf_all <- print_accuracy(model_all, data_all, test_all)

#create graph that compares the accuracies 
fig <- compare_model_acc(conf_1, conf_2, conf_3, conf_all)
fig
ggsave(".\\charts\\model_3\\accuracies.png",  plot = fig,  
       width = 6.5,  height = 3,  units =  "in"
)
#create the roc charts 
title <- "CTREE"
roc <- get_roc(model_1, data_1, test_1)
roc <- roc + ggtitle(paste(title, " 1"))
roc
ggsave(".\\charts\\model_3\\roc_1.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

roc <- get_roc(model_2, data_2, test_2)
roc <- roc + ggtitle(paste(title, " 2"))
roc
ggsave(".\\charts\\model_3\\roc_2.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

roc <- get_roc(model_3, data_3, test_3)
roc <- roc + ggtitle(paste(title, " 3"))
roc
ggsave(".\\charts\\model_3\\roc_3.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

roc <- get_roc(model_all, data_all, test_all)
roc <- roc + ggtitle(paste(title, " all"))
roc
ggsave(".\\charts\\model_3\\roc_all.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)


model_1$modelInfo

conf_all$byClass
