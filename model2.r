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
library(ggfortify)
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

count <- as.integer(count(train_original)[[1]] * 1.9)
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

tuneLength = 20
ctrl <- trainControl(
  method = "repeatedcv", 
  repeats = 10,
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

grid <- createGrid("knn", len=4)

positiveWeight = 0.6
negativeWeight = 0.4

modelWeights <- ifelse(data_1_over$risk_cases_numbers== "high",
                       positiveWeight, negativeWeight)

grid <- expand.grid(cp = c(0.01, 0.001))
model_1 <- 1

method = "rpart"
model_1 <- data_1_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC",
        #tuneGrid = grid
  )

model_1$bestTune

rpart.plot(model_1$finalModel, extra = 2,
           box.palette = list("Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Pu"))

imp <- varImp(model_1, compete = FALSE)
plt <- ggplot(imp) + ggtitle("RPART Feature Set 1 Importance")
ggsave(".\\charts\\model_2\\model2_1imp.png",  plot = plt,  
       width = 6.5,  height = 3,  units =  "in"
)

p <- plot_pca_groups(select(data_1, -risk_cases_numbers), predict(model_1, select(data_1, -risk_cases_numbers))) + 
  ggtitle("PCA on Set 1")
p
ggsave(".\\charts\\model_2\\pca_1.png",  plot = p,  
       width = 6.5,  height = 3,  units =  "in"
)

model_2 <- data_2_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )

p <- plot_pca_groups(data_2, predict(model_2, data_2[1:length(names(data_2))-1])) + 
  ggtitle("PCA on Set 2")
ggsave(".\\charts\\model_2\\pca_2.png",  plot = p,  
       width = 6.5,  height = 3,  units =  "in"
)


imp <- varImp(model_2, compete = FALSE)
plt <- ggplot(imp) + ggtitle("RPART Feature Set 2 Importance")
ggsave(".\\charts\\model_2\\model2_2imp.png",  plot = plt,  
       width = 6.5,  height = 3,  units =  "in"
)

model_3 <- data_3_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )


imp <- varImp(model_3, compete = FALSE)
plt <- ggplot(imp) + ggtitle("RPART Feature Set 3 Importance")
ggsave(".\\charts\\model_2\\model2_3imp.png",  plot = plt,  
       width = 6.5,  height = 3,  units =  "in"
)

p <- plot_pca_groups(data_3, predict(model_3, data_3[1:length(names(data_3))-1])) + 
  ggtitle("PCA on Set 3")
ggsave(".\\charts\\model_2\\pca_3.png",  plot = p,  
       width = 6.5,  height = 3,  units =  "in"
)



model_all <- data_all_over %>%
  train(risk_cases_numbers ~ .,
        data = . ,
        method = method,
        trControl = ctrl,
        tuneLength = tuneLength, 
        metric = "ROC"
  )

imp <- varImp(model_all, compete = FALSE)
plt <- ggplot(imp) + ggtitle("Model 2 Feature Set ALL Importance")
ggsave(".\\charts\\model_2\\model2_allimp.png",  plot = plt,
       width = 6.5,  height = 3,  units =  "in"
)

p <- plot_pca_groups(data_all, predict(model_all, select(data_all, -risk_cases_numbers))) + 
  ggtitle("PCA on Set ALL")
p
ggsave(".\\charts\\model_2\\pca_all.png",  plot = p,  
       width = 6.5,  height = 3,  units =  "in"
)

#create the confusion matrix as well as calculate other data
conf_1 <- print_accuracy(model_1, data_1, test_1)
conf_2 <- print_accuracy(model_2, data_2, test_2)
conf_3 <- print_accuracy(model_3, data_3, test_3)
conf_all <- print_accuracy(model_all, data_all, test_all)

#create graph that compares the accuracies 
fig <- compare_model_acc(conf_1, conf_2, conf_3, conf_all)
fig
ggsave(".\\charts\\model_2\\accuracies.png",  plot = fig,  
       width = 6.5,  height = 3,  units =  "in"
)
#create the roc charts 
title <- "R Part"
roc <- get_roc(model_1, data_1, test_1)
roc <- roc + ggtitle(paste(title, " 1"))
roc
ggsave(".\\charts\\model_2\\roc_1.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

roc <- get_roc(model_2, data_2, test_2)
roc <- roc + ggtitle(paste(title, " 2"))
roc
ggsave(".\\charts\\model_2\\roc_2.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

roc <- get_roc(model_3, data_3, test_3)
roc <- roc + ggtitle(paste(title, " 3"))
roc
ggsave(".\\charts\\model_2\\roc_3.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

roc <- get_roc(model_all, data_all, test_all)
roc <- roc + ggtitle(paste(title, " all"))
roc
ggsave(".\\charts\\model_2\\roc_all.png",  plot = roc,  
       width = 6.5,  height = 3,  units =  "in"
)

