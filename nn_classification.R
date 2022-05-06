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
data_all <- get_subset_all(train_original)
test_all <- get_subset_all(test_original)

yhat <- get_nn_X_Pred()

length(yhat)
count(data_all)


plot_pca_groups(select(data_all, -risk_cases_numbers), yhat) + 
  ggtitle("PCA on Set All with ANN")
ggsave(".\\charts\\pca_nn.png",
       width = 6.5,  height = 3,  units =  "in"
)
