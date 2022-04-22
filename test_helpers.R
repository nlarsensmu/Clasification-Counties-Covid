library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")
library("dplyr")
library("gsubfn")

source("./clustering_helpers.R")

data <- read_csv("./data/Query1.csv")
data
train_percent = 0.7
test_percent = 0.3


result  = train_test_split(data, train_percent, test_percent)
train <- result[[1]]
test <- result[[2]]
