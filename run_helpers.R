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

source("./helpers/helpers.R")
data <- read_csv("./data/Query1.csv")
train_percent = 0.9
test_percent = 0.1
result  = train_test_split(data, train_percent, test_percent)

#We are going to save off some of the data till the very end.
write.csv(result[[1]], file = "train.csv")
write.csv(result[[2]], file = "validation.csv")
