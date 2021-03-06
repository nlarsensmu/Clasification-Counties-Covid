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
data <- read_csv("./datatable.csv")
data$risk_deaths
data$risk_cases
train_percent = 0.9
test_percent = 0.1
result  = train_test_split(data, train_percent, test_percent)

#We are going to save off some of the data till the very end.
write.csv(result[[1]], file = "train.csv")
write.csv(result[[2]], file = "validation.csv")


###### Just code to test not used #####
test <- get_subset_1(data)
test <- get_subset_2(data)
test <- get_subset_3(data)

