#Used to split one set of data into a train and test split.
train_test_split <- function(data, train_percent, test_percent){
  zeros <- rep(0, as.integer(ceiling(train_percent * nrow(data))))
  ones <-  rep(1,  as.integer(floor(test_percent * nrow(data))))
  
  split <- sample(c(zeros, ones))
  train <- data[split == 0, ]     
  test <- data[split == 1, ]   
  list(train, test) 
}
