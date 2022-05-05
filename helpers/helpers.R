#Used to split one set of data into a train and test split.
train_test_split <- function(data, train_percent, test_percent){
  zeros <- rep(0, as.integer(ceiling(train_percent * nrow(data))))
  ones <-  rep(1,  as.integer(floor(test_percent * nrow(data))))
  
  split <- sample(c(zeros, ones))
  train <- data[split == 0, ]     
  test <- data[split == 1, ]   
  list(train, test) 
}

get_subset_1 <- function(data) {
  data$risk_cases_numbers <- sapply(data$risk_cases, unclass)
  data$risk_cases_numbers <- as.factor(data$risk_cases_numbers)
  
  select(data, 
         total_pop,
         median_age,
         white_pop,
         black_pop,
         asian_pop,
         hispanic_pop,
         amerindian_pop,
         other_race_pop,
         two_or_more_races_pop,
         risk_cases_numbers, 
         median_income,
         median_rent,
         percent_income_spent_on_rent,
         high_school_diploma,
         less_one_year_college,
         bachelors_degree,
         masters_degree,
         risk_cases_numbers)
}


get_subset_2 <- function(data) {
  data$risk_cases_numbers <- sapply(data$risk_cases, unclass)
  data$risk_cases_numbers <- as.factor(data$risk_cases_numbers)
  
  select(data, 
         median_income,
         median_rent,
         percent_income_spent_on_rent,
         high_school_diploma,
         less_one_year_college,
         bachelors_degree,
         masters_degree,
         risk_cases_numbers)
}


get_subset_3 <- function(data) {
  data$risk_cases_numbers <- sapply(data$risk_cases, unclass)
  data$risk_cases_numbers <- as.factor(data$risk_cases_numbers)
  
  select(data, 
         commute_less_than_30,
         commute_30_60,
         commute_more_than_60,
         commuters_by_public_transportation,
         total_pop,
         median_age,
         risk_cases_numbers)
}

get_subset_all <- function(data) {
  data$risk_cases_numbers <- sapply(data$risk_cases, unclass)
  data$risk_cases_numbers <- as.factor(data$risk_cases_numbers)
  
  select(data, 
         total_pop,
         median_age,
         white_pop,
         black_pop,
         asian_pop,
         hispanic_pop,
         amerindian_pop,
         other_race_pop,
         two_or_more_races_pop,
         commuters_by_public_transportation,
         median_income,
         median_rent,
         percent_income_spent_on_rent,
         high_school_diploma,
         less_one_year_college,
         bachelors_degree,
         masters_degree,
         commute_less_than_30,
         commute_30_60,
         commute_more_than_60,
         risk_cases_numbers)
}

print_accuracy <- function(model, train_data, test_data) { 
  yhat_train <- predict(model, train_data)
  conf_train <- confusionMatrix(yhat_train, train_data$risk_cases_numbers)
  print(conf_train$overall)
  print(conf_train$table)
  
  yhat_test <- predict(model, test_data)
  conf_test <- confusionMatrix(yhat_test, test_data$risk_cases_numbers)
  print(conf_test$overall)
  print(conf_test$table)
  
  c(conf_test, conf_train)
}

get_roc <- function(model, train_data, test_data) {
  yhat_test <- predict(model, test_data, type="prob")
  r_test <- roc(test_data$risk_cases_numbers == 'high', yhat_test[,"high"])
  test_auc <- r_test$auc
  ggroc(r_test)
  yhat_train <- predict(model, train_data, type="prob")
  r_train <- roc(train_data$risk_cases_numbers == 'high', yhat_train[,"high"])
  train_auc <- r_train$auc
  r_train
  ggroc(r_train)
  
  train_ruc_string <- paste("Train AUC (", round(r_train$auc, 4), ")")
  test_ruc_string <- paste("Validation AUC (", round(r_test$auc, 4), ")")
  ggroc(list(train = r_train, validation = r_test)) +
    annotate("text", x = c(0.50,0.50), y=c(0.25,0.20), label = c(test_ruc_string, train_ruc_string))
}

compare_model_acc <- function(conf_1, conf_2, conf_3, conf_all){
  model_names <- rep(c("Dataset_1", "Dataset_2","Dataset_3","All Columns"),3)
  
  metric_names <- c(rep("Recall", 4), rep("Precision", 4), rep("Accuracy", 4))
  
  recalls <- c(conf_1$byClass[["Recall"]], conf_2$byClass[["Recall"]], 
               conf_3$byClass[["Recall"]], conf_all$byClass[["Recall"]])
  
  
  precisions <- c(conf_1$byClass[["Precision"]], conf_2$byClass[["Precision"]], 
                  conf_3$byClass[["Precision"]], conf_all$byClass[["Precision"]])
  
  
  accuracys <- c(conf_1$overall[["Accuracy"]], conf_2$overall[["Accuracy"]], 
                 conf_3$overall[["Accuracy"]], conf_all$overall[["Accuracy"]])
  
  all_metrics <- c(recalls, precisions, accuracys)
  
  bar_data <- data.frame(model_names, metric_names, all_metrics)
  ggplot(bar_data, aes(fill=metric_names, y=all_metrics, x=model_names)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Comparing Model Accuracy") + 
    ylab("Accuracy") + xlab("Model") +
    ylim(0, 1)
}

plot_pca_groups <- function(train_data, yhat) {
  
  data_temp = train_data[1:length(names(train_data))-1]
  data_temp["yhat"] = yhat
  
  data_temp.pca <- prcomp(data_temp[1:length(names(data_temp))-1])
  
  autoplot(data_temp.pca,
           data=data_temp,
           colour = 'yhat')
}
