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
