library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")
library("dplyr")
library(qgcomp)

 

# the last 356 days of data from the pandemic and census
data_init <- read_csv("./data/query1.csv")

# Select the columns we are thinking about investigating
data_init <- data_init %>% select(total_pop,
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
                                  commute_less_10_mins,
                                  commute_10_14_mins,
                                  commute_15_19_mins,
                                  commute_20_24_mins,
                                  commute_25_29_mins,
                                  commute_30_34_mins,
                                  commute_35_39_mins,
                                  commute_40_44_mins,
                                  commute_45_59_mins,
                                  commute_60_more_mins,
                                  deaths,
                                  confirmed_cases,
                                  county_name,
                                  state,
                                  county_fips_code)


# We are going to use some intuition to combine some columns
data_init$commute_less_than_30 <- data_init$commute_less_10_mins + 
  data_init$commute_10_14_mins +
  data_init$commute_15_19_mins +
  data_init$commute_20_24_mins +
  data_init$commute_25_29_mins

data_init$commute_30_60 <- data_init$commute_30_34_mins +
  data_init$commute_35_39_mins +
  data_init$commute_40_44_mins +
  data_init$commute_45_59_mins

data_init$commute_more_than_60 <- data_init$commute_60_more_mins

data_init <- data_init %>% select(
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
  deaths,
  confirmed_cases,
  county_name,
  state,
  county_fips_code
)



# Get cases/deaths per 1000
data_init$confirmed_cases_per1000 = data_init$confirmed_cases / 
  (data_init$total_pop)*1000
data_init$deaths_per1000 = data_init$deaths / 
  (data_init$total_pop)*1000

summary(data_init)

# Normalize population fields (taken from R companion)
scale_numeric <- 
  function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
data_scaled <- data_init %>% scale_numeric()

# reset the deaths and cases
data_scaled$deaths_per1000 <- data_init$deaths_per1000
data_scaled$confirmed_cases_per1000 <- data_init$confirmed_cases_per1000

summary(data_scaled)

# Investigate Outliers

## Investigate Population vs cases and deaths
p1 <- ggplot(data_scaled, aes(x = total_pop, y = confirmed_cases_per1000)) + 
  geom_rect(mapping=aes(xmin=-1, xmax=10, ymin=0, ymax=300), 
            color="blue", fill="blue", alpha=0.1) +
  geom_point(size = 0.5) +
  ggtitle("Outliers from normalized Population and Cases")
p1
ggsave(".\\charts\\population_outliers.jpeg", width = 6.5, height = 3)

## Fetch the counties we are removing and note them
outliers <- data_scaled %>% filter(total_pop > 10 | confirmed_cases_per1000 > 300)
outliers$county_name
outliers$state

data_filtered <- data_scaled %>% filter(total_pop < 10 & confirmed_cases_per1000 < 300)

## Investigate Median Age
p1 <- ggplot(data_filtered, aes(x = median_age, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from normalized and cases Population")
p1

## Investigate median_income
p1 <- ggplot(data_filtered, aes(x = median_income, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from Median Income and Cases")
p1
ggsave(".\\charts\\median_income_outliers.jpeg", width = 6.5, height = 3)

## Investigate median_rent
p1 <- ggplot(data_filtered, aes(x = median_rent, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from Median Rent and Cases")
p1
ggsave(".\\charts\\median_rent_outliers.jpeg", width = 6.5, height = 3)

#percent_income_spent_on_rent
p1 <- ggplot(data_filtered, aes(x = percent_income_spent_on_rent, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from % of income spent on Rent and Cases")
p1
ggsave(".\\charts\\percent_income_spent_on_rent.jpeg", width = 6.5, height = 3)

# high_school_diploma outliers
p1 <- ggplot(data_filtered, aes(x = high_school_diploma, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from Normalized High School  Diploma holders and Cases")
p1
ggsave(".\\charts\\high_school_diploma.jpeg", width = 6.5, height = 3)

# bachelors_degree
p1 <- ggplot(data_filtered, aes(x = bachelors_degree, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from Normalized Bachelors Degree and Cases")
p1
ggsave(".\\charts\\bachelors_degree.jpeg", width = 6.5, height = 3)

# masters_degree
p1 <- ggplot(data_filtered, aes(x = masters_degree, y = confirmed_cases_per1000)) + 
  geom_point(size = 0.5) +
  ggtitle("Outliers from Normalized Masters Degree and Cases")
p1
ggsave(".\\charts\\masters_degree.jpeg", width = 6.5, height = 3)

# Pick confirmed cases categories
p <- ggplot(data_filtered, aes(y=confirmed_cases_per1000)) + 
  geom_boxplot() +
  ggtitle("Box Plot of Confirmed Cases per 1000 people")
p
ggsave(".\\charts\\confirmed_cases.jpeg", width = 6.5, height = 3)

data_filtered $risk_cases <-
  data_filtered %>%
    pull(confirmed_cases_per1000) %>%
      cut(breaks=c(-Inf,80,110,Inf), 
          labels=c("low", "middle", "high"))

summary(data_filtered %>% select(risk_cases))


## Incase we pivoit to using deaths 

# Pick confirmed cases categories
p <- ggplot(data_filtered, aes(y=deaths_per1000)) + 
  geom_boxplot()
p

data_filtered $risk_deaths <-
  data_filtered %>%
  pull(deaths_per1000) %>%
  cut(breaks=c(-Inf,1.1,2.5,Inf), 
      labels=c("low", "middle", "high"))

summary(data_filtered)

data_filtered %>% write.csv(file = "datatable.csv")


##################################
### play ground
source("./clustering_helpers.R")
data <- temp

data$deaths_per10000 <-  data$deaths_per1000 * 10
n <- 5
vect <- data$deaths_per10000
vect <- sort(vect)
l <- length(data$deaths_per10000)
break_deaths <- c(-Inf)
n <- n + 1
window <- l %/% ((n - 1))
for (i in 1:(n-2)) {
  print(i*window)
  break_deaths <- append(break_deaths, vect[i*window])
}
break_deaths <- append(break_deaths, Inf)


labels_deaths <- c()
for (i in 1:(n-1)) {
  labels_deaths <- append(labels_deaths, paste("deaths_class_", i, sep = ""))
}
labels_deaths

cut(data$deaths_per10000,
    breaks = break_deaths,
    labels = labels_deaths)

test <- catagorize_deaths(data, 5)
data$deaths_class = test
print(data %>% filter(deaths_class == "deaths_class_3"))
catagorize_cases(data, 5)

n
cut1 <- med - min
cut2 <- max - med
cut(data$confirmed_cases_per1000, 
    breaks=c(-Inf, cut1, cut2, Inf), 
    labels=c("low","middle","high"))

