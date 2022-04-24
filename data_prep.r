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
library("ggcorrplot")
 

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

# Normalize total_pop (taken from R companion)
data_scaled <- data_init
data_scaled$total_pop <- as.vector(scale(data_scaled$total_pop))
data_scaled$median_age <- as.vector(scale(data_scaled$median_age))
data_scaled$median_income <- as.vector(scale(data_scaled$median_income))
data_scaled$median_rent <- as.vector(scale(data_scaled$median_rent))
data_scaled$percent_income_spent_on_rent <- as.vector(scale(data_scaled$percent_income_spent_on_rent))

#Every column from here on out will be noramilized based on population.
#The resulting columns will be X per 1000 peoples
data_scaled$white_pop <- data_scaled$white_pop / data_init$total_pop * 1000
data_scaled$black_pop <- data_scaled$black_pop / data_init$total_pop * 1000
data_scaled$asian_pop <- data_scaled$asian_pop/ data_init$total_pop * 1000
data_scaled$hispanic_pop <- data_scaled$hispanic_pop / data_init$total_pop * 1000
data_scaled$amerindian_pop <- data_scaled$amerindian_pop / data_init$total_pop * 1000
data_scaled$other_race_pop <- data_scaled$other_race_pop / data_init$total_pop * 1000
data_scaled$two_or_more_races_pop <- data_scaled$two_or_more_races_pop / data_init$total_pop * 1000
data_scaled$commuters_by_public_transportation <- data_scaled$commuters_by_public_transportation / data_init$total_pop * 1000
data_scaled$high_school_diploma <- data_scaled$high_school_diploma / data_init$total_pop * 1000
data_scaled$less_one_year_college <- data_scaled$less_one_year_college / data_init$total_pop * 1000
data_scaled$bachelors_degree <- data_scaled$bachelors_degree / data_init$total_pop * 1000
data_scaled$masters_degree <- data_scaled$masters_degree / data_init$total_pop * 1000
data_scaled$commute_less_than_30 <- data_scaled$commute_less_than_30 / data_init$total_pop * 1000
data_scaled$commute_30_60 <- data_scaled$commute_30_60 / data_init$total_pop * 1000
data_scaled$commute_more_than_60 <- data_scaled$commute_more_than_60 / data_init$total_pop * 1000


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
  geom_boxplot()
p

data_filtered $risk_cases <-
  data_filtered %>%
    pull(confirmed_cases_per1000) %>%
      cut(breaks=c(-Inf,90,110,Inf), 
          labels=c("low", "middle", "high"))

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
corr_data <- select(data_filtered,
  masters_degree,
  commute_less_than_30,
  commute_30_60,
  commute_more_than_60,
  deaths,
  confirmed_cases
)

corr_data$random <- runif(3136, min=0, max=100)
cm1 <- corr_data %>% as.matrix %>% cor()
cm1
p <- ggcorrplot(cm1)
p
