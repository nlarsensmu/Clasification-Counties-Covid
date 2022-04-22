library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")
library("dplyr")

 

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
                                  confirmed_cases)


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
  masters_degree,
  commute_less_than_30,
  commute_30_60,
  commute_more_than_60,
  deaths,
  confirmed_cases
)

# Normalize population fields (taken from R companion)
scale_numeric <- 
  function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
data_scaled <- data_init %>% scale_numeric()

# reset the deaths and cases
data_scaled$deaths <- data_init$deaths
data_scaled$confirmed_cases <- data_init$confirmed_cases

summary(data_scaled)

# Investigate Outliers
# Investigate Population vs cases and deaths
p1 <- ggplot(data_scaled, aes(x = original_total_pop, y = confirmed_cases_per1000)) + 
  geom_point()
p1
target <- max(temp$original_total_pop)
target

match(temp$original_total_pop, target)

outlier_row = temp %>% filter(original_total_pop > 7.5e6)
outlier_row$county_name
outlier_row$state
outlier_row$county_fips_code

p1 <- ggplot(temp, aes(x = original_total_pop, y = deaths)) + 
  geom_point() +
  geom_text(data = temp,
            aes(x = 1e07-21e5, y = 1.425e4, label = "LA County >>")) +
  ggtitle("Cases per Population Outliers")
p2 <- ggplot(temp, aes(x = original_total_pop, y = confirmed_cases)) + 
  geom_point() +
  geom_text(data = temp,
            aes(x = 1e07-21e5, y = 9.3e5, label = "LA County >>")) +
  ggtitle("Deaths per Population Outliers")


g <- ggarrange(p1, p2)
annotate_figure(g, top = text_grob("Populations vs Deaths and Cases"))

ggsave("./charts/DeathsCasesPlot.jpg", width = 6.5, height = 3)

# Filter out LA county.

temp <- temp %>% filter(county_fips_code != "06037")


temp

temp <- temp %>% filter(original_total_pop >= 1000)

temp %>% write.csv(file = "datatable.csv")


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

