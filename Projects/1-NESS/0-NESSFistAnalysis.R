######Chapter 5; Case Study - ER injuries
library(shiny)
library(vroom)
library(tidyverse)

#Data: National Electronic Injury Surveillance System (NEISS) - 2017

injuries <- vroom::vroom("injuries.tsv.gz")
products <- vroom::vroom("https://raw.githubusercontent.com/hadley/mastering-shiny/master/neiss/products.tsv")
population <- vroom::vroom("https://raw.githubusercontent.com/hadley/mastering-shiny/master/neiss/population.tsv")

summary <- selected %>%
    count(age, sex, wt = weight) %>%
    left_join(population, by=c("age","sex")) %>%
    mutate(prop_accident = n / population * 1e4)

summary %>%
    ggplot(aes(age, prop_accident, colour = sex)) +
    geom_line() +
    labs(y = "Estimated number of injuries")
