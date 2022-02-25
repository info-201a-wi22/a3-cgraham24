library(tidyverse)
library(maps)
library("dplyr")

incarcerations <-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

first_year <- min(incarcerations$year)

last_year <- max(incarcerations$year)

num_col <- ncol(incarcerations)

num_region <- incarcerations %>%
  summarize(n = n_distinct(region))

