library(dplyr)
library(tidyverse)
library(ggplot2)
library(XML)
library(RCurl)
library(rvest)
library(zoo)

perHundredThousands <- function (X, Y) {
  return((X/Y)*100000 )
}

mobility_region <- function(dataset, region ) {
  return(
    dataset %>% filter(sub_region_1 == region) %>%
    gather(key="movement_catagory", value="percent_change_from_baseline" ,
         -sub_region_1,-sub_region_2, -date) %>%
    mutate(movement_catagory = gsub("_percent_change_from_baseline", "", movement_catagory)))
}

covid_state <- function (dataset, region) {
  return (
    dataset %>%
      filter(state == region) %>%
      group_by(date) %>%
      mutate(total_cases_per_capita = perHundredThousands(sum(cases_seven_day), sum(pop2014))) %>%
      filter(!is.na(total_cases_per_capita))

  )
}

covid_conference <- function (dataset, conferences) {
  return (
    dataset %>%
      filter(Primary_Conference %in% conferences, date >= "2020-08-01") %>%
      group_by(date, Primary_Conference) %>%
      mutate(total_cases_per_capita = perHundredThousands(sum(cases_seven_day), sum(pop2014))) %>%
      filter(!is.na(total_cases_per_capita))
  )
}