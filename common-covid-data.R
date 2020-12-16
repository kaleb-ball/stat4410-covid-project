library(dplyr)
library(tidyverse)
library(ggplot2)
library(XML)
library(RCurl)
library(rvest)
library(zoo)

# setwd("/Users/kalebball/Desktop/Fall 2020/STAT 4410 - Introduction to Data Science/Project/stat4410-covid-project/")

us_city_state_county <- read.csv("https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv", sep="|")
us_counties_covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  mutate(county=toupper(county))
us_counties_pop <- read.csv("https://data.nber.org/census/popest/county_population.csv") %>%
  select(state_name, county_name, pop2014) %>%
  mutate(county_name = toupper(gsub(" County", "", county_name))) %>%
  mutate(county_name = gsub("^\\s+|\\s+$", "", county_name)) %>%
  mutate(state_name = gsub("^\\s+|\\s+$", "", state_name)) %>%
  na.omit()

us_counties_covid <- full_join(us_counties_covid, us_counties_pop, by=c("county" = "county_name", "state"="state_name"))

us_counties_covid <- us_counties_covid %>%
  group_by(county, state) %>%
  mutate(
    date = as.Date(date),
    daily_cases = c(cases[1],diff(cases)),
    daily_deaths = c(deaths[1], diff(deaths)),
    cases_seven_day = zoo::rollmean(daily_cases, k=7, fill = NA),
    deaths_seven_day = zoo::rollmean(daily_deaths, k=7, fill = NA)) %>%
  na.omit()

us_city_state_county <- us_city_state_county %>%
  select(City, State.full, County) %>%
  distinct()

remove(us_counties_pop)
