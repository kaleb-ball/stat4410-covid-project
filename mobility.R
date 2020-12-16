library(dplyr)
library(tidyverse)
library(ggplot2)
library(XML)
library(RCurl)
library(rvest)
library(zoo)
library(hrbrthemes)
library(viridis)

source("common-covid-data.R")
source("functions.R")


us_mobility <- read.csv("us-mobility.csv")
us_counties_covid <- us_counties_covid %>%
  select(state, county, cases_seven_day, deaths_seven_day, date, pop2014)

us_mobility <- us_mobility %>%
  select(sub_region_1, sub_region_2, date,
         retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline) %>%
  mutate(sub_region_2 = toupper(gsub(" County", "", sub_region_2))) %>%
  mutate(date = as.Date(date))

us_covid_per_capita <- us_counties_covid %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(total_cases_per_capita = perHundredThousands(sum(cases_seven_day), sum(pop2014))) %>%
  filter(!is.na(total_cases_per_capita))

facet_labels <- c(grocery_and_pharmacy="Grocery and Pharamacy",
            parks="Parks",
            residential="Residential",
            retail_and_recreation= "Retail and Recreation",
            transit_stations = "Transit Stations",
            workplaces = "Workplaces")

percentChangeColor <- "#69b3a2"
casesColor <- rgb(0.2, 0.6, 0.9, 1)

national_mobility_covid_plot <- ggplot() +
  geom_line(us_covid_per_capita, mapping = aes(as.Date(date), total_cases_per_capita), size=.75, color=casesColor) +
  geom_line(mobility_region(us_mobility, ""),  mapping = aes(date, percent_change_from_baseline), size=.75, color=percentChangeColor) +
  xlab("Date") +
  scale_y_continuous(
    name = "Percent Change in Movement from Baseline",
    sec.axis = sec_axis(~.,name="Cases Per Hundred Thousand")) +
  facet_wrap(~movement_catagory,  labeller=labeller(movement_catagory = facet_labels)) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = percentChangeColor, size=15),
    axis.title.y.right = element_text(color = casesColor, size=15),
    axis.title.x = element_text(size=15, hjust = .5)
  ) +
  ggtitle("National Cases and Changes in Movement")

