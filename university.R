library(dplyr)
library(tidyverse)
library(ggplot2)
library(XML)
library(RCurl)
library(rvest)
library(zoo)
library(hrbrthemes)
library(viridis)
library(gghighlight)


#setwd("/Users/kalebball/Desktop/Fall 2020/STAT 4410 - Introduction to Data Science/Project/stat4410-covid-project/")
source("functions.R")
source("common-covid-data.R")

#Import and Clean Data
wikiPage <- read_html("https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_institutions")
d1_counties <- wikiPage %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  select("School", "City", "State", "Enrollment", "Primary conference") %>%
  rename(Primary_Conference= "Primary conference") %>%
  left_join(us_city_state_county, by = c("City", "State"="State.full"))

d1_counties_covid <- full_join(d1_counties, us_counties_covid, by=c("County"="county", "State"="state"))

d1_vs_other_covid <- us_counties_covid %>%
  mutate(has_d1 = county %in% d1_counties$County)

#Divison 1 Univerity Counties vs Other Counties Plots

confColor <- "#69b3a2"
nationalColor <- rgb(0.2, 0.6, 0.9, 1)



d1_vs_not_cases_per_capita_plot <- d1_vs_other_covid %>%
  group_by(date, has_d1) %>%
  mutate(total_cases_per = perHundredThousands(sum(cases_seven_day), sum(pop2014))) %>%
  filter(!is.na(total_cases_per)) %>%
  mutate(has_d1 = factor(has_d1)) %>%
  mutate(has_d1 = recode_factor(has_d1, `TRUE`="Yes", `FALSE`="No")) %>%
  ggplot(aes(date, total_cases_per, color=has_d1)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_ipsum() +
  ggtitle("Per Capita Cases In Counties By \nPresence of a Major University") +
  xlab("Date") +
  ylab("Daily Cases Per Hundred Thousand") +
  labs(color="Has D1 Univeristy") +
  scale_color_viridis(discrete = TRUE, begin=.25, end=.75,option = "A") +
  theme(
    axis.title.y = element_text(size=10, vjust = .5),
    axis.title.x = element_text(size=10, hjust = .5)
  )


d1_vs_not_cases_total_plot <- d1_vs_other_covid %>%
  group_by(date, has_d1) %>%
  summarise(total_cases = sum(cases_seven_day)) %>%
  mutate(has_d1 = factor(has_d1)) %>%
  mutate(has_d1 = recode_factor(has_d1, `TRUE`="Yes", `FALSE`="No")) %>%
  ggplot(aes(date, total_cases, color=has_d1)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_ipsum() +
  ggtitle("Total Cases In Counties By \nPresence of a Major University") +
  xlab("Date") +
  ylab("Total Daily Cases") +
  labs(color="Has D1 Univeristy") +
  scale_color_viridis(discrete = TRUE, begin=.25, end=.75,option = "A") +
  theme(
    axis.title.y = element_text(size=10, vjust = .5),
    axis.title.x = element_text(size=10, hjust = .5)
  )


d1_vs_not_deaths_per_capita_plot <- d1_vs_other_covid %>%
  group_by(date, has_d1) %>%
  mutate(total_deaths_per = perHundredThousands(sum(deaths_seven_day), sum(pop2014))) %>%
  filter(!is.na(total_deaths_per)) %>%
  mutate(has_d1 = factor(has_d1)) %>%
  mutate(has_d1 = recode_factor(has_d1, `TRUE`="Yes", `FALSE`="No")) %>%
  ggplot(aes(date, total_deaths_per, color=has_d1)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_ipsum() +
  ggtitle("Per Capita Deaths In Counties By \nPresence of a Major University") +
  xlab("Date") +
  ylab("Daily Deaths Per Hundred Thousand") +
  labs(color="Has D1 Univeristy") +
  scale_color_viridis(discrete = TRUE, begin=.25, end=.75,option = "A") +
  theme(
    axis.title.y = element_text(size=10, vjust = .5),
    axis.title.x = element_text(size=10, hjust = .5)
  )


d1_vs_not_deaths_total_plot <- d1_vs_other_covid %>%
  group_by(date, has_d1) %>%
  summarise(total_deaths = sum(deaths_seven_day)) %>%
  mutate(has_d1 = factor(has_d1)) %>%
  mutate(has_d1 = recode_factor(has_d1, `TRUE`="Yes", `FALSE`="No")) %>%
  ggplot(aes(date, total_deaths, color=has_d1)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_ipsum() +
  ggtitle("Total Deaths In Counties By \nPresence of a Major University") +
  xlab("Date") +
  ylab("Total Daily Deaths") +
  labs(color="Has D1 Univeristy") +
  scale_color_viridis(discrete = TRUE, begin=.25, end=.75,option = "A") +
  theme(
    axis.title.y = element_text(size=10, vjust = .5),
    axis.title.x = element_text(size=10, hjust = .5)
  )


national_cases_per_capita <- us_counties_covid %>%
  ungroup() %>%
  filter(date >= "2020-08-01") %>%
  group_by(date) %>%
  summarise(total_cases_per_capita = perHundredThousands(sum(cases_seven_day), sum(pop2014))) %>%
  mutate(Primary_Conference = "National") %>%
  filter(!is.na(total_cases_per_capita))

regular_start_p5 <- c("Big 12 Conference", "Southeastern Conference", "Atlantic Coast Conference")
late_start_p5 <- c("Big Ten Conference", "Pac-12 Conference", "Big East Conference")

regular_start_p5_plot <- d1_counties_covid %>%
  covid_conference(regular_start_p5) %>%
  select("Primary_Conference", "date", "total_cases_per_capita") %>%
  bind_rows(national_cases_per_capita) %>%
  mutate(National = ifelse(Primary_Conference == "National", T, F)) %>%
  mutate(Primary_Conference = factor(Primary_Conference)) %>%
  mutate(Primary_Conference = recode_factor(Primary_Conference, `1`="National", `2`="Atlantic Coast Conference", `3`="Big 12 Conference", `4`="Southeastern Conference")) %>%
  ggplot(aes(date, total_cases_per_capita, group=Primary_Conference, alpha=National)) +
  geom_line(aes(color=Primary_Conference), size=1.5) +
  theme_ipsum() +
  scale_color_viridis(begin=0, end=.8, discrete = TRUE, option = "C") +
  scale_alpha_manual(values = c(.2,2), guide= 'none') +
  ggtitle("Per Capita Cases for Major Confrences\nwith August Football") +
  xlab("Date") +
  ylab("Cases Per Hundred Thousand") +
  labs(color="") +
  theme(
    axis.title.y = element_text(size=10, hjust = .5),
    axis.title.x = element_text(size=10, hjust = .5)
  )

late_start_p5_plot <- d1_counties_covid %>%
  covid_conference(late_start_p5) %>%
  select("Primary_Conference", "date", "total_cases_per_capita") %>%
  bind_rows(national_cases_per_capita) %>%
  mutate(National = ifelse(Primary_Conference == "National", T, F)) %>%
  mutate(Primary_Conference = factor(Primary_Conference)) %>%
  mutate(Primary_Conference = recode_factor(Primary_Conference, `1`="National", `2`="Big East Conference", `3`="Big Ten Conference", `4`="Pac-12 Conference")) %>%
  ggplot(aes(date, total_cases_per_capita, group=Primary_Conference, alpha=National)) +
  geom_line(aes(color=Primary_Conference), size=1.25) +
  theme_ipsum() +
  scale_color_viridis(begin=0, end=.8, discrete = TRUE, option = "A") +
  scale_alpha_manual(values = c(.2,2), guide= 'none') +
    ggtitle("Per Capita Cases for Major Confrences\nwithout August Starting Football") +
  xlab("Date") +
  ylab("Cases Per Hundred Thousand") +
  labs(color="") +
  theme(
    axis.title.y = element_text(size=10, hjust = .5),
    axis.title.x = element_text(size=10, hjust = .5)
  ) +
  geom_vline(xintercept=as.Date("2020-10-24")) +
  geom_text(aes(x=as.Date("2020-10-24"), y=75,
                label="Big Ten Football Start",
                family = "Arial Narrow"),
            angle=90,size=3.5, vjust=-1) +
  geom_vline(xintercept=as.Date("2020-11-7")) +
  geom_text(aes(x=as.Date("2020-11-7"), y=75,
                label="Pac-12 Football Start",
                family = "Arial Narrow"),
            angle=90,size=3.5, vjust=-1)

