library(dplyr)
library(reshape2)
library(ggplot2)


sf_cases <- filter(statewide_cases, county == "San Francisco")
sf_cases$date <- as.Date(sf_cases$date)
sf_cases_mask <- filter(sf_cases, date > "2020-05-30")
sf_cases_no_mask <- filter(sf_cases, date <= "2020-05-30")
ggplot(sf_cases_mask, aes(date, totalcountconfirmed)) + geom_point(size = 1)
ggplot(sf_cases_no_mask, aes(date, totalcountconfirmed)) + geom_point(size = 1) 


case_demographics_age$date <- as.Date(case_demographics_age$date)
sf_age <- melt(case_demographics_age, id = c("date", "totalpositive", "age_group"))
sf_age_death <- melt(case_demographics_age, id = c("date", "deaths", "age_group"))
sf_age_death_percent <- melt(case_demographics_age, id = c("date", "deaths_percent", "age_group"))
ggplot(sf_age, aes(date, totalpositive)) + geom_point(size = 1) + facet_wrap(~ age_group) + geom_vline(xintercept = as.Date("2020-04-17"))
ggplot(sf_age_death, aes(date, deaths)) + geom_point(size = 1) + facet_wrap(~ age_group) + geom_vline(xintercept = as.Date("2020-04-17")) + geom_vline(xintercept = as.Date("2020-05-30"))
ggplot(sf_age_death_percent, aes(date, deaths_percent)) + geom_point(size = 1) + facet_wrap(~ age_group) + geom_vline(xintercept = as.Date("2020-04-17")) + geom_vline(xintercept = as.Date("2020-05-30"))


case_demographics_sex$date <- as.Date(case_demographics_sex$date)
sf_sex <- melt(case_demographics_sex, id = c("date", "totalpositive2", "sex"))
ggplot(sf_sex, aes(date, totalpositive2)) + geom_point() + facet_wrap(~ sex) + geom_smooth(method = lm)


case_demographics_ethnicity$date <- as.Date(case_demographics_ethnicity$date)
sf_ethnicity <- melt(case_demographics_ethnicity, id = c("date", "cases", "race_ethnicity"))
ggplot(sf_ethnicity, aes(date, cases)) + geom_line() + facet_wrap(~ race_ethnicity)

