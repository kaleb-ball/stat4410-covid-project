library(dplyr)
library(reshape2)
library(ggplot2)
#March 16 - stay at home order
#April 17 - face cover policy
#May 29 - masks required outside

ca_cases <- filter(statewide_cases, county == "San Francisco")
ca_cases$date <- as.Date(ca_cases$date)
ca_cases_mask <- filter(ca_cases, date > "2020-05-30")
ca_cases_no_mask <- filter(ca_cases, date <= "2020-05-30")
ggplot(ca_cases_mask, aes(date, totalcountconfirmed)) + geom_point(size = 1)
ggplot(ca_cases_no_mask, aes(date, totalcountconfirmed)) + geom_point(size = 1) 


case_demographics_age$date <- as.Date(case_demographics_age$date)
ca_age <- melt(case_demographics_age, id = c("date", "totalpositive", "age_group"))
ca_age_death <- melt(case_demographics_age, id = c("date", "deaths", "age_group"))
ca_age_death_percent <- melt(case_demographics_age, id = c("date", "deaths_percent", "age_group"))
ggplot(ca_age, aes(date, totalpositive)) + geom_point(size = 1) + facet_wrap(~ age_group)
ggplot(ca_age_death, aes(date, deaths)) + geom_point(size = 1) + facet_wrap(~ age_group)
ggplot(ca_age_death_percent, aes(date, deaths_percent)) + geom_point(size = 1) + facet_wrap(~ age_group) 


case_demographics_sex$date <- as.Date(case_demographics_sex$date)
ca_sex <- melt(case_demographics_sex, id = c("date", "totalpositive2", "sex"))
ggplot(ca_sex, aes(date, totalpositive2)) + geom_point() + facet_wrap(~ sex) + geom_smooth(method = lm)


case_demographics_ethnicity$date <- as.Date(case_demographics_ethnicity$date)
ca_ethnicity <- melt(case_demographics_ethnicity, id = c("date", "cases", "race_ethnicity"))
ggplot(ca_ethnicity, aes(date, cases)) + geom_line() + facet_wrap(~ race_ethnicity)



COVID.19_Cases_Summarized_by_Date__Transmission_and_Case_Disposition$Specimen.Collection.Date <- as.Date(COVID.19_Cases_Summarized_by_Date__Transmission_and_Case_Disposition$Specimen.Collection.Date)
sa_cases <- melt(COVID.19_Cases_Summarized_by_Date__Transmission_and_Case_Disposition, id = c("Specimen.Collection.Date", "Case.Count"))
ggplot(sa_cases, aes(Specimen.Collection.Date, Case.Count))+ geom_point(size = .5) 
  + geom_vline(xintercept = as.Date("2020-03-16")) + geom_vline(xintercept = as.Date("2020-05-17"))


COVID.19__Cases_Summarized_by_Race_and_Ethnicity$Specimen.Collection.Date <- as.Date(COVID.19__Cases_Summarized_by_Race_and_Ethnicity$Specimen.Collection.Date)
sa_cases_eth <- melt(COVID.19__Cases_Summarized_by_Race_and_Ethnicity, id = c("Specimen.Collection.Date", "Cumulative.Confirmed.Cases", "Race.Ethnicity"))
sa_cases_eth2 <- melt(COVID.19__Cases_Summarized_by_Race_and_Ethnicity, id = c("Specimen.Collection.Date", "New.Confirmed.Cases", "Race.Ethnicity"))
ggplot(sa_cases_eth, aes(Specimen.Collection.Date, Cumulative.Confirmed.Cases)) + geom_point(size = .5) + facet_wrap(~ Race.Ethnicity) + geom_vline(xintercept = as.Date("2020-03-16")) + geom_vline(xintercept = as.Date("2020-05-17"))
ggplot(sa_cases_eth2, aes(Specimen.Collection.Date, New.Confirmed.Cases)) + geom_point(size = .5) + facet_wrap(~ Race.Ethnicity) + geom_vline(xintercept = as.Date("2020-03-16")) + geom_vline(xintercept = as.Date("2020-05-17"))


COVID.19_Cases_Summarized_by_Age_Group$Specimen.Collection.Date <- as.Date(COVID.19_Cases_Summarized_by_Age_Group$Specimen.Collection.Date)       
sa_cases_age <- melt(COVID.19_Cases_Summarized_by_Age_Group, id = c("Specimen.Collection.Date", "Cumulative.Confirmed.Cases", "Age.Group"))
sa_cases_age2 <- melt(COVID.19_Cases_Summarized_by_Age_Group, id = c("Specimen.Collection.Date", "New.Confirmed.Cases", "Age.Group"))
ggplot(sa_cases_age, aes(Specimen.Collection.Date, Cumulative.Confirmed.Cases)) + geom_point(size = .5) + facet_wrap(~ Age.Group) + geom_vline(xintercept = as.Date("2020-03-16")) + geom_vline(xintercept = as.Date("2020-05-17"))
ggplot(sa_cases_age2, aes(Specimen.Collection.Date, New.Confirmed.Cases)) + geom_point(size = .5) + facet_wrap(~ Age.Group) + geom_vline(xintercept = as.Date("2020-03-16")) + geom_vline(xintercept = as.Date("2020-05-17")) + geom_vline(xintercept = as.Date("2020-05-29"))




