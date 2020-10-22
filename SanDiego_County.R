library(dplyr)
library(lubridate)
library(magrittr)
library(geojsonio)
library(ggplot2)

csv <- read.csv("COVID_19_Statistics_San_Diego_County.csv")
cols<-c("date", "test", "positives", "hospitalized", "icu", "deaths", "newcases", "newtests", "rolling_perc_pos_cases")
san_diego_covid <- csv[, (names(csv) %in% cols)]
san_diego_covid$date <- as.Date(gsub(' \\d+:\\d+:\\d+[\\+]\\d+', '', san_diego_covid$date),"%Y/%m/%d")

san_diego_covid %<>% 
  mutate(newdeaths = deaths - lag(deaths)) %>%
  filter(newdeaths > 0)

totalCases <- ggplot(san_diego_covid, aes(date, positives))+geom_point(size=1) +
  xlab("Date") + ylab("Positive Percent") + ggtitle("San Diego County: Cumulative Cases") +
  geom_vline(xintercept=as.Date("2020-05-01")) +
  geom_text(aes(x=as.Date("2020-05-01"), y=9, 
                label="San Diego County Mask Mandate"), 
            angle=90, vjust=1.2, size=3.5) +
  geom_text(aes(x=as.Date("2020-05-01"), 
                label="May 1", y=9), 
            angle=90, vjust = -1.2, size=3.5) +
  theme_bw() 

newCases <- ggplot(san_diego_covid, aes(date, newcases)) + geom_point(size=1) +
  xlab("Date") + ylab("New Cases") + ggtitle("San Diego County: Daily Cases") +
  geom_vline(xintercept=as.Date("2020-05-01")) +
  geom_text(aes(x=as.Date("2020-05-01"), y=600, 
                label="San Diego County Mask Mandate"), 
            angle=90, vjust=1.2, size=3.5) +
  geom_text(aes(x=as.Date("2020-05-01"), 
                label="May 1", y=600), 
            angle=90, vjust = -1.2, size=3.5) +
  theme_bw()
    

positiveRate <- ggplot(san_diego_covid, aes(date, rolling_perc_pos_cases))+geom_point(size=1) +
  xlab("Date") + ylab("Positive Percent") + ggtitle("San Diego County: Positve Test Percent (Seven Day Rolling Average)") +
  geom_vline(xintercept=as.Date("2020-05-01")) +
  geom_text(aes(x=as.Date("2020-05-01"), y=9, 
                label="San Diego County Mask Mandate"), 
            angle=90, vjust=1.2, size=3.5) +
  geom_text(aes(x=as.Date("2020-05-01"), 
                label="May 1", y=9), 
            angle=90, vjust = -1.2, size=3.5) +
  ylim(2,10) + theme_bw() 


totalDeaths <- ggplot(san_diego_covid, aes(date, deaths))+geom_point(size=1) +
  xlab("Date") + ylab("Deaths") + ggtitle("San Diego County: Cumulative Deaths") +
  geom_vline(xintercept=as.Date("2020-05-01")) +
  geom_text(aes(x=as.Date("2020-05-01"), y=800, 
                label="San Diego County Mask Mandate"), 
            angle=90, vjust=1.2, size=3.5) +
  geom_text(aes(x=as.Date("2020-05-01"), 
                label="May 1", y=800), 
            angle=90, vjust = -1.2, size=3.5) +
  theme_bw() 

newDeaths <- ggplot(san_diego_covid, aes(date, newdeaths))+geom_point(size=1) +
  xlab("Date") + ylab("Deaths") + ggtitle("San Diego County: New Deaths") +
  geom_vline(xintercept=as.Date("2020-05-01")) +
  geom_text(aes(x=as.Date("2020-05-01"), y=20, 
                label="San Diego County Mask Mandate"), 
            angle=90, vjust=1.2, size=3.5) +
  geom_text(aes(x=as.Date("2020-05-01"), 
                label="May 1", y=20), 
            angle=90, vjust = -1.2, size=3.5) +
  theme_bw() 

print(totalCases)
print(newCases)
print(positiveRate)
print(totalDeaths)
print(newDeaths)
