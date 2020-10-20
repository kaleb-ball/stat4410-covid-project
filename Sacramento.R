statecases<-read.csv("C:/Users/Joey/Documents/MATH 4410/statewide_cases.csv")
statetests<-read.csv("C:/Users/Joey/Documents/MATH 4410/statewide_testing.csv")
library(ggplot2)
library(plotly)
library(reshape2)
sac_cases<-statecases[statecases$county=="Sacramento",]
sac_cases$date<-as.Date(sac_cases$date)

head(sac_cases)
#State of Emergency- March 4
#Restrictions tightened- March 11
#Senior Home Visits Restricted- March 15
#Statewide shelter-in-place
#Mask Mandate- June 18?
subsac<-subset(sac_cases, select=c(1,2,3,6))
subsac2<-subset(sac_cases, select=c(1,4,5,6))
subsac["percentdead"]<-subsac["totalcountdeaths"]/subsac["totalcountconfirmed"]*100
msac<-melt(subsac, id=c("county", "date"))

ggplot(sac_cases, aes(date, totalcountconfirmed)) + geom_point(size = .5) + 
  geom_vline(xintercept=as.Date("2020-06-18"))
ggplot(sac_cases, aes(date, totalcountconfirmed)) + geom_line() + 
  geom_vline(xintercept=as.Date("2020-06-18")) +
  xlab("Date") + ylab("Total Confirmed Cases")
ggplot(sac_cases, aes(date, totalcountdeaths)) + geom_point() +
  geom_vline(xintercept=as.Date("2020-06-18"))
ggplot(subsac, aes(date, percentdead))+geom_point()+
  geom_vline(xintercept=as.Date("2020-06-18"))
ggplot(subsac2, aes(date, newcountconfirmed))+geom_point()+
  geom_vline(xintercept=as.Date("2020-06-18"))
ggplot(subsac2, aes(date, newcountconfirmed))+geom_line()+
  geom_vline(xintercept=as.Date("2020-06-18"))
