#setwd("~/CPE213DATAMODEL")

#Library setup
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(e1071)
library(chron)
library(corrplot)
library(lubridate)

#Load data + Adjust data
ad <- read.csv("advertising.csv",sep=",") %>% 
  separate('Timestamp',into =c("Date","Time"),sep=" ")
str(ad)

###Exploring Data###
#Checking if Daily time spent on site affect the ad being clicked#
mean(ad$Daily.Time.Spent.on.Site)
hist(ad$Daily.Time.Spent.on.Site)

#Checking peak time of the day
timestamp <-data.frame(date=ad$Date,time=ad$Time)
timestamp<- timestamp %>% 
  separate(time,into =c("Hour","Minute","Second"),sep=":") %>% 
  separate(date,into =c("Year","Month","Day",sep="-"))
timestamp$Hour<-as.numeric(timestamp$Hour)
timestamp$Day<-as.numeric(timestamp$Day)
timestamp$Month<-as.numeric(timestamp$Month)
timestamp$Day
str(timestamp)
hist(timestamp$Hour)

#Checking relation between attributes
temp<- data.frame(DailyTimeSpent=ad$Daily.Time.Spent.on.Site,Age=ad$Age,AreaIncome=ad$Area.Income,DailyInternetUsage=ad$Daily.Internet.Usage,male=ad$Male,clickedad=ad$Clicked.on.Ad)
corrplot(cor(temp))

###Checking specific relation between attributes###
#Daily time spent. vs Daily internet usage
ad %>% 
  ggplot(aes(Daily.Time.Spent.on.Site,Daily.Internet.Usage))+
  geom_point()+
  abline(v=3, col="purple")

#Daily time spent. vs Area income
ad %>% 
  ggplot(aes(Daily.Time.Spent.on.Site,Area.Income))+
  geom_point()

#Daily internet usage vs Area Income
ad %>% 
  ggplot(aes(Daily.Internet.Usage,Area.Income))+
  geom_point()

#Checking if age affected the ad being clicked
toage <- filter(ad,Clicked.on.Ad==1)
mean(toage$Age)
toage %>% 
  ggplot(aes(x=Age))+
  geom_histogram()

#Checking if age affected the ad not being clicked
nottoage <- filter(ad,Clicked.on.Ad==0)
mean(nottoage$Age)
nottoage %>% 
  ggplot(aes(x=Age))+
  geom_histogram()

