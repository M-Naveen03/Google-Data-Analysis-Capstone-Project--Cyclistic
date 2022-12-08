---
title: "Google Data Analytics Capstone project"
author: "Naveen M"
date: "2022-12-07"
output:
  html_document: default
subtitle: Cyclistic Case Study
---

###Introduction

hi everyone, this case study about Cyclistic company user analysis for my Google Data Analytics certificate. in this case study, I work as an Junior Data Analyst for a fictional company called Cyclistic.In this case study, I choose the data of Cyclistic company from 2021 january to 2021 december (12 months). to answer the required question i'll follow the six steps of data analyzing process from my course: Ask, Prepare,Process, Analyze, Share & Act.

###The Scenario

The director of marketing of Cyclistic, Lily Moreno, believes that the future of the company's growth is based on increasing the number of members.

Three questions will guide the future marketing campaign: 

1.How do annual members and casual riders use Cyclistic bikes differently? 

2.Why would casual riders buy Cyclistic annual membership?

 3.How can Cyclistic use digital media to influence casual riders to become members?

###The Ask Phase

Business Task: Cyclistic has concluded that casual riders are less profitrable than annual riders. so, the task is to change the strategy about targeting the casual riders to become members.

*Key stakeholders: 

1. Lily Moreno: The director of marketing and my manager. 

2. The executive team: they must approve the recommendations by the team.

###The Prepare Phase

Data Source: 12 month of bike share data from 01/01/2021 to 31/12/2021 was downloaded from files. The data licensed by Motivate International Inc Licence.

Data Credibility and Data Security are checked both are valid.

###The Process Phase

I used R for data verification and cleaning. Reasons: 1.Excel Worksheet have limitations in number of rows. 2.SQL needs extra cloud storage. i have only cloud console it shows size problems in big query.

###The Analyse Phase

####Setting up the environment

Here, I use several libraries that help reading, cleaning, organizing and analyzing the data.

```{r libraries, echo=TRUE, eval=TRUE}
library(readr)
library(dplyr)
library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
```

####Importing data 
Cyclistic data from 01/2021 to 12/2021 is imported and read as csv. files and combine all the datas into single big data frame

```{r read, echo=TRUE, eval=FALSE}

data_01 <- read.csv("G:/1.naveen/2021/202101-cyclistic-tripdata.csv")
data_02 <- read.csv("G:/1.naveen/2021/202102-cyclistic-tripdata.csv")
data_03 <- read.csv("G:/1.naveen/2021/202103-cyclistic-tripdata.csv")
data_04 <- read.csv("G:/1.naveen/2021/202104-cyclistic-tripdata.csv")
data_05 <- read.csv("G:/1.naveen/2021/202105-cyclistic-tripdata.csv")
data_06 <- read.csv("G:/1.naveen/2021/202106-cyclistic-tripdata.csv")
data_07 <- read.csv("G:/1.naveen/2021/202107-cyclistic-tripdata.csv")
data_08 <- read.csv("G:/1.naveen/2021/202108-cyclistic-tripdata.csv")
data_09 <- read.csv("G:/1.naveen/2021/202109-cyclistic-tripdata.csv")
data_10 <- read.csv("G:/1.naveen/2021/202110-cyclistic-tripdata.csv")
data_11 <- read.csv("G:/1.naveen/2021/202111-cyclistic-tripdata.csv")
data_12 <- read.csv("G:/1.naveen/2021/202112-cyclistic-tripdata.csv")




trips <- bind_rows( data_01, data_02, data_03, data_04, data_05, data_06, data_07, data_08, data_09, data_10, data_11, data_12)
```

####Clean up and organize data to prepare for analysis

change into integer from character and check structure

```{r int, echo=TRUE, eval=TRUE}
options(warn=-1)
trips <- mutate(trips, start_station_id = as.integer(start_station_id))
trips <- mutate(trips, end_station_id = as.integer(end_station_id))

str(trips)
```
Columns that list the date, month, day, day_of_week are added.

```{r date, echo=TRUE, eval=TRUE}
trips$date <- as.Date(trips$started_at)
trips$month <- format(as.Date(trips$date), "%B")
trips$day<- format(as.Date(trips$date), "%d")
trips$day_of_week<-format(as.Date(trips$date), "%A")
```
Check for NA's and remove duplicates

```{r NA, echo=TRUE, eval=TRUE}
trips <- drop_na(trips)

trips_no_duplicates <- trips[!duplicated(trips$ride_id), ]
print(paste("Removed", nrow(trips) - nrow(trips_no_duplicates), "duplicate rows"))
```
Add a new "ride_length" calculation in minutes and check structure

```{r ride_length, echo=TRUE, eval=TRUE}
trips_2021 <- mutate(trips_no_duplicates, ride_length= difftime(ended_at, started_at, units= "mins"))
str(trips_2021)
head(trips_2021)
```
Check for the incorrect ride_length and view a glimpse.

```{r check, echo=TRUE, eval=TRUE}
nrow(trips_2021[trips_2021$ride_length<=0, ])
all_trips_2021 <- trips_2021[!trips_2021$ride_length <=0, ]
glimpse(all_trips_2021)
```
To calculate how many members and casual riders.

```{r rider_type, echo=TRUE, eval=TRUE}
total_rider_type <- table(all_trips_2021$member_casual)
View(total_rider_type)
```
####First analysis step: minimum ride length, maximum ride length, mean ride length calculated.

```{r echo=TRUE, eval=TRUE}
trips_data<- all_trips_2021 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length= mean(ride_length), min_ride_length= min(ride_length), max_ride_length= max(ride_length))
head(trips_data)
```
The mode of weekday is calculated for checking when most bike rented on which weekday.

```{r plot1: mode of the week_day, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_2021 %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(mapping = aes(x= day_of_week, y=number_of_rides, color="red")) + geom_col()
```
This plot shows most of the rides were on Saturday and Sunday followed by Friday.

plot day_of_week vs average_duration

```{r plot2: ride_length per day, echo=TRUE, eval=TRUE, error=TRUE}         
all_trips_2021 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),
  average_duration=mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x=day_of_week, y= average_duration, fill= member_casual)) + geom_col(position = "dodge")
```
this plot explains that casual riders rent bikes for longer duration than members.

next, this plot is about number of rides per day for every rider

```{r plot3: number of rides per day, echo=TRUE, eval=TRUE, error=TRUE}
  all_trips_2021 %>% 
   group_by(member_casual, day_of_week) %>% 
   summarise(number_of_rides = n(),
   average_duration=mean(ride_length)) %>% 
   arrange(member_casual, day_of_week) %>% 
   ggplot(aes(x=day_of_week, y= number_of_rides, fill= member_casual)) + geom_col(position = "dodge")
```
From above plot casual riders rides more than members on weekends.

this plot explains the average ride length of members and casual riders.

```{r plot4: mean ride length and riders, echo=TRUE, eval=TRUE, error=TRUE}
 all_trips_2021 %>% 
   group_by(member_casual) %>% 
   summarise(max(ride_length), min(ride_length), avg_ride_length= mean(ride_length))%>% 
   ggplot(aes(x=member_casual, y= avg_ride_length, fill= member_casual)) + geom_col()+ scale_y_continuous(breaks= seq(0, 40, by=5))
```
To check the most popular month in the year

```{r popular month, echo=TRUE, eval=TRUE, error=TRUE}
 popular_month<- all_trips_2021 %>% 
   group_by(month) %>% 
   summarise(number_of_rides= n(), average_duration= mean(ride_length)) %>% 
   arrange(-number_of_rides)
View(popular_month)
```
This shows July months has more number of rides followed August

To view overall rider count by rider type

```{r rider count, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_2021 %>% 
  group_by(member_casual) %>% 
  summarise(rider_count = n()) %>% 
  ggplot(aes(x= member_casual, y = rider_count, fill= member_casual))+ 
  geom_col()
```
To view the whole year rider data by pointing for continuous changes of rides.

```{r plot5: number of rides along the whole year, echo=TRUE, eval=TRUE, error=TRUE}
all_trips_2021 %>% 
  group_by(month, member_casual) %>% 
  summarise(number_of_rides = n(), avg_ride_length = mean(ride_length)) %>% 
  ggplot() + geom_point(mapping= aes(x = month, y= number_of_rides, color = member_casual)) 
```
###The Share Phase

####Conclusions/Summary of Insights

Difference between members and casual riders.

Most number of rides happened in Saturday and Sunday followed by Friday (Plot 1). May be because of weekend tourism and holidays.

Casual members average ride duration is higher than members(plot2). members use bikes for daily works but casual riders rides for unplanned works, sight seeing etc.,

On an average, members rides constantly on same pace on everyday but casual riders rides high on weekends (plot3). members rides for office use or daily use etc so they ride on same paces.

Average ride length for casual rides are higher than members (plot 4)

On monthly view, if highest rides shows in a month than casual riders may be high on that month (plot5). some months like June,July,August seems high rides. it is may be festival or big events.

####Recommendations

Give reward like coupons, vouchers on continuous rides and memberships.

Marketing campaign: 

* The campaign should be launched during July to August because it shows peak rides.

*Weekend is best option to reach casual riders because they took more rides on weekends.
