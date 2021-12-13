######################################################
###  Google Data Analytics Certificate Case Study  ###
###                  Cyclistic                     ###
### How does a bike-share navigate speedy success? ###
######################################################

## author: Austin Seo
## data made available by: Motivate International Inc.


# import packages

#install.packages("here")
#install.packages("janitor")
#install.packages("skimr")
#install.packages("maptools")
#install.packages("mapproj")
#install.packages("mapdata")
#install.packages("ggmap")

# load packages
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(readr)
library(plyr)
library(lubridate)
library(maptools)
library(mapproj)
library(mapdata)
library(ggmap)

# import data
all_trip_data <- list.files(path = "./CSV", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv, na.omit) %>% 
  bind_rows
#head(all_trip_data)

# create data frame
all_trips_df <- data.frame(all_trip_data)
#head(all_trips_df)
#colnames(all_trips_df)

# rename columns
names(all_trips_df) [1:13] <- c("ride_id", "rideable_type", "started_at", "ended_at", "start_station_name", 
                                "start_station_id", "end_station_name", "end_station_id", "start_lat", "start_lng", 
                                "end_lat", "end_lng", "member_casual")

# first row is what was supposed to be the colnames
# need to drop first row of table
all_trips_df <- all_trips_df[-c(1),]
#head(all_trips_df)

# drop NA rows
all_trips_df <- all_trips_df %>% 
  drop_na() # drops rows with NA

# filter for testing location -> create new df containing test trips
test_trips_df <- all_trips_df %>% 
  filter(start_station_name == "WATSON TESTING - DIVVY")

  
# filter out test trips from all_trips_df
all_trips_df <- all_trips_df %>% 
  filter(start_station_name != "WATSON TESTING - DIVVY") # seemed to be a test location for the company, decided to filter out

# convert data types
all_trips_df <- all_trips_df %>% 
  mutate(started_at = ymd_hms(started_at), # formats date as ymd hms
         ended_at = ymd_hms(ended_at), 
         start_station_id = as.numeric(start_station_id),
         end_station_id = as.numeric(end_station_id),
         start_lat = as.numeric(start_lat),
         start_lng = as.numeric(start_lng),
         end_lat = as.numeric(end_lat),
         end_lng = as.numeric(end_lng),
         ride_length_min = round(as.numeric(ended_at-started_at), digits = 2), # returns the diff in time in sec
         day_of_week = weekdays(started_at)) # retrieves day of the week that the ride started on
  

# change ride_length_min to min instead of sec
all_trips_df <- all_trips_df %>% 
  mutate(ride_length_min = round(as.numeric((ended_at-started_at)/60), digits = 2)) # returns diff in min

  

## Business problem: Cyclistic is looking to increase profits, to do so they will need to
##                   convert casual customers to annual members.

### Question: How do annual members and casual riders use Cyclistic bikes differently? ###



summary(all_trips_df) # returns negative min ride length -> 

all_trips_df <- all_trips_df %>% 
  filter(ride_length_min > 0) # filter out rides less than 0 minutes

# create df for casual riders
casual_trips_df <- all_trips_df %>% # data frame of rides for casual riders
  filter(member_casual == "casual")

#summary(casual_trips_df) # avg ride length 32.92 min

# create df for annual members
annual_member_trips_df <- all_trips_df %>% # data frame of rides for annual members
  filter(member_casual == "member")

#summary(annual_member_trips_df) # avg ride length 13.52 min


### not sure if i will keep these dfs and respective plots
# df to show weekend activity of casual riders
casual_weekend_df <- casual_trips_df %>% 
  filter(ride_length_min < 1500, # filtering for rides under 24hrs long
         day_of_week == c("Sunday","Saturday")) # filter for only saturday and sunday

# df to show weekday activity of casual riders
casual_weekday_df <- casual_trips_df %>% 
  filter(ride_length_min < 1500, # filtering for rides less than 24 hr in length
         day_of_week != "Saturday" & day_of_week != "Sunday") %>% # filters out weekend days, returns weekdays
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) # days of the week had no natural order resulting in being printed out of order on the graph, this fixes that

# bar chart comparing total number of rides on Sat vs Sun over 1 year
casual_weekend_plot <- ggplot(data = casual_weekend_df, mapping = aes(x=day_of_week)) +
  geom_bar(aes(fill = day_of_week)) +
  labs(
    title = "Number of Casual Rides on Weekends",
    subtitle = "October 2020 - October 2021",
    x = "Day of the Week",
    y = "Number of Rides"
  )

casual_weekday_plot <- ggplot(data = casual_weekday_df, mapping = aes(x=day_of_week)) +
  geom_bar(aes(fill = day_of_week)) +
  labs(
    title = "Number of Casual Rides on Weekdays",
    subtitle = "October 2020 - October 2021",
    x = "Day of the Week",
    y = "Number of Rides",
    fill = "Day of the Week"
  )
#casual_weekday_plot # days of the week are out of order


### definitely keeping this df and plot
casual_weekly_df <- casual_trips_df %>% # create df to represent entire week
  filter(ride_length_min < 1500, # filter out rides longer than 24hrs
         year(started_at) > "2020") %>% # filter out 2020 data
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), # create a hierarchy of weekdays
         month = month(started_at)) # create column representing the month the ride was in

casual_weekly_plot <- ggplot(data = casual_weekly_df, mapping = aes(x=day_of_week)) +
  geom_bar(aes(fill = day_of_week)) + # color for each weekday
  labs(
    title = "Number of Weekly Casual Rides",
    subtitle = "January 2021 - October 2021",
    x = "Day of the Week",
    y = "Number of Rides",
    fill = "Day of the Week"
  ) +
  facet_wrap(~month) # create a weekly plot for each month
#casual_weekly_plot # activity is highest during late spring, summer, early fall, and weekends

# create df of weekly rides less than 24 hrs in length for members
member_weekly_df <- annual_member_trips_df %>% 
  filter(ride_length_min < 1500, # filter out rides longer than 24hrs
         year(started_at) > "2020") %>% # filter out 2020 data
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), # create a hierarchy of weekdays
         month = month(started_at)) # create column representing the month the ride was in

# create bar chart showing the amount of rides per week per month for members in 2021
member_weekly_plot <- ggplot(data = member_weekly_df, mapping = aes(x=day_of_week)) +
  geom_bar(aes(fill = day_of_week)) + # color for each weekday
  labs(
    title = "Number of Weekly Member Rides",
    subtitle = "January 2021 - October 2021",
    x = "Day of the Week",
    y = "Number of Rides",
    fill = "Day of the Week"
  ) +
  facet_wrap(~month) # create a weekly plot for each month
#member_weekly_plot

states_map <- map_data("state", region = "illinois")

p <- ggplot() + coord_fixed() + xlab("") + ylab("")

base_us <- p +
  geom_polygon(data = states_map, aes(long, lat, group=group), color="light green", fill="light green") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="none",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())
base_us
test <- member_weekly_df[1:1000,]

map_data <- base_us + 
  geom_point(data = test, aes(x=start_lng, y=start_lat), color="Deep Pink", fill="Pink", pch=21, size=5, alpha=I(0.7))
map_data

###
## create map of chicago from https://cfss.uchicago.edu/notes/raster-maps-with-ggmap/
#
chi_bb <- c( # store bounding box coordinates
  left = -87.936287,
  bottom = 41.679835,
  right = -87.447052,
  top = 42.000835
)

chicago_stamen <- get_stamenmap( # retrieve map from Stamen Maps
  bbox = chi_bb,
  zoom = 11
)

chicago_map <- chicago_stamen

# use ggmap() to view
member_ride_density <- ggmap(chicago_map) + # map to show ride yearly ride density
  stat_density2d(data = member_weekly_df, aes(x=start_lng, y=start_lat), color="Deep Pink", fill=stat(nrow(test)), alpha=I(0.5), geom = "polygon") +
  labs(
    title = "Ride Density for Members",
    subtitle = "January 2021 - October 2021",
    x = "Longitude",
    y = "Latitude",
  )
member_ride_density



