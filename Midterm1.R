##### BTC-1855 MIDTERM PROJECT: ANGELA BAKAJ

##### Loading the Datasets into R & Assigning Data to Variables

weather_data <- read.csv("/Users/angiebakaj/Downloads/babs/weather.csv")
trip_data <- read.csv("/Users/angiebakaj/Downloads/babs/trip.csv")
station_data <- read.csv("/Users/angiebakaj/Downloads/babs/station.csv")

##### Installing the necessary packages:
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(lubridate)

##### Performing EDA:

### EDA on "station_data"
glimpse(station_data)
# From the "glimpse" function, I see that the dates are given as characters, and if I want to perform math on them later, it would be beneficial to convert them to POSIX.
print(status(station_data))
#
freq(station_data)
#
print(profiling_num(station_data))
#
plot_num(station_data)
#
describe(station_data)

### EDA on "trip_data"
glimpse(trip_data)
# 
print(status(trip_data))
#
freq(trip_data)
#
print(profiling_num(trip_data))
#
plot_num(trip_data)
#
describe(trip_data)
# 

### EDA on "weather_data"
glimpse(weather_data)
# 
print(status(weather_data))
#
freq(weather_data)
#
print(profiling_num(weather_data))
#
plot_num(weather_data)
#
describe(weather_data)
# 


##### Data Cleaning:

### Cleaning "station_data":
# Changing the date in "installation_date" column to mdy format using Lubridate's "mdy" function:
station_data$installation_date <- mdy(station_data$installation_date, tz = "UTC")

### Cleaning "weather_data":







