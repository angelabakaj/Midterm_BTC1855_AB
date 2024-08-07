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
library(ggplot2)
library(dplyr)


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

### Cleaning "trip_data":

# Remove "nil" values in the "zip_code" column:
trip_data$zip_code[trip_data$zip_code == "nil"] <- NA

# Check for any non-numeric values in the "zip_code" column:
trip_data$zip_code[grepl("[^0-9]|^.{1,4}$|^.{6,}$", trip_data$zip_code)] <- NA

# Limit the "zip_code" column to only the valid zip-codes in the US:
# According to Google, the highest is 99950 in Ketchikan, AK and the lowest is 00501 in Holtsville, NY. 
numeric_zipcodes <- as.integer(trip_data$zip_code)
trip_data$zip_code[!(numeric_zipcodes >= 501 & numeric_zipcodes <= 99950)] <- NA

# Remove the outliers of the "duration" column:
# Anything less than 180s is likely a cancelled trip:
# There is also an extreme outlier of 194 days that must be removed:
trip_data$duration[trip_data$duration < 180 | trip_data$duration > 720454] <- NA
### MUST FIX THE UPPER LIMIT (find reason to justify, possibly 1 day, or maybe longest trip there and back)

# Putting "start" and "end" dates into POSIX format for potential downstream analysis:
#(replace with below)

### Cleaning "weather_data":

# Putting "date" into POSIX format for potential downstream analysis:
weather_data$date <- mdy(weather_data$date, tz = "UTC")

# CONTINUE WEATHER CLEANING



##### Rush Hours Determination:

### Converting both to POSIX format (MIGHT MOVE TO CLEANING)
trip_data$start_date <- ymd_hms(trip_data$start_date)
trip_data$end_date <- ymd_hms(trip_data$end_date)

# Extract the hour and day of the week from the start_date
trip_data <- trip_data %>%
  mutate(start_hour = hour(start_date),
         start_wday = wday(start_date, label = TRUE))

# Filter for weekdays (Monday to Friday)
trip_data_weekdays <- trip_data %>%
  filter(start_wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Create a histogram of the start hours on weekdays
ggplot(trip_data_weekdays, aes(x = start_hour)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Bike Rentals by Hour on Weekdays",
       x = "Hour of the Day",
       y = "Number of Trips") +
  theme_minimal()








