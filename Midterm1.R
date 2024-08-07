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

# Convert missing values present in "events" (as seen from "describe" function) to NA:
weather_data$events[weather_data$events == ""] <- NA

# Accounting for "T"s in precipitation (as seen from "describe" function)
# "T" means "trace"; representing values less than 0.01; we will assign 0.005 to these to account for trace values less than 0.01.
weather_data$precipitation_inches[weather_data$precipitation_inches == "T"] <- 0.005

# All values in this precipitation column must be numeric for downstream analysis:
weather_data$precipitation_inches <- as.numeric(weather_data$precipitation_inches)

# "city", "events", and "cloud_cover" must all be factors for downstream analysis:
weather_data$city <- as.factor(weather_data$city)
weather_data$events <- as.factor(weather_data$events)
weather_data$cloud_cover <- as.factor(weather_data$cloud_cover)

##### Rush Hours Determination:

### Converting both to POSIX format (MIGHT MOVE TO CLEANING ABOVE)
trip_data$start_date <- mdy_hm(trip_data$start_date)
trip_data$end_date <- mdy_hm(trip_data$end_date)

# Extract the hour and day of the week from the start_date
trip_data <- trip_data %>%
  mutate(start_hour = hour(start_date),
         start_wday = wday(start_date, label = TRUE))

# Filter for weekdays (Monday to Friday)
trip_data_weekdays <- trip_data %>%
  filter(start_wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Create a histogram of the start hours on weekdays
ggplot(trip_data_weekdays, aes(x = start_hour)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(title = "Distribution of Bike Rentals by Hour on Weekdays",
       x = "Hour of the Day",
       y = "Number of Trips") +
  theme_minimal()
# From this histogram we see that the rush hours occur between the 7th-9th hours of the day (7-9am) and also between the 16th-18th hours (4-6pm)

### Determine the top 10 most frequent starting and ending stations during these rush hours:

# Define rush hours
rush_hours <- c(7, 8, 9, 16, 17, 18)

# Filter for rush hours
trip_data_rush_hours <- trip_data_weekdays %>%
  filter(start_hour %in% rush_hours)

# Determine 10 most frequent starting stations
top_start_stations <- trip_data_rush_hours %>%
  group_by(start_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10, count)
# Here, we see the top 10 most frequent starting stations.
print(top_start_stations)

# Determine 10 most frequent ending stations
top_end_stations <- trip_data_rush_hours %>%
  group_by(end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10, count)
# Here, we see the top 10 most frequent ending stations.
print(top_end_stations)


### Determine the top 10 most frequent starting and ending station on weekends:
# Repeat above process to create another histogram:
trip_data <- trip_data %>%
  mutate(start_hour = hour(start_date),
         start_wday = wday(start_date, label = TRUE))

# Filter for weekends (Saturday and Sunday)
trip_data_weekends <- trip_data %>%
  filter(start_wday %in% c("Sat", "Sun"))

# Create a histogram of the start hours on weekends
ggplot(trip_data_weekends, aes(x = start_hour)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Bike Rentals by Hour on Weekends",
       x = "Hour of the Day",
       y = "Number of Trips") +
  theme_minimal()

# Determine 10 most frequent starting stations
top_start_stations2 <- trip_data_weekends %>%
  group_by(start_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10, count)
# Here, we see the top 10 most frequent starting stations.
print(top_start_stations)

# Determine 10 most frequent ending stations
top_end_stations2 <- trip_data_weekends %>%
  group_by(end_station_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10, count)
# Here, we see the top 10 most frequent ending stations.
print(top_end_stations)

##### Bike Utilization Analysis:

# Extracting month and year from start_date in trip_data:
trip_data2 <- trip_data %>%
  mutate(month = month(start_date, label = TRUE, abbr = FALSE),
         year = year(start_date))

# Calculating the total duration that each bike was used per month
monthly_usage <- trip_data2 %>%
  group_by(year, month) %>%
  summarise(total_duration_seconds = sum(duration, na.rm = TRUE), .groups = 'drop')

# Calculating the total number of seconds in each month
# I will create a vector for the days in each month in 2014 using the "lubridate" package again:
month_days <- data.frame(
  month = month.name,
  days_in_month = sapply(1:12, function(m) days_in_month(ymd(paste("2014", m, "01", sep = "-"))))
)

# Adding the year and the total available time in seconds
month_days <- month_days %>%
  mutate(total_time_available = days_in_month * 24 * 60 * 60)

# Merging the "monthly_usage" with "month_days" to get the total time available
# For this, I must join the data for monthly usage data with the total available time to get the utlization
average_utilization <- monthly_usage %>%
  left_join(month_days, by = c("month" = "month")) %>%
  mutate(utilization = total_duration_seconds / total_time_available) %>%
  arrange(year, match(month, month.name))

# Here, we can see the average utilization
print(average_utilization)

# Making a bar plot of the average utilization
ggplot(average_utilization, aes(x = factor(month, levels = month.name), y = utilization)) +
  geom_bar(stat = "identity", fill = "lightblue", col = "black") +
  labs(title = "Average Monthly Bike Utilization",
       x = "Month",
       y = "Average Utilization") +
  theme_minimal()

##### Weather-Rental Correlation Analysis:

# must go back and fix weather data cleaning






