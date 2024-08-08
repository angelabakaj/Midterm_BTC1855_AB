##### BTC-1855 MIDTERM PROJECT: ANGELA BAKAJ

##### Loading the Datasets into R & Assigning Data to Variables

weather_data <- read.csv("/Users/angiebakaj/Downloads/babs/weather.csv")
trip_data <- read.csv("/Users/angiebakaj/Downloads/babs/trip.csv")
station_data <- read.csv("/Users/angiebakaj/Downloads/babs/station.csv")

##### Installing the necessary packages & keeping a neat collection of these packages here:
# I will update this "library" for each downstream analysis
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("corrplot")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(lubridate)
library(ggplot2)
library(dplyr)
library(corrplot)

##### Performing EDA:

### EDA on "station_data"
glimpse(station_data)
# 
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
# Changing the date in "installation_date" column to POSIX mdy format using Lubridate's "mdy" function:
# This will aid in downstream analysis involving dates in "station_data"
station_data$installation_date <- mdy(station_data$installation_date, tz = "UTC")

### Cleaning "trip_data":

# From the "describe()" fucntion we notice: "nil" values and non-USA zip-codes:

# Handle "nil" values in the "zip_code" column by setting them to NA:
trip_data$zip_code[trip_data$zip_code == "nil"] <- NA

# Check for any non-numeric values in the "zip_code" column:
# Handles non-US zip codes that contain letters or are more or less than 5 digits:
trip_data$zip_code[grepl("[^0-9]|^.{1,4}$|^.{6,}$", trip_data$zip_code)] <- NA

# Limit the "zip_code" column to only the valid zip-codes in the US:
# According to Google, the highest is 99950 in Ketchikan, AK and the lowest is 00501 in Holtsville, NY. 
numeric_zipcodes <- as.integer(trip_data$zip_code)
trip_data$zip_code[!(numeric_zipcodes >= 501 & numeric_zipcodes <= 99950)] <- NA

# Remove the outliers of the "duration" column:
# Anything less than 180s is likely a cancelled trip:
# There is also an extreme outlier of 194 days that must be removed:
trip_data$duration[trip_data$duration < 180 | trip_data$duration > 36000] <- NA
# I have set the upper limit to 36,000 seconds (check report for justification & logic)
# Checking if this is reasonable in terms of the data as a whole using the "quantile()" function:
quantile(trip_data$duration, na.rm = TRUE, 0.99)
# The 99th percentile for duration in seconds is approximately 22,000, so I know my limit of 36000 does not reduce my dataset extremely.

# In case of future analysis, I will keep this excluded data in a separate CSV file:
trip_data$duration[trip_data$duration < 180 | trip_data$duration > 36000] <- NA

# Putting "start" and "end" dates into POSIX format for potential downstream analysis:
trip_data$start_date <- mdy_hm(trip_data$start_date)
trip_data$end_date <- mdy_hm(trip_data$end_date)

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

# Extracting the hour and day of the week from the start_date:
trip_data <- trip_data %>%
  mutate(start_hour = hour(start_date),
         start_wday = wday(start_date, label = TRUE))

# Filtering for weekdays (Monday to Friday):
trip_data_weekdays <- trip_data %>%
  filter(start_wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Creating a histogram of the start hours on weekdays:
ggplot(trip_data_weekdays, aes(x = start_hour)) +
  geom_histogram(binwidth = 1, fill = "lightblue", col = "black") +
  labs(title = "Distribution of Bike Rentals by Hour on Weekdays",
       x = "Hour of the Day",
       y = "Number of Trips") +
  theme_minimal()
# From this histogram we see that the rush hours occur between the 7th-9th hours of the day (7-9am) and also between the 16th-18th hours (4-6pm)

### Determine the top 10 most frequent starting and ending stations during these rush hours:

# Defining rush hours from the histogram above:
rush_hours <- c(7, 8, 9, 16, 17, 18)

# Filtering for rush hours:
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
# Repeating above process to create another histogram:
trip_data <- trip_data %>%
  mutate(start_hour = hour(start_date),
         start_wday = wday(start_date, label = TRUE))

# Filtering for weekends (Saturday and Sunday):
trip_data_weekends <- trip_data %>%
  filter(start_wday %in% c("Sat", "Sun"))

# Creating a histogram of the start hours on weekends:
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

# Adding the year and the total available time in seconds:
month_days <- month_days %>%
  mutate(total_time_available = days_in_month * 24 * 60 * 60)

# Merging the "monthly_usage" with "month_days" to get the total time available:
# For this, I must join the data for monthly usage data with the total available time to get the utilization:
average_utilization <- monthly_usage %>%
  left_join(month_days, by = c("month" = "month")) %>%
  mutate(utilization = total_duration_seconds / total_time_available) %>%
  arrange(year, match(month, month.name))

# Here, we can see the average utilization in the form of a table:
print(average_utilization)

# Making a bar plot of the average utilization for better visualization:
ggplot(average_utilization, aes(x = factor(month, levels = month.name), y = utilization)) +
  geom_bar(stat = "identity", fill = "lightblue", col = "black") +
  labs(title = "Average Monthly Bike Utilization",
       x = "Month",
       y = "Average Utilization") +
  theme_minimal()

##### Weather-Rental Correlation Analysis:

# Creating a dataset for the joined/merged "trip_data":
merged_data <- trip_data %>%
  mutate(date = as.Date(start_date))

# Joining the "station_data" dataset to add a column specifying the city:
merged_data <- left_join(merged_data, station_data, by = c("start_station_id" = "id"))

# Creating a dataset for summarizing daily data within each city, and merge it with "weather_data" based on city and date:
# Grouping by city and date:
grouped_data <- group_by(merged_data, city, date)

# Summarizing the data:
summarized_data <- summarize(grouped_data,
                             total_trip_time = sum(duration),
                             number_of_trips = n())

# Joining the "weather_data":
joined_data <- left_join(summarized_data, weather_data, by = c("city", "date"))

# Selecting the required columns:
daily_city_summary <- select(joined_data, -zip_code, -date, -events) %>%
  mutate(cloud_cover = as.numeric(cloud_cover))

# Creating a for-loop to re-apply the correlation "corrplot()" function to each city:
for(city in unique(daily_city_summary$city)) {
  tmp <- daily_city_summary[daily_city_summary$city == city, -1]
  correlation_matrix <- cor(tmp)
  correlation_matrix <- correlation_matrix[1:2, -c(1,2)]
  corrplot(correlation_matrix, title = city)
  }







