# Midterm_BTC1855_AB
Plan:

-Perform EDA
  -in each separate dataset ("trip", "weather", "station")
  -use EDA results to determine how to clean the dataset

-clean data
  -in each separate dataset ("trip", "weather", "station")
  
    -For station, must fix:
      -date (put to POSIX)

    -For weather, must fix:
      -date (put to POSIX)
      -max_visibility (get rid of missing values, 9)
      -mean_visibility (get rid of missing values, 9)
      -min_visibility (get rid of missing values, 9)
      -max_gust_speed (get rid of missing values, 451)
      -precipitation (get rid of the "T" detected)
      -events (get rid of the missing values, 1473)

    -For trip data, must fix:
      -duration (must limit the outliers, 180 is cancelled trip and too high)
      -start date (put into POSIX)
      -end date (put into POSIX)
      -bikeID and stationID (why are there more than what is distinct?)
      -zip code (get rid of missing, weird/invalid values)

-Determine the "rush hours": highest volume hours on weekdays

-Determine the 10 most frequent "starting" and "ending" stations during the rush hours previously found

-Determine the average utilization of bikes for each month

-Determine the weather predictors which have an impact on bike rental patterns
  -join "trip" and "weather" datasets
  -use cor() function from "corrplot" package to create a correlation matrix for the newly joined dataset
  -use the matrix to determine which weather predictors have the highest/strongest correlations
