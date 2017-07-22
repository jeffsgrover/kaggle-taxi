# Taxi Trip Duration

# Preamble
cd <- "/Users/jeffgrover/Documents/Data Science/Kaggle/Taxi Trip Duration/"
setwd(cd)
library(tidyverse)
library(lubridate)
library(geosphere)
library(maptools)

# Importing data
train <- fread("train.csv")

# id - a unique identifier for each trip
# vendor_id - a code indicating the provider associated with the trip record
# pickup_datetime - date and time when the meter was engaged
# dropoff_datetime - date and time when the meter was disengaged
# passenger_count - the number of passengers in the vehicle (driver entered value)
# pickup_longitude - the longitude where the meter was engaged
# pickup_latitude - the latitude where the meter was engaged
# dropoff_longitude - the longitude where the meter was disengaged
# dropoff_latitude - the latitude where the meter was disengaged
# store_and_fwd_flag - This flag indicates whether the trip record was held in vehicle memory before sending to the vendor because the vehicle did not have a connection to the server - Y=store and forward; N=not a store and forward trip
# trip_duration - duration of the trip in seconds


# Data Cleaning
train <- mutate_at(train, c("pickup_datetime", "dropoff_datetime"), as_datetime)
train <- mutate_at(train, "vendor_id", as.factor)
## Create distance variable
train$dist <- distHaversine(cbind(train$pickup_longitude, train$pickup_latitude),
                             cbind(train$dropoff_longitude, train$dropoff_latitude))
# Convert to kilometers
train$dist <- train$dist/1000

# Exploratory Analysis
dur <- ggplot(train, aes(x=trip_duration))
dur + geom_density()

## An outlier is screwing things up. Let's look at observations below 10,000
dur_trimmed <- ggplot(subset(train, trip_duration<10000), aes(x=trip_duration))