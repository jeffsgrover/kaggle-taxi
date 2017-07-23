# Taxi Trip Duration

# Preamble
cd <- "/Users/jeffgrover/Documents/Data Science/Kaggle/Taxi Trip Duration/"
setwd(cd)
library(data.table)
library(tidyverse)
library(lubridate)
library(geosphere)
library(maptools)
library(leaflet)
library(rworldmap)

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

#################
# Data Cleaning #
#################
train <- train_original
train <- mutate(train, pickup_datetime = as_datetime(pickup_datetime, tz="New York City"))
## Wait, why are so many of the pickup and dropoff datetimes equal? Fix this.
train <- mutate_at(train, "vendor_id", as.factor)

# Create distance variable using the geosphere package
train$dist <- distHaversine(cbind(train$pickup_longitude, train$pickup_latitude),
                             cbind(train$dropoff_longitude, train$dropoff_latitude))
# Comes out as meters. Convert to miles
train$dist <- train$dist*0.000621371

# Create trip duration in minutes and hours
train$trip_duration_min <- train$trip_duration/60
train$trip_duration_hr <- train$trip_duration_min/60

# Create miles traveled per hour
train$mph <- train$dist/train$trip_duration_hr

########################
# Exploratory Analysis #
########################

# Let's plot stuff.

## Let's start with passenger count.
qplot(y=train$passenger_count, x=1, geom="boxplot")
## How many observations have zero passengers? Those can't be right.
sum(train$passenger_count==0)
## 60. Guess we're not keeping those. 
## Let's keep looking before we remove anything.

## Now, trip duration.
qplot(x=train$trip_duration_hr, geom="density")
## No way people are spending upwards of 900 hours in a cab. 
## What are the longest cab rides?
train %>% select (id, trip_duration_hr, dist, mph, pickup_datetime, dropoff_datetime) %>% 
      arrange(desc(trip_duration_hr)) %>% 
      head(20)
## Four cab rides are over three weeks long. Guess we're not keeping those either.
## What about the other weirdly long cab rides?
train_longride <- filter(train, trip_duration_hr>2 & trip_duration_hr<24)
qplot(x=train_longride$trip_duration_hr, geom="density")
## There seems to be a weird spike around 24 hours. 
## I've never taken a cab in NYC but that can't be right.
## How far were they traveling?
with(train_longride, qplot(x=trip_duration_hr, y=dist))
## No way they were taking a whole day to go less than 20 miles... these can't be right, either.
## Let's take a closer look at these almost-24-hr trips.
train_24hr <- filter(train, trip_duration_hr>20 & trip_duration_hr<24)
qplot(x=train_24hr$trip_duration_hr, geom="density")
train_24hr %>% select(id, pickup_datetime, dropoff_datetime, dist, trip_duration_hr) %>%
      arrange(trip_duration_hr) %>% 
      head(20)
## So it looks like several ended exactly at midnight, down to the second. Probably just an error.
## Some of these durations are too short, too.
train_shortride <- filter(train, trip_duration_min<15)
qplot(x=train_shortride$trip_duration_min, geom="density")
## What are some of the shortest?
train %>% select(id, trip_duration, pickup_datetime, dropoff_datetime, dist) %>%
      arrange(trip_duration) %>%
      head(50)
## One-second cab rides?
sum(train$trip_duration==1)
## 33 of those. For some, the datetimes corroborate the 1-second story, so we can't recover the actual trip durations.
## How many cab rides were less than, say, one minute?
train_veryshortride <- filter(train, trip_duration<60)
qplot(x=train_veryshortride$trip_duration, geom="density")
with(train_veryshortride, qplot(x=trip_duration, y=dist))
## Most of these are reasonably short, but there's no way people are going more than a mile in less than a minute.

## Let's keep looking.

## How far were these train rides?
qplot(x=train$dist, geom="density")
## Nope. What are the outliers?
train %>% select(mph) %>% 
      arrange(desc(mph)) %>% 
      head(20)
## There's got to be a limit to how far these are before they're not even in NYC anymore.
## Maybe the coordinates are messed up? Take a look; there are only 13 trips over 100 miles.
train_longdist <- filter(train, dist>100) 
qplot(data=train_longdist, x=pickup_longitude, y=pickup_latitude)
leaflet(data=train_longdist) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(~pickup_longitude, ~pickup_latitude, radius=1, color="red") %>%
      addCircleMarkers(~dropoff_longitude, ~dropoff_latitude, radius=1, color="green")
## Okay, no one is getting picked up in the Atlantic Ocean or northern Canada
## And no one is getting dropped off in western Pennsylvania. 
## We'll be dropping these too; probably bounding the coordinates.

## Now let's look at how fast the drivers were going.
qplot(x=train$mph, geom="density")
## Look at the craziest ones.
train %>% select(id, trip_duration_min, dist, mph) %>% 
      arrange(desc(mph)) %>%
      head(150)
## No way these duration-distance combos are correct.

## Let's re-plot without some of the super crazy speeds.
train_notsofast <- filter(train, mph<100)
qplot(data=train_notsofast, x=mph, geom="density")

## There's a spike at zero speed, too. How many had no speed?
sum(train$mph==0)
## Almost 6,000! Is it lack of distance traveled?
sum(train$mph==0 & train$dist==0)
## Yup. What do these coordinates look like?
train_nodist <- filter(train, dist==0)
leaflet(data=train_nodist) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(~pickup_longitude, ~pickup_latitude, radius=1, color="blue") %>%
      addCircleMarkers(~dropoff_longitude, ~dropoff_latitude, radius=1, color="green")

## Alright. Let's drop some of the crazy datapoints.
train_original <- train
train <- filter(train, passenger_count!=0)
train <- filter(train, trip_duration_hr<24)