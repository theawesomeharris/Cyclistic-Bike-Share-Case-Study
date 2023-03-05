
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created

# List of column names
colnames(all_trips) 

# How many rows are in data frame?
nrow(all_trips) 

# Dimensions of the data frame?
dim(all_trips) 

# See the first 6 rows of data frame. Also tail(all_trips)
head(all_trips) 
tail (all_trips)

# See list of columns and data types (numeric, character, etc)
str(all_trips) 

# Statistical summary of data. Mainly for numerics
summary(all_trips) 

# Find the number of unique characters in column and the insights behind

length(unique(all_trips$ride_id)) 
## 5667717 unique customers

length(unique(all_trips$rideable_type)) 
## There are 3 unique rideable types

length(unique(all_trips$member_casual)) 
## There are 2 unique user types

# Then find all unique characters in a column and the insights behind

unique(all_trips$rideable_type) 
## "electric_bike" "classic_bike"  "docked_bike" 

unique(all_trips$member_casual) 
## "casual" "member"

# Find out the number of unique counts of each variable in a column
table(all_trips$member_casual)
## casual  member 
## 2322032 3345685 

# IMPORTANT! CHECK FOR NA VALUES IN EACH COLUMN!
# Create a function to check for NA
check_na <- function(df) {
  sapply(df, function(x) {
    sum(is.na(x))
  })
}
# Run this function check_na() 
check_na(all_trips)

## it was found that end_lat and end_lng have 5858 rows of NA

result <- subset(all_trips, is.na(all_trips$end_lat) )
result <- subset(all_trips, is.na(all_trips$end_lng) )

## these rows need to be removed as bad data

# From summary(), there is value 0 in end_lat and end_lng column as well
result <- subset(all_trips, all_trips$end_lat == 0 )
result <- subset(all_trips, all_trips$end_lng == 0 )
result <- subset(all_trips, all_trips$end_station_id == "chargingstx07")

# Upon inspection, there is missing end station coordinates
#  for Green St & Madison Ave*.
# Luckily the data is available in other rows 
#  where end_lat = 41.88183 and end_lng = -87.64883

result <- subset(all_trips
  , all_trips$end_station_name == "Green St & Madison Ave*" 
  & all_trips$end_lat == 0 | all_trips$end_lng == 0)

# Correct the end_lat and end_lng values

all_trips <- all_trips %>% 
  mutate(end_lat = if_else(end_lat == 0, 41.88183, end_lat)) %>%
  mutate(end_lng = if_else(end_lng == 0, -87.64883, end_lng))

summary(all_trips)

# Use recode vectorized function in Mutate function if you
# want to rename certain unique keys in a column.
# In the example below, the use of certain names have changed.
# "Subscriber" means "member" and "Customer" means "casual" in other dataset.
# So to standardize the variable names are renamed to the latest one.

## all_trips <- all_trips %>%
##  mutate(member_casual = recode(member_casual
##                                , "Subscriber" = "member")
##                                , "Customer" = "casual"))

### After this step, recheck to make sure the proper number of 
### observations are reassigned by repeating line 168.

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each day, month or year.

all_trips$date <- as.Date(all_trips$started_at)
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

## Note: When importing .csv file do not temper with the .csv file
##       e.g. adding column with calculation.
##       All the time related functions will not work correctly
##       e.g. difftime, POSIXct, etc.

# Add a ride_length calculation to all trips in seconds

all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

## Scrutinize ride length calculation

max(all_trips$ride_length) #returns 2483235 seconds
min(all_trips$ride_length) #returns -621202 seconds
result <- subset(all_trips, all_trips$ride_length == 2483235) 
result <- subset(all_trips, all_trips$ride_length == 1) 
result <- subset(all_trips, all_trips$ride_length == -621201) 

# all_trips1 <- all_trips %>% sort(all_trips$ride_length, decreasing = TRUE)
# create a dataframe in descending order of ride_length to investigate further
# using sort() result in very long processing time, try using order()


# Add a distance calculation to all trips in kilometer

all_trips$distance <- round(distHaversine(cbind(all_trips$start_lng
                                                , all_trips$start_lat)
                                          , cbind(all_trips$end_lng
                                                  , all_trips$end_lat))*0.001,3)

# check for NA again
check_na(all_trips)
## 5858 rows has NA, these rows definitely need to be removed

# check why max distance is so big
summary(all_trips)
result <- subset(all_trips, all_trips$distance == 9825.063)
results <- subset(all_trips, all_trips$distance > 30) # discovered something
result <- subset(all_trips, all_trips$start_station_id == "Pawel Bialowas - Test- PBSC charging station")
# returms 1190.855km which is an outlier, to be removed

# Inspect the structure of the columns
str(all_trips)

# Remove bad data
# Check whether there is negative time duration in the ride_length column
any(all_trips$ride_length < 0) #returns TRUE, there are negative values
any(all_trips$ride_length == 0) #returns TRUE, there are zero values
neg_rows <- which(all_trips$ride_length <= 0) #store negative and zero rows values
all_trips[neg_rows, ] #display rows with negative ride_length

# From previous years, we know that the station ID for maintenance is "HQ QR"
# Check station ID for any trips that were sent for maintenance
n_distinct(all_trips$start_station_name) #returns 1675
n_distinct(all_trips$start_station_id) #returns 1314
# number of station_name and station_id does not match. not a big concern.
options(max.print = 1675) #default display is only 999 rows
unique(all_trips$start_station_name)
any(all_trips$start_station_name == "HQ QR") #returns false
## Hubbard Bike Checking data will be removed

# Tips to search for unique string words for bike maintenance
# result <- unique(all_tripsv2$end_station_id)
# result <- grepl("check", all_tripsv2$end_station_id)

result <- subset(all_trips, all_trips$start_station_name == "" |
                   all_trips$end_station_name == "") # 1298357 rows

# 1298357 / 5667717 = 0.229
# about 23% of data compromised by missing station names

# There are MANY EMPTY START STATION NAME AND ID BUT COORDINATES ARE AVAILABLE
# Need to carry out exploratory data analysis to find out

n_distinct(all_trips$start_station_name) #1675
n_distinct(all_trips$start_station_id) #1314
n_distinct(all_trips$end_station_name) #1693
n_distinct(all_trips$end_station_id) #1318

station_start <- subset(all_trips, select = c(start_station_name, start_station_id, start_lat, start_lng))
station_start <- unique(station_start, by = c("start_station_name"))
station_start <- station_start[order(station_start$start_station_name), ] #order alphabetically
station_start <- station_start %>% 
  rename("station_name" = "start_station_name"
          , "station_id" = "start_station_id"
          , "lat" = "start_lat"
          , "lng" = "start_lng")

station_end <- subset(all_trips, select = c(end_station_name, end_station_id, end_lat, end_lng))
station_end <- unique(station_end, by = c("end_station_name"))
station_end <- station_end[order(station_end$end_station_name), ] #order alphabetically
station_end <- station_end %>% 
  rename("station_name" = "end_station_name"
         , "station_id" = "end_station_id"
         , "lat" = "end_lat"
         , "lng" = "end_lng")

station_data <- full_join(station_start, station_end, )
station_data <- unique(station_data, by = c("station_name")) 

station_data_ordered <- station_data[order(station_data$lat, station_data$lng), ]

station_data_ordered1 <- station_data_ordered
station_data_ordered1$lat <- round(station_data_ordered$lat, 1)
station_data_ordered1$lng <- round(station_data_ordered$lng, 1)
station_data_ordered1 <- station_data_ordered1[order(station_data_ordered1$lat, station_data_ordered1$lng), ]

# create lookup dataframe for available data
lookup <- unique(subset(station_data_ordered1, !station_data_ordered1$station_name == ""))

# create dataframe for all missing data
station_data_ordered2 <- subset(station_data_ordered1, station_data_ordered1$station_name == "")

# the point is to fill in available data (lookup) into missing data

station_data_ordered2$station_name <- if_else(station_data_ordered2$station_name == "", lookup[match(paste0(station_data_ordered2$lat
                                                         ,station_data_ordered2$lng)
                                                 , paste0(lookup$lat, lookup$lng)), ]$station_name, station_data_ordered2$station_name)


## Realized it is difficult to identify the missing station names 
## from lat and lng data alone. 



# Create a new dataframe v2 since data is being removed
all_tripsv2 <- all_trips[!(all_trips$end_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" |
                             all_trips$ride_length <= 0 |
                             all_trips$start_station_id 
                             == "Pawel Bialowas - Test- PBSC charging station"), ]

result <- subset(all_tripsv2, ride_length <= 0) # check ok
result <- subset(all_tripsv2, distance > 30) 


# Remove all NA rows
all_tripsv2 <- na.omit(all_tripsv2)

#Recheck the new dataframe v2
summary(all_tripsv2)

result <- subset(all_tripsv2, ride_length > 86400)
result <- subset(all_tripsv2, all_tripsv2$end_station_id 
                 == "Hubbard Bike-checking (LBS-WH-TEST)")
