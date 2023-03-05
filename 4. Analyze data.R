
#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# (A) Descriptive analysis on ride_length (all figures in seconds)

#mean(all_tripsv2$ride_length) #straight average (total ride length / rides)
#median(all_tripsv2$ride_length) #midpoint number in the ascending array of ride lengths
#max(all_tripsv2$ride_length) #longest ride
#min(all_tripsv2$ride_length) #shortest ride
#summary(all_tripsv2$ride_length) #summary of all the above

# Compare members and casual users
#aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = mean)
#aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = median)
#aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = max)
#aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual, FUN = min)

## Realized the minimum ride_length is 1, dive more into the data
table_ridelength <- all_tripsv2 %>% count(all_tripsv2$ride_length, sort = FALSE)
## So many data with ride_length < 1 minute, not very likely for normal usage

## aggregate(formula, data, FUN, ..., simplify = TRUE)
## Where:
## formula:  a formula that describes the structure of the data frame
##          , with the left-hand side (LHS) of the formula specifying 
##          the columns to be aggregated, and the right-hand side (RHS) 
##          specifying the grouping variables. 
##          The tilde ~ separates the LHS from the RHS.
## data:     the data frame to be aggregated.
## FUN:      the function(s) to be applied to each group of the data frame 
##          specified in the formula. You can specify multiple functions 
##          by providing a list of functions.
## ...:      additional arguments to be passed to the function(s) specified in FUN.
## simplify: a logical value indicating whether the result should be 
##          simplified to a vector or matrix (if possible) or returned 
##          as a data frame. The default value is TRUE.

# See the average ride time by each day for members vs casual users
#aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual + all_tripsv2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_tripsv2$day_of_week <- ordered(all_tripsv2$day_of_week
                                   , levels=c("Sunday", "Monday"
                                              , "Tuesday", "Wednesday"
                                              , "Thursday", "Friday"
                                              , "Saturday"))

# Run again the average ride time by each day
#aggregate(all_tripsv2$ride_length ~ all_tripsv2$member_casual + all_tripsv2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
# Create a dataframe of the above for simpler analysis

Ridership_wday <- all_tripsv2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field 
  group_by(member_casual, weekday, rideable_type) %>% #grouping
  summarise(number_of_rides = n() #calculate
            , average_duration = round(mean(ride_length)/60, 0) #in minutes
            , total_duration = round(sum(ride_length)/60/60, 0) #in hours
            , total_distance = round(sum(distance),0)) %>% #in km
  arrange(member_casual, weekday, rideable_type) #sorts the table

# Analyze ridership data by type and month

Ridership_mth <- all_tripsv2 %>%
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarise(number_of_rides = n() #calculate
            , average_duration = round(mean(ride_length)/60, 0) #in minutes
            , total_duration = round(sum(ride_length)/60/60, 0) #in hours
            , total_distance = round(sum(distance),0)) %>% #in km
  arrange(member_casual, month, rideable_type)


# Create visualization for the average duration

## Plot of average duration by user type by WEEKDAY

### ALTERNATIVE 1 (ggplot() follows dataframe expression)

#1
Ridership_wday %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Average Riding Duration by Membership Type"
       , subtitle = "in 2022", x = "Day", y = "Average Duration (min)"
       , fill = "Membership type")

### ALTERNATIVE 2 (ggplot() expression only)

ggplot(Ridership_wday, aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Average Riding Duration by Membership Type"
       , subtitle = "in 2022", x = "Day", y = "Average Duration (min)"
       , fill = "Membership type")

## Plot of average duration by user type by MONTH

### ALTERNATIVE 1 (ggplot() follows dataframe expression)

#2
Ridership_mth %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Average Riding Duration by Membership Type"
       , subtitle = "in 2022", x = "Month", y = "Average Duration (min)"
       , fill = "Membership Type")

### ALTERNATIVE 2 does not work as the month column and month() clashes

#  ggplot(Ridership_mth, aes(x = month, y = average_duration, fill = member_casual)) +
#  geom_col(position = "dodge") +
#  labs(title = "Comparison of Average Riding Duration by Membership Type",
#       x = "Month", y = "Average Duration (min)")

# *(B) Descriptive analysis on riding duration vs time (rideable type)


#3
Ridership_mth %>%
  ggplot(aes(x = month, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Average Riding Duration by Membership Type"
       , subtitle = "in 2022", x = "Month", y = "Average Duration (min)"
       , fill = "Rideable Type") + ylim(0, 60)

#4
Ridership_wday %>%
  ggplot(aes(x = weekday, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Average Riding Duration by Membership Type"
       , subtitle = "in 2022", x = "Day", y = "Average Duration (min)"
       , fill = "Rideable type") + ylim(0, 60)


# *(C) Descriptive analysis on number of rides vs time (type of membership)

#5
Ridership_mth %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Number of Rides by Membership Type"
       , subtitle = "in 2022", x = "Month", y = "Number of Rides"
       , fill = "Membership Type") + ylim(0, 300000)

#6
Ridership_wday %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Number of Rides by Membership Type"
       , subtitle = "in 2022", x = "Day", y = "Number of Rides"
       , fill = "Membership Type") + ylim(0, 300000)

# *(D) Descriptive analysis on number of rides vs month (type of ride)

#7
Ridership_mth %>%
  ggplot(aes(x = month, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Number of Rides by Rideable Type"
       , subtitle = "in 2022", x = "Month", y = "Number of Rides"
       , fill = "Type of Ride") + ylim(0, 300000)
#8
Ridership_wday %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Number of Rides by Rideable Type"
       , subtitle = "in 2022", x = "Day", y = "Number of Rides"
       , fill = "Type of Ride") + ylim(0, 300000)

# Descriptive Analysis on Distance travelled by User Type

#9
Ridership_mth %>%
  ggplot(aes(x = month, y = total_distance, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Total Distance Travelled by Membership Type"
       , subtitle = "in 2022", x = "Month", y = "Distance Travelled (km)"
       , fill = "Membership Type")
#10
Ridership_wday %>%
  ggplot(aes(x = weekday, y = total_distance, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Total Distance Travelled by Membership Type"
       , subtitle = "in 2022", x = "Month", y = "Distance Travelled (km)"
       , fill = "Membership Type")

#11
Ridership_mth %>%
  ggplot(aes(x = month, y = total_distance, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Total Distance Travelled by Rideable Type"
       , subtitle = "in 2022", x = "Month", y = "Total Distance Travelled (km)"
       , fill = "Type of Ride")
#12
Ridership_wday %>%
  ggplot(aes(x = weekday, y = total_distance, fill = rideable_type)) +
  geom_col(position = "dodge") +
  labs(title = "Comparison of Total Distance Travelled by Rideable Type"
       , subtitle = "in 2022", x = "Month", y = "Total Distance Travelled (km)"
       , fill = "Type of Ride")

# (E) Bike preference by user type

bikepreference <- all_tripsv2 %>%
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_rides = n()
            , total_duration = round(sum(ride_length)/60/60, 0)) %>% #in hours
  arrange(member_casual, rideable_type)

result <- subset(all_tripsv2, member_casual == "member" & rideable_type == "docked_bike")

# It is found that member do not use docked_bike at all.
# For correct visualization, we add one row of this with 0 values in the table.

bikepreference[nrow(bikepreference)+1, ] <- list("docked_bike", "member", 0, 0)
bikepreference <- bikepreference %>%  
  arrange(member_casual, rideable_type)

#13
bikepreference %>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Comparison of Bike Preference by Membership Type"
       , subtitle = "in 2022", x = "Membership Type", y = "Number of Rides"
       , fill = "Type of Ride")

#14
bikepreference %>%
  ggplot(aes(x = member_casual, y = total_duration, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Comparison of Bike Preference by Membership Type"
       , subtitle = "in 2022", x = "Membership Type", y = "Total Hours Cycled"
       , fill = "Type of Ride") + ylim(0, 400000)

# (F) Most Popular Stations by User Type

Popular_Start_Station <- all_tripsv2 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, desc(number_of_rides))

top5_startst_member <- head(subset(Popular_Start_Station, member_casual == "member" & start_station_name != ""), 5)
top5_startst_casual <- head(subset(Popular_Start_Station, member_casual == "casual" & start_station_name != ""), 5)

Popular_End_Station <- all_tripsv2 %>%
  group_by(end_station_name, member_casual) %>%
  summarise(number_of_rides = n()) %>%
  arrange(desc(number_of_rides))

top5_endst_member <- head(subset(Popular_End_Station, member_casual == "member" & end_station_name != ""), 5)
top5_endst_casual <- head(subset(Popular_End_Station, member_casual == "casual" & end_station_name != ""), 5)

# Number of rides by User Type

all_tripsv2 %>%
  ggplot(aes(x = member_casual)) + 
  geom_bar(position = "fill") +
  labs(title = "Number of Rides by Membership Type"
       , subtitle = "in 2022", x = "Membership Type", y = "Number of Rides") 


#=======================================
# STEP 5: CONCLUSION AND RECOMMENDATIONS
#=======================================


