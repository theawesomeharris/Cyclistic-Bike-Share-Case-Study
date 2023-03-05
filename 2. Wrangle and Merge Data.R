
#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

# colnames(M01_2022)
# colnames(M02_2022)
# colnames(M03_2022)
# colnames(M04_2022)
# colnames(M05_2022)
# colnames(M06_2022)
# colnames(M07_2022)
# colnames(M08_2022)
# colnames(M09_2022)
# colnames(M10_2022)
# colnames(M11_2022)
# colnames(M12_2022)

# upon inspection, all files have the same column names as below
# [1] "ride_id"            "rideable_type"      "started_at"        
# [4] "ended_at"           "start_station_name" "start_station_id"  
# [7] "end_station_name"   "end_station_id"     "start_lat"         
# [10] "start_lng"          "end_lat"            "end_lng"           
# [13] "member_casual"     

# Inspect the dataframes and look for incongruence. 

# str(M01_2022)
# str(M02_2022)
# str(M03_2022)
# str(M04_2022)
# str(M05_2022)
# str(M06_2022)
# str(M07_2022)
# str(M08_2022)
# str(M09_2022)
# str(M10_2022)
# str(M11_2022)
# str(M12_2022)

# Convert the data types of columns so that they can stack correctly.
# Ensure numbers are as Number, texts as Character, etc.

# To change the data type of a column, use the following script:

# Mutation syntax (convert to type Character) as follow:
# table_name <- mutate(table_name, col_name1 = as.character(col_name1)
#                   , col_name2 = as.character(col_name2))

# Example:
# M01_2022 <-  mutate(M1_2022, ride_id = as.character(ride_id)
#                  ,rideable_type = as.character(rideable_type)) 

# Combine (Stack) all individual month data frame into one big data frame
all_trips <- bind_rows(M01_2022, M02_2022, M03_2022, M04_2022
                       , M05_2022, M06_2022, M07_2022, M08_2022
                       , M09_2022, M10_2022, M11_2022, M12_2022)

# Remove one "duration..mins." column that was added in the M01_2022 csv file
# that was brought forward into the merged file

# all_trips <- all_trips %>%
#  select(-c(duration..mins.))
