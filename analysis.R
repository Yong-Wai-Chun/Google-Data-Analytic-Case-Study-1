### Adapted from Divvy_Exercise_Full_Year_Analysis ###


# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished': Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: "In what ways do members and casual riders use Divvy bikes differently?"


# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  


library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("D:/lecture notes/lecture notes/Google")


#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
members <- read_csv("processed/m1.csv")
casuals <- read_csv("processed/c1.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(members)
colnames(casuals)

# Inspect the dataframes and look for incongruencies
str(members)
str(casuals)


# Convert ride_id and rideable_type to character so that they can stack correctly
members <-  mutate(members, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
casuals <-  mutate(casuals, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 


# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(members, casuals)


# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng,))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics


# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (2) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (3) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.


# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)


# Inspect the structure of the columns
str(all_trips)


# Convert "ride_length" from Factor to numeric so we can run calculations on the data

is.factor(all_trips$ride_length)

all_trips$ride_length <- lubridate::hms(all_trips$ride_length) 
all_trips$ride_length <- period_to_seconds(all_trips$ride_length)

is.numeric(all_trips$ride_length)


#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips$ride_length) #straight average (total ride length / rides)
median(all_trips$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips$ride_length) #longest ride
min(all_trips$ride_length) #shortest ride


# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips$ride_length)


# Compare members and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                                                        #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%                 # calculates the average duration
  arrange(member_casual, weekday)                                                                # sorts


# Let's visualize the number of rides by rider type
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')


#You're done! Congratulations!