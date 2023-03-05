
# title: "Case study of How Does a Bike-Share Navigate Speedy Success?"
# author: "Jing Xien"
# prepared: "24/1/2023"

##############
# Introduction
##############
# In 2016, Cyclistic launched a successful bike-share program in Chicago and has more than 5800 bicycles and 600 docking stations.
# Moreno, the Marketing Director believes that maximizing the number of annual members will be key to company's future growth.
# Hence, the marketing analyst team wants to understand from the historical data how the casual riders and annual riders, use the service differently. 
# The marketing team will design a new marketing strategy to convert casual riders into annual members from these insights.

# To understand the user trends, this report will present the findings in the following structure:
# * Number of trips completed by user type
# * Total distance (in kilometers) traveled by user type
# * Hours cycled by user type
# * Bike preference by user type
# * Number of rides completed by month by user type
# * Number of rides completed by day by user type
# * Top 5 start station by user types
# * Top 5 end station by user types

##########################
#Install required packages
##########################

#install.packages("r package", repos = "http://cran.us.r-project.org")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("geosphere")
#install.packages("clipr")

library(tidyverse) #for data import and wrangling
library(lubridate) #for wrangling date attributes
library(ggplot2) #for data visualizations
library(dplyr) #for data manipulation
library(readr) #for reading rectangular text data
library(geosphere) #for geographical location
library(scales) #for scale functions for visualizations
library(clipr) #for undo function
library(stringr) #for searching text string
