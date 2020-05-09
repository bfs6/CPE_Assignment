####Read in Libraries####
library(data.table)
library(tidyverse)
library(dplyr)
library(Hmisc)


####Read in Data####
pedestrian_stops <- fread("data-raw/pedestrian_stops.csv", sep = ",", header = TRUE)
police_stop_data <- fread("data-raw/police_stop_data.csv", sep = ",", header = TRUE)
police_use_of_force <- fread("data-raw/police_use_of_force.csv", sep = ",", header = TRUE)
neighborhood_crime_stats <- fread("data-raw/neighborhood_crime_stats.csv", sep = ",", header = TRUE)




