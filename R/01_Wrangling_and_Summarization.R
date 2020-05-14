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


####Part 1####
police_use_of_force_tidy <-
  police_use_of_force %>% 
  select(CaseNumber, SubjectRoleNumber, ForceType) %>% 
  distinct() %>%
  group_by(CaseNumber, SubjectRoleNumber) %>%
  mutate(ForceTypeNum = 1:n(), varType = "ForceType") %>%
  unite(combi, varType, ForceTypeNum) %>% 
  spread(combi, ForceType)
number_of_police_force_incidents <- nrow(police_use_of_force_tidy) 
  

####Part 2####
##Get Rid of Invalid Stops
invalid_codes <- c("GOA", "AOK", "UTL", "TOW", 
                   "SEC", "CNL", "UNF", "NOS",
                   "AQT", "FTC", "FAL")
valid_stops <-
  police_stop_data %>%
  separate(callDisposition, c("callDispositionCode", "callDispositionDescription"), sep = "-") %>%
  filter(callDispositionCode %nin% invalid_codes)

##Vehicle Stops
vehicle_stop_problems <- c("Traffic Law Enforcement (P)", "Suspicious Vehicle (P)")
v_stops <- valid_stops %>%
  filter(problem %in% vehicle_stop_problems)

##Pedestrian Stops 
p_stops <- valid_stops %>%
  filter(problem %nin% vehicle_stop_problems)


####Part 3####
##Function to Adjust Races
fix_race <- function(df){
  df[df == ""] = NA
  df$race <- gsub("East African", "Black", df$race)
  df$race <- gsub("Latino", "Latinx", df$race)
  df$race[is.na(df$race)] = "Other"
  df$race <- gsub("Other|Unknown", "Other Race", df$race)
  return(df)
}

##Vehicle and Pedestrian Stops by Race
v_stops <- fix_race(df = v_stops)
p_stops <- fix_race(df = p_stops)
v_stops_race <-
  v_stops %>%
  group_by(race) %>%
  dplyr::summarize(vehicle_stops = n())
p_stops_race <- 
  p_stops %>%
  group_by(race) %>% 
  dplyr::summarize(pedestrian_stops = n())
stops_by_race <- merge(p_stops_race, v_stops_race, by = "race", all = TRUE)

##Percentage of Stops by Race
stops_by_race <- 
  stops_by_race %>% 
  mutate(percentage_pedestrian_stops = pedestrian_stops/sum(pedestrian_stops),
         percentage_vehicle_stops = vehicle_stops/sum(vehicle_stops))

##Percentage of Stops where Search Conducted by Race
search_v_stops_by_race <- 
  v_stops %>%
  group_by(race, vehicleSearch) %>%
  dplyr::summarize(count = n()) %>%
  mutate(searchType = "vehicleSearch") %>%
  unite(combi, searchType, vehicleSearch) %>%
  spread(combi, count) %>%
  select(c(race, vehicleSearch_YES))

search_p_stops_by_race <- 
  p_stops %>%
  group_by(race, personSearch) %>%
  dplyr::summarize(count = n()) %>%
  mutate(searchType = "personSearch") %>%
  unite(combi, searchType, personSearch) %>%
  spread(combi, count) %>%
  select(c(race, personSearch_YES))

search_stops <- merge(search_v_stops_by_race, search_p_stops_by_race, by = "race", all = TRUE)
stops_by_race <- merge(stops_by_race, search_stops, by = "race", all = TRUE)
stops_by_race <-
  stops_by_race %>%
  mutate(percentage_vehicle_searches = vehicleSearch_YES/vehicle_stops,
         percentage_pedestrian_searches = personSearch_YES/pedestrian_stops) %>%
  select(-c(vehicleSearch_YES, personSearch_YES))

##How does Minneapolis Population Match up with Other Metrics
population_survey_data <- data.frame(race = c("Asian", "Black", "Latinx", "Native American", "Other Race", "White"),
                                     population_percentage = c(0.06, 0.169, 0.091, 0.009, 0.047, 0.624))
stops_by_race <- merge(stops_by_race, population_survey_data, by = "race", all = TRUE)
metric_cols <- c("percentage_pedestrian_stops", "percentage_vehicle_stops",
                 "percentage_vehicle_searches", "percentage_pedestrian_searches")
stops_by_race_DT <- setDT(stops_by_race)[, paste0("diff_", metric_cols) := get("population_percentage") - .SD, .SDcols = metric_cols]
stops_by_race_DT <- stops_by_race_DT[, "mean_difference_percentage" := rowMeans(.SD), .SDcols = paste0("diff_", metric_cols)]













