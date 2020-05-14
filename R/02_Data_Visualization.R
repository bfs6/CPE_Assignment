####Read in Libraries####
library(data.table)
library(tidyverse) 
library(dplyr)
library(Hmisc)
library(ggplot2)
library(scales)
library(ggthemes)


####Read in Data####
neighborhood_crime_stats_raw <- fread("data-raw/neighborhood_crime_stats.csv", sep = ",", header = TRUE)


####Clean Data####
##Filter Neighborhoods and UCR Description
filterNeighborhoods <- c("Cedar Riverside", "Downtown West", "Lyndale",
                         "Seward", "Near North")
neighborhood_crime_stats_raw$neighborhood <- gsub("Near - North", "Near North", neighborhood_crime_stats_raw$neighborhood)
neighborhood_crime_stats <-
  neighborhood_crime_stats_raw %>%
  filter(neighborhood %in% filterNeighborhoods) %>%
  filter(ucrDescription %nin% c("Arson", "Auto Theft"))

##Aggregate Data at Neighborhood/UCR Description Level
aggregated_neighborhood_crime_stats <- 
  neighborhood_crime_stats %>%
  group_by(neighborhood, ucrDescription) %>%
  dplyr::summarize(crime_count = sum(number, na.rm = TRUE))

##Calculate Totals by UCR
crime_count_by_ucr <-
  neighborhood_crime_stats_raw %>%
  group_by(ucrDescription) %>%
  dplyr::summarize(total_crime_count = sum(number, na.rm = TRUE))

##Merge Back into Other DF
aggregated_neighborhood_crime_stats <- merge(aggregated_neighborhood_crime_stats, crime_count_by_ucr, by = "ucrDescription", all.x = TRUE)
 
##Calculate Percentages
aggregated_neighborhood_crime_stats <-
  aggregated_neighborhood_crime_stats %>%
  mutate(crime_percentage = crime_count/total_crime_count)


####Visualization####
##Calculate Top of Y Axis
possScales <- c(0:4)/4
maxVal <- max(aggregated_neighborhood_crime_stats$crime_percentage)
yMaxInd <- tail(which(possScales <= maxVal), 1)
yMax <- possScales[yMaxInd + 1]

##GGPlot
crime_plot <- ggplot(aggregated_neighborhood_crime_stats,
                     aes(fill = neighborhood, x = ucrDescription, y = crime_percentage)) +
  geom_bar(position = "dodge", stat = "identity") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab("UCR Description") + ylab("Percentage of Total Incidents") + labs(fill = "Neighborhood") +
  ggtitle("Minnesota Neighborhood Crime Statistics") + coord_cartesian(ylim = c(0, yMax)) +
  theme_solarized()
ggsave(filename = "plots/CPE_Data_Visualization.png", crime_plot, dpi = 300, width = 15, height = 10)


