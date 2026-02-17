#load in libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#read in data
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

#cleaning up data ----
#reassign name of column to CO2
colnames(datCO2)[4] <- "CO2"

#convert the entity names to factor
datCO2$Entity <- as.factor(datCO2$Entity)

#in class work ----
#make a vector of all column names
name.Ent <- levels(datCO2$Entity)
name.Ent

#make a dataframe of only US data
US <- datCO2 %>%
  filter(Entity == "United States")

#another way from the tutorial
US <- datCO2[datCO2$Entity == "United States",]

#plot of US CO2 emissions in base R
plot(US$Year, US$CO2,
     type = "b",
     pch = 19,
     xlab = "Year",
     ylab = "Fossil Fuel emissions (billions of tons of CO2)",
     yaxt = "n")
axis(2, seq(0, 6000000000, by = 2000000000),
     seq(0,6, by = 2), las =2)

#plot of US CO2 emissions using ggplot
ggplot(US, aes(x=Year, y=CO2)) + #chains the plot to the geometries that we want to plot
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Fossil Fuel emissions (tons of CO2)") 

#create new data frame with only countries from North America
NorthA <- datCO2 %>%
  filter(Entity == "United States" |
           Entity == "Mexico" |
           Entity == "Canada")

#plot each country's CO2 emmissions on the same graph using ggplot
ggplot(NorthA, 
       aes(x = Year, y = CO2, color = Entity)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("red", "royalblue", "darkgoldenrod3"))

#In-class prompts

#Read in the csv to help answer the questions
climate_df <- read.csv("/cloud/project/activity03/climate-change.csv")

#clean data by creating a timeseries column of date
climate_df$date <- ymd(climate_df$Day)

#Prompt 1

#isolating just the Northern and Southern Hemisphere data
north_south_df <- climate_df %>%
  filter(Entity == "Northern Hemisphere" |
           Entity == "Southern Hemisphere")

north_df <- climate_df %>%
  filter(Entity == "Northern Hemisphere")

south_df <- climate_df %>%
  filter(Entity == "Southern Hemisphere")

#plotting in base R
plot(north_df$date, north_df$temperature_anomaly,
     type = "l",
     pch = 19,
     xlab = "Date",
     ylab = "Temperature Anomaly (celcius)",
     col = "blue")
points(south_df$date, # x data
       south_df$temperature_anomaly, # y data
       type = "l", 
       pch = 19, 
       col= "red")

#plotting using ggplot
ggplot(north_south_df, 
       aes(x = date, y = temperature_anomaly, color = Entity)) +
  geom_point() +
  geom_line() +
  labs(x = "Date",
       y = "Temperature Anomaly (celcius)") 



#Prompt 2

#create a new df of total CO2 emissions
total_CO2 <- NorthA %>%
  group_by(Year) %>%
  summarise(total = sum(CO2))

#plot the total CO2 emissions
ggplot(total_CO2, 
       aes(x = Year, y = CO2)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y = "Total CO2 Emissions of the US, Canada, and Mexico",
       title = "North America CO2 Emissions")
  


#Optional challenge if you have extra time
Try and find an answer through your search engine: how would you add subscripts for CO in your
plot axes label?
  
