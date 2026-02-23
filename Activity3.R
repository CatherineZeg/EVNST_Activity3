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
       aes(x = Year, y = total)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
       y = "Total CO2 Emissions of the US, Canada, and Mexico",
       title = "North America CO2 Emissions")
  
#HW ----
  
#Question 1:

#creating a data frame of Polish, French, and Italian emissions
pfi_CO2 <- datCO2[datCO2$Entity == "Poland" | 
                       datCO2$Entity == "France" |
                       datCO2$Entity == "Italy",]

no_scientific <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)

#plot the CO2 emissions of the three chosen countries
ggplot(pfi_CO2, 
       aes(x = Year, y = CO2, col = Entity)) +
  geom_point() +
  geom_line() +
  labs(title = "Emissions",
       x = "Year",
       y = "Fossil Fuel emissions") 
  #scale_y_continuous(breaks = seq(0, 6000000000, by = 200000000), labels = no_scientific) 
  #theme(axis.ticks.length  = unit(0.5, "cm"))
  #scale_y_continuous(labels = no_scientific)
  
  
#Question 2:
  
#create df summing all countries CO2 emissions per year
CO2_emissions_agg <- datCO2 %>%
  group_by(Year) %>%
  summarise(total = sum(CO2))

#use ggplot to plot World CO2 emissions
ggplot(CO2_emissions_agg, 
       aes(x = Year, y = total)) +
      geom_point() +
      geom_line() +
  labs(title = "Total World CO2 emissions",
       x = "Year",
       y = "Fossil Fuel Emissions")

#create df summing all hemispheres temperature anomalies per date
temp_anomaly_agg <- climate_df %>%
  group_by(date) %>%
  summarise(total = sum(temperature_anomaly))

#use ggplot to plot the world temp
ggplot(temp_anomaly_agg, 
       aes(x = date, y = total)) +
  geom_point() +
  geom_line() +
  labs(title = "World Temperature",
       x = "Year",
       y = "Temperature")
       

#Question 3:
#trying to recreate the plot on page 9 of FISHERIES OF THE UNITED STATES 2023 report
#https://s3.amazonaws.com/media.fisheries.noaa.gov/2026-02/FUS-2023-web.pdf

#load in the data from NOAA fisheries
fisheries <- read.csv("/cloud/project/activity03/FOSS_landings.csv", skip = 1)

#removes row 11, 2024, so that all the data is from 2014-2023
fisheries_new <- fisheries[-c(11),]

str(fisheries_new)

#Insert int versions of Pounds, Metric.Tons, and Dollars, columns of fisheries df
fisheries_new$Pounds_int <- as.numeric(gsub(",", "", fisheries_new$Pounds))
fisheries_new$Metric.Tons_int <- as.numeric(gsub(",", "", fisheries_new$Metric.Tons))
fisheries_new$Dollars_int <- as.numeric(gsub(",", "", fisheries_new$Dollars))

#loading in impact font
install.packages("showtext")
library(showtext)
font_add("impact", "/cloud/project/impact.ttf")

font_add_google("impact", family = "impact")

                                   
barplot(fisheries_new$Pounds_int,
        names.arg = fisheries_new$Year,
        xlab = "Year",
        ylab = "Landings (billions of pounds)",
        yaxt = "n",
        xaxt = "n",
        family = "Arial") 
title("U.S. Commercial Landings and Revenue", 
      adj = 0, 
      line = 3)
#font.main = "impact")
mtext("from 2014 to 2023", 
      side = 3,
      line = 2,
      adj = 0)
axis(2, seq(0, 12000000000, by = 3000000000),
     seq(0,12, by = 3), las = 2, tck = 0) 
axis(1, at = c(0.5, 2.5, 4.5, 6.5, 11.5),
    labels = c("2015", "2017", "2019", "2021", "2023"), tck = 0)
axis(4, seq(0, 12000000000, by = 3000000000),
     seq(0,12, by = 3), las = 2, tck = 0)
par(new = TRUE) 
plot(fisheries_new$Dollars_int, 
     pch = 19, 
     type = "b",
     col = "blue", 
     axes = FALSE,
     ylab = "", 
     xlab = "")

  
