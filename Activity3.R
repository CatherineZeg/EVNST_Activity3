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

#plot each country's CO2 emissions on the same graph using ggplot
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

#plot the CO2 emissions of the three chosen countries
ggplot(pfi_CO2, 
       aes(x = Year, y = CO2, col = Entity)) +
  geom_line(size = 0.75) +
  scale_color_manual(
    values = c("France" = "#EE4266", "Poland" = "#FFD23F", "Italy" = "#540D6E"),
    breaks = c("France", "Poland", "Italy") 
  )+
  labs(title = "CO2 Emissions of France, Poland, and Italy",
       x = "Year",
       y = "Fossil Fuel emissions (billions of tons of CO2)",
       color = "Country") +
  theme_minimal()
  
  
#Question 2:
  
#create df summing all countries CO2 emissions per year
CO2_emissions_agg <- datCO2 %>%
  group_by(Year) %>%
  summarise(total = sum(CO2))

#use ggplot to plot World CO2 emissions
ggplot(CO2_emissions_agg, 
       aes(x = Year, y = total)) +
      geom_line() +
  labs(title = "CO2 Emissions (1750 - 2020)",
       x = "Year",
       y = "Fossil Fuel Emissions (billions of tons of CO2)") +
  theme_minimal() 

#create df summing all hemispheres temperature anomalies per date
temp_anomaly_agg <- climate_df %>%
  group_by(date) %>%
  summarise(total = sum(temperature_anomaly))

#use ggplot to plot the world temp
ggplot(temp_anomaly_agg, 
       aes(x = date, y = total, fill = total)) +
  geom_area(color = "#48B8D0",
            fill = "#8AE1FC",
            alpha = 0.5,
            show.legend = FALSE) +
  theme_minimal() +
  labs(title = "World Temperature (1880 - 2021)",
       x = "Year",
       y = "Temperature (celcius)")

#Question 3:
#trying to recreate the plot on page 9 of FISHERIES OF THE UNITED STATES 2023 report
#https://s3.amazonaws.com/media.fisheries.noaa.gov/2026-02/FUS-2023-web.pdf

#load in the data from NOAA fisheries
fisheries <- read.csv("/cloud/project/activity03/FOSS_landings.csv", skip = 1)

#removes row 11, 2024, so that all the data is from 2014-2023
fisheries_new <- fisheries[-c(11),]

#Insert int versions of Pounds, Metric.Tons, and Dollars, columns of fisheries df
fisheries_new$Pounds_int <- as.numeric(gsub(",", "", fisheries_new$Pounds))
fisheries_new$Metric.Tons_int <- as.numeric(gsub(",", "", fisheries_new$Metric.Tons))
fisheries_new$Dollars_int <- as.numeric(gsub(",", "", fisheries_new$Dollars))

#plot Landings and Landings revenue using base R
#create bar plot of the total Landings per year
barplot(fisheries_new$Pounds_int,
        names.arg = fisheries_new$Year,
        xlab = substitute(paste(bold("Year"))),
        ylab = substitute(paste(bold("Landings"))),
        col = "#7eb2d4",
        ylim = c(0,12000000000),
        yaxt = "n",
        xaxs = "i",
        xaxt = "n",
        family = "Arial") 
#outlines the graph
box()
#Adds title
title("U.S. Commercial Landings and Revenue",
      adj = 0, 
      line = 3)
#Adds second y-axis title
mtext(substitute(paste(bold("Landings Revenue"))), 
      side = 4,
      line = 2,
      adj = 0.5)
#adds more text to the main title
mtext(substitute(paste(italic("from 2014 to 2023"))), 
      side = 3,
      line = 2,
      adj = 0)
#adds more text to the first y-axis title
mtext(substitute(paste(italic("(billions of pounds)"))), 
      side = 2,
      line = 2)
#adds more text to the second y-axis title
mtext(substitute(paste(italic("(billions of real 2023 US dollars)"))), 
      side = 4,
      line = 3)
#Adds axis ticks on the left y-axis
axis(2, seq(0, 12000000000, by = 3000000000),
     seq(0,12, by = 3), las = 2, tck = 0) 
#Adds axis ticks on the x-axis
axis(1, at = c(2, 4.25, 6.75, 9.25, 11.5),
    labels = c("2015", "2017", "2019", "2021", "2023"),
    tck = 0)
#set the margin dimensions
par(mar = c(5, 5, 5, 5), xpd=TRUE)
#allows pairing the second graph on top
par(new = TRUE) 
#plot a line graph from the Landings Revenue
plot(fisheries_new$Year, 
     fisheries_new$Dollars_int,
     pch = 21, 
     type = "o",
     col = "#313d79",
     bg = "#313d79",
     cex = 1,
     lwd = 3,
     axes = FALSE,
     ylab = "", 
     xlab = "",
     ylim = c(0, 12000000000))
#Adds axis ticks on the right y-axis
axis(4, seq(0, 12000000000, by = 3000000000),
     seq(0,12, by = 3), las = 2, tck = 0)
#Adds legend, referring to the symbols of Landings and Revenue
mtext("⏹ Landings ⏺ Revenue", side = 3, line = 0.5)

