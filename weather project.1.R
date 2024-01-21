seq.Date(2023-01-01, 2023-01-25
)

seq(32, 44)
print(mean(25:82))
print(sum(41:68))
new.function <- function(a) {for(i in 1:a) {b <- i^2 print(b)}} # call the function new.function supplying 6 as an argument
new. function(6)
new.function <- function(a = 1, b = 2, c = 3) {result <- a * b + c print(result)}
library(dplyr)
library(stringr)

install.packages("rlang")
install.packages("tidymodels")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("dplyr")
install.packages("stringr")




library(tidymodels)
library(tidyverse)
library(rlang)
library(magrittr)
library(dplyr)
library(stringr)

URL = 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'
download.file(URL, destfile = "noaa-weather-sample-data.tar.gz")
untar("noaa-weather-sample-data.tar.gz", tar = "internal")
weather_sample <- read.csv("noaa-weather-sample-data/jfk_weather_sample.csv")
head(weather_sample, 6)
str(weather_sample)
weather_df <- weather_sample %>% select('HOURLYRelativeHumidity', 'HOURLYDRYBULBTEMPF', 'HOURLYPrecip', 'HOURLYWindSpeed', 'HOURLYStationPressure')
head(weather_df, 10)
unique(weather_df$HOURLYPrecip)

weather_df1 <- weather_df %>%
  
  mutate(HOURLYPrecip = as.character(replace(HOURLYPrecip, HOURLYPrecip == "T", "0.0")),
         
         HOURLYPrecip = str_remove(HOURLYPrecip, pattern = "s$"))

weather_df2 <- weather_df1[complete.cases(weather_df1), ]

glimpse(weather_df2)

weather_df2$HOURLYPrecip <- (as.numeric(weather_df2$HOURLYPrecip))
glimpse(weather_df2)
weather_df2 <- rename(HOURLYRelativeHumidity = 'relative_humidity', HOURLYDRYBULBTEMPF = 'dry_bulb_temp_f', HOURLYPrecip = 'precip', HOURLYWindSpeed = 'wind_speed', HOURLYStationPressure = 'station_pressure')
glimpse(weather_df2)
set.seed(1234)
weather_split <- initial_split(weather_df2, prop = 0.8)
library(ggplot2)
ggplot(data = weather_df2, mapping = aes(x = HOURLYRelativeHumidity,y = HOURLYPrecip)) + geom_histogram(bins = 100, color = "white", fill = "red") + coord_cartesian(xlim = NULL)

linear_model <- lm(HOURLYPrecip ~ HOURLYRelativeHumidity, data = weather_split)