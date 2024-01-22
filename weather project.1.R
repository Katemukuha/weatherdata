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
weather_df3 <- rename(weather_df2, relative_humidity = HOURLYRelativeHumidity, dry_bulb_temp_f = HOURLYDRYBULBTEMPF, precip = HOURLYPrecip, wind_speed = HOURLYWindSpeed, station_pressure = HOURLYStationPressure)
glimpse(weather_df3)

set.seed(1234)
weather_split1 <- initial_split(weather_df3, prop = 0.8)

ggplot(data = weather_df3, mapping = aes(x = (relative_humidity))) + geom_histogram(bins = 100, color = "white", fill ="red" ) + coord_cartesian(xlim = NULL)

linear_model <- lm(precip ~ relative_humidity, data = weather_split1)
summary(linear_model)

linear_model1 <- lm(precip ~ dry_bulb_temp_f, data = weather_split1)
summary(linear_model1)

linear_model2 <- lm(precip ~ wind_speed, data = weather_split1)
summary(linear_model2)

linear_model3 <- lm(precip ~ station_pressure, data = weather_split1)
summary(linear_model3)

ggplot(data = weather_df3, mapping = aes(x = relative_humidity, y = precip)) + geom_point() + geom_smooth(method = "lm", na.rm = TRUE)

ggplot(data = weather_df3, mapping = aes(x = dry_bulb_temp_f, y = precip)) + geom_point() + geom_smooth(method = "lm", na.rm = TRUE)

ggplot(data = weather_df3, mapping = aes(x = wind_speed, y = precip)) + geom_point() + geom_smooth(method = "lm", na.rm = TRUE)

ggplot(data = weather_df3, mapping = aes(x = station_pressure, y = precip)) + geom_point() + geom_smooth(method = "lm", na.rm = TRUE)

weather_recipe <- recipe(precip ~ ., data = weather_df3)
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>% set_engine("glmnet")
ridge_wf <- workflow() %>% add_recipe(weather_recipe)
ridge_fit <- ridge_wf %>% add_model(ridge_spec) %>% fit(data = weather_df3)
ridge_fit %>% pull_workflow_fit() %>% tidy()

