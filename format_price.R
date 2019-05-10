rm(list=ls())

library(dplyr)
library(ggmap)
library(gridExtra)
library(stringr)
library(lucr)
library(reshape2)

setwd("~/Desktop/data/")
prc <- read.csv("PriceData.csv")
loc <- read.csv("locations.csv")

## Clean up price data to get it in the format you'd like and join with locations
price <- prc %>% filter(product_source == "Local") %>% 
  filter(country != "Haiti") 

## Removing non-ag things
price <- price[!str_detect(price$product, 
                           c("us", "Labor", "Cattle", "Goats",
                             "Sheep", "Beef", "Camel", "Fish",
                             "Gasoline", "Diesel", "Palm")),]

price$period_date <- as.Date(price$period_date, format="%m/%d/%y")
## Fix currencies so they're more consistent

## Add in some year and location data
price$location <- paste0(price$country, ", ", price$market)
price$year <- format(as.Date(price$period_date, format="%m/%d/%y"),"%Y")
price <- left_join(price, loc, by = "location") 

## Download price conversion rates and deflation calculator
prc_conv <- read.csv("price_conversion.csv")
def_conv <- read.csv("deflation_vals/deflation_calc.csv", check.names = F)

## Format deflation data
def_conv <- melt(def_conv, na.rm = T)
def_conv <- def_conv %>% 
  dplyr::select("Country Name", "Country Code", "Indicator Name", "variable", "value") %>%
  rename(deflation_calc = value)

## Calculate deflated rates 
#divide deflation by 100, then devide rpie y that
#run log of the converted value not the usd con
price2 <- left_join(price, def_conv, by = c("country"="Country Name", "year" = "variable"))
price2 <- price2 %>% filter(!is.na(deflation_calc))
price2$def_value <- price2$value / (price2$deflation_calc / 100)

## Calculate USD value
#price2 <- left_join(price2, prc_conv, by = "currency")
#price2$us_value <- price2$value / price2$usd

# Clean up unusable data
price2 <- price2[which(!is.na(price2$latitude)),]

# Adjust dates
price2$period_date <- as.Date(price2$period_date, format = "%Y-%m-%d")
price2$month <- format(price2$period_date, "%m")
price2 <- price2 %>% mutate(def_value = ifelse(unit == "100_kg", def_value/100, def_value))
price <- price2

