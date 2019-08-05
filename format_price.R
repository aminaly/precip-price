rm(list=ls())
setwd("/Users/amina/Documents/Stanford/precip-price")

library(dplyr)
library(ggmap)
library(gridExtra) 
library(stringr)
library(lucr)
library(reshape2)

prc <- read.csv("downloaded/pricedata_nooutliers.csv")
loc <- read.csv("downloaded/locations.csv")

## Clean up price data to get it in the format you'd like and join with locations
price <- prc %>% filter(product_source == "Local") %>% 
  filter(country != "Haiti") 

## Removing non-ag things
#price <- price[!str_detect(price$product, 
#                           c("us", "Labor", "Cattle", "Goats",
#                             "Sheep", "Beef", "Camel", "Fish",
#                             "Gasoline", "Diesel", "Palm", "Bus",
#                             "Soap", "bond", "chickens", "Mutton", "cow")),]

## Date cleaning & addition
price$period_date <- as.Date(price$period_date, format="%m/%d/%y")
price$location <- paste0(price$country, ", ", price$market)
price$year <- format(as.Date(price$period_date, format="%m/%d/%y"),"%Y")

## Adding in location and removing ones without lat-longs
price <- left_join(price, loc, by = "location") 
price <- price[which(!is.na(price$latitude)),]

## Download price conversion rates and deflation calculator
prc_conv <- read.csv("downloaded/price_conversion.csv")
def_conv <- read.csv("downloaded/deflation_calc.csv", check.names = F)

## Format deflation data
def_conv <- melt(def_conv, na.rm = T)
def_conv <- def_conv %>% 
  dplyr::select("Country Name", "Country Code", "Indicator Name", "variable", "value") %>%
  rename(deflation_calc = value)

## Calculate deflated rates 
#divide deflation by 100, then divide price by that
#run log of the converted value not the usd con
price <- left_join(price, def_conv, by = c("country"="Country Name", "year" = "variable"))
price <- price %>% filter(!is.na(deflation_calc))
price$def_value <- price$value / (price$deflation_calc / 100)

## Calculate USD value
#price2 <- left_join(price2, prc_conv, by = "currency")
#price2$us_value <- price2$value / price2$usd

# Adjust dates
price$period_date <- as.Date(price$period_date, format = "%Y-%m-%d")
price$month <- format(price$period_date, "%m")
price <- price %>% mutate(def_value = ifelse(unit == "100_kg", def_value/100, def_value))

# save out the price
saveRDS(price, "saved-output/formatted-price.rds")
