ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
library(lfe)
library(dplyr)
library(extRemes)
library(lubridate)
library(stringr)

##### Data Cleaning #####

#Clean up the precip data a little bit 
clean_precip <- function(precip) {
  
  precip$date <- as.Date(precip$date, format = "%Y.%m.%d")
  precip$month <-  format(precip$date, "%m")
  precip$year <- format(precip$date, "%Y")
  precip <- precip[which(!is.na(precip$temp_mean)),]
  precip <- precip %>% filter(format(precip$date, "%Y") > 1993) %>% rename(avg_rainfall = temp_mean)
  
  pp_data <- left_join(price, precip, by = c("month", "market" = "location", "year"))
  
  pp_data <- pp_data %>% filter(!is.na(avg_rainfall)) %>% filter(!is.na(value)) %>% filter(def_value != 0)
  pp_data$yrmnth <- paste0(pp_data$year, " ", pp_data$month)
  
  return(pp_data)
}

#divide deflation by 100, then devide rpie y that
#run log of the converted value not the usd con#pp_data <- clean_precip(precip)
#### Set of functions for standardizing regression runs

##correctly filter out the data for analysis
filter_grain <- function(data, grain) {
  
  if(grain == "sorghum") {
    
    dta <- data[str_detect(data$product, "Sorghum"),]
    dta <- dta[!str_detect(dta$product, "Sorghum Flour"),]
    dta$type <- "sorghum"
    
  } else if(grain == "millet") {
    
    dta <- data[str_detect(data$product, "Millet"),]
    dta$type <- "millet"
    
  } else if(grain == "maize") {
    
    dta <- data[str_detect(data$product, "Maize"),]
    dta <- dta[!str_detect(dta$product, "Meal"),]
    dta <- dta %>% filter(country != "Zimbabwe")
    dta$type <- "maize"
    
  }
  
  return(dta)
}

##get the seasons and merge data
merge_seasons <- function(data, grn) {
  
  #get the growing season file and filter
  szn <- read.csv("downloaded/growing_seasons.csv")
  szn <- szn %>% filter(grain == grn)
  
  #update country-specific naming conventions
  if(grn == "maize") data <- data %>% mutate(country = ifelse(location %in% c("Nigeria, Ibadan, Bodija", "Nigeria, Lagos, Mile 12", "Nigeria, Aba"), "Nigeria (S)", country))
  merged <- left_join(data, szn)
  
  return(merged)
}

##fix dates
#s < max == minus 1 year
#s > max == same year 
filter_dates <- function(data, grn) {
  
  breakdown <- data %>% group_by(location, product) %>% 
    summarize(minyear = as.Date(format(min(date, na.rm = T), "%m/%d/%Y"), "%m/%d/%Y"),
              maxyear = as.Date(format(max(date, na.rm = T), "%m/%d/%Y"), "%m/%d/%Y"))
  
  #note that these dates were chosen by checking for maximum overlap in data. Earliest date is 1 year before 
  #the intended time period for use in precip calculation. It is removed after precip calcs are done
  dates <- switch(grn, "sorghum" = c(as.Date("03/01/2008", "%m/%d/%Y"), as.Date("09/01/2017", "%m/%d/%Y")),
                  "maize" = c(as.Date("05/02/2011", "%m/%d/%Y"), as.Date("12/27/2017", "%m/%d/%Y")),
                  "millet" = c(as.Date("02/01/2010", "%m/%d/%Y"), as.Date("08/30/2018", "%m/%d/%Y")))
  
  data <- data %>% filter(date >= dates[1]) %>% filter(date <= dates[2])
  
  #data_markets <- breakdown %>% filter(minyear <= dates[1]) %>% filter(maxyear >= dates[2])
  #data <- data %>% filter(location %in% data_markets$location) %>% filter(date >= dates[1]) %>% filter(date <= dates[2]) 
  
  return(data)
}

##calculate precip
#daily = True means you get daily average values rather than total accumulated precip
calc_precip <- function(data, daily=FALSE, zeros=TRUE, overwrite=FALSE) {
  
  #check first to see if we've already calculated this.
  grn <- data$type[1]
  calcname <- paste0("precip/calc_ppdata_", grn, "_", buf)
  calcname <- ifelse(daily, paste0(calcname, "_average"), paste0(calcname, "_accumulated"))
  calcname <- ifelse(zeros, paste0(calcname, "_zeros.rds"), paste0(calcname, "_no_zeros.rds"))
  if(file.exists(calcname) & !overwrite) return(readRDS(calcname))
  
  #if we are removing zeros, let's get it out of the data now. It's really anything less than 1mm
  if(!zeros) data <- data %>% filter(avg_rainfall >= 1)
  
  data$p_sow <- 0
  data$p_grow <- 0
  data$p_harv <- 0
  data$p_sup <- 0
  
  for (i in 1:nrow(data))  {
    
    cur_month <- as.numeric(data$month[i])
    cur_yr <- as.numeric(data$year[i])
    
    #sowing season
    # if the current month is after the end of the (sowing) season, use this year. 
    #Otherwise use previous year's season is considered
    yr <- ifelse(cur_month > data$se[i], cur_yr, cur_yr - 1)
    #current logic assumes start and end dates happen in the same year
    early_date <- as.Date(paste0(yr, "-", data$ss[i], "-01"), format = "%Y-%m-%d") 
    late_date <- as.Date(paste0(yr, "-", data$se[i], "-01"), format = "%Y-%m-%d") 
    
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      filter(date <= late_date) %>%
      group_by(country, location, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
    
    data$p_sow[i] <- rain
    
    #growing season
    yr <- ifelse(cur_month > data$ge[i], cur_yr, cur_yr - 1)
    early_date <- as.Date(paste0(yr, "-", data$gs[i], "-01"), format = "%Y-%m-%d") 
    late_date <- as.Date(paste0(yr, "-", data$ge[i], "-01"), format = "%Y-%m-%d") 
    
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      filter(date <= late_date) %>%
      group_by(country, location, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
    
    data$p_grow[i] <- rain
    
    #harvest season
    yr <- ifelse(cur_month > data$he[i], cur_yr, cur_yr - 1)
    early_date <- as.Date(paste0(yr, "-", data$hs[i], "-01"), format = "%Y-%m-%d") 
    late_date <- as.Date(paste0(yr, "-", data$he[i], "-01"), format = "%Y-%m-%d") 
    
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      filter(date <= late_date) %>%
      group_by(country, location, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
    
    data$p_harv[i] <- rain
    
    # between harvest and date of sale
    # we don't update year because it is the same logic as the end of harvest szn 
    early_date <- late_date
    late_date <- as.Date(data$date[i], format = "%Y-%m-%d") 
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      #notice that we got rid of the equal to for the late date. Doesn't make sense for supply chain questions
      filter(date < late_date) %>%
      group_by(country, location, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
    
    data$p_sup[i] <- rain
    
    # prior month
    # we don't update year because it is the same logic as the end of harvest szn 
    late_date <- as.Date(data$date[i], format = "%Y-%m-%d") 
    early_date <- seq(late_date, length = 2, by = "-1 months")[2]
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      #notice that we got rid of the equal to for the late date. Doesn't make sense for supply chain questions
      filter(date < late_date) %>%
      group_by(country, location, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
    
    data$p_onemonth[i] <- rain
  
  # prior 2 months
  # we don't update year because it is the same logic as the end of harvest szn 
  late_date <- as.Date(data$date[i], format = "%Y-%m-%d") 
  early_date <- seq(late_date, length = 2, by = "-2 months")[2]
  rain_dates <- data %>%
    filter(location == data$location[i]) %>%
    filter(date >= early_date) %>%
    #notice that we got rid of the equal to for the late date. Doesn't make sense for supply chain questions
    filter(date < late_date) %>%
    group_by(country, location, date) %>%
    summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
  rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
  
  data$p_twom[i] <- rain
  
  # prior 3 months
  # we don't update year because it is the same logic as the end of harvest szn 
  late_date <- as.Date(data$date[i], format = "%Y-%m-%d") 
  early_date <- seq(late_date, length = 2, by = "-3 months")[2]
  rain_dates <- data %>%
    filter(location == data$location[i]) %>%
    filter(date >= early_date) %>%
    #notice that we got rid of the equal to for the late date. Doesn't make sense for supply chain questions
    filter(date < late_date) %>%
    group_by(country, location, date) %>%
    summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
  rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
  
  data$p_threem[i] <- rain
  
  # prior 6 months
  # we don't update year because it is the same logic as the end of harvest szn 
  late_date <- as.Date(data$date[i], format = "%Y-%m-%d") 
  early_date <- seq(late_date, length = 2, by = "-6 months")[2]
  rain_dates <- data %>%
    filter(location == data$location[i]) %>%
    filter(date >= early_date) %>%
    #notice that we got rid of the equal to for the late date. Doesn't make sense for supply chain questions
    filter(date < late_date) %>%
    group_by(country, location, date) %>%
    summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
  rain <- ifelse(daily, mean(rain_dates$avg_rainfall), sum(rain_dates$avg_rainfall))
  
  data$p_sixm[i] <- rain
  
  }

  
  #Get rid of the first year, just there for calculation purposes
  data <- data %>% filter(period_date >= min(period_date) + 365) 
  
  saveRDS(data, calcname)
  return(data)
  
}

##prepare data
#does all the calls necessary to get data back for any given grain. Shortcut
get_grain_data <- function(pp_data, grn, run_daily, include_zeros, overwrite=FALSE) {
  
  data <- filter_grain(pp_data, grn)
  data <- merge_seasons(data, grn)
  data <- filter_dates(data, grn)
  data <- calc_precip(data, daily = run_daily, zeros = include_zeros, overwrite)
  
  return(data)
}

##### REGRESSIONS #####

## regress log data with fixed effects
l_regress <- function(data, var, level) {
  
  if(level == 1) {
    switch(var, 
           "p_sow" = mod_data <- lm(log(def_value) ~ p_sow + as.factor(market) + as.factor(yrmnth), data=data),
           "p_grow" = mod_data <- lm(log(def_value) ~ p_grow + as.factor(market) + as.factor(yrmnth), data=data),
           "p_harv" = mod_data <- lm(log(def_value) ~ p_harv + as.factor(market) + as.factor(yrmnth), data=data),
           "p_sup" = mod_data <- lm(log(def_value) ~ p_sup + as.factor(market) + as.factor(yrmnth), data=data),
           "p_onemonth" = mod_data <- lm(log(def_value) ~ p_onemonth + as.factor(market) + as.factor(yrmnth), data=data),
           "p_twom" = mod_data <- lm(log(def_value) ~ p_twom + as.factor(market) + as.factor(yrmnth), data=data),
           "p_threem" = mod_data <- lm(log(def_value) ~ p_threem + as.factor(market) + as.factor(yrmnth), data=data),
           "p_sixm" = mod_data <- lm(log(def_value) ~ p_sixm + as.factor(market) + as.factor(yrmnth), data=data))
    
  } else if(level == "log") {
    
    log <- function(x) ifelse(x <= 0, 0, base::log(x))
    
    switch(var, 
           "p_sow" = mod_data <- lm(log(def_value) ~ log(p_sow) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_grow" = mod_data <- lm(log(def_value) ~ log(p_grow) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_harv" = mod_data <- lm(log(def_value) ~ log(p_harv) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_sup" = mod_data <- lm(log(def_value) ~ log(p_sup) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_onemonth" = mod_data <- lm(log(def_value) ~ log(p_onemonth) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_twom" = mod_data <- lm(log(def_value) ~ log(p_twom) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_threem" = mod_data <- lm(log(def_value) ~ log(p_threem) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_sixm" = mod_data <- lm(log(def_value) ~ log(p_sixm) + as.factor(market) + as.factor(yrmnth), data=data))
  } else {
    
    log <- function(x) ifelse(x <= 0, 0, base::log(x))
    
    switch(var, 
           "p_sow" = mod_data <- lm(log(def_value) ~ poly(p_sow,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_grow" = mod_data <- lm(log(def_value) ~ poly(p_grow,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_harv" = mod_data <- lm(log(def_value) ~ poly(p_harv,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_sup" = mod_data <- lm(log(def_value) ~ poly(p_sup,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_onemonth" = mod_data <- lm(log(def_value) ~ poly(p_onemonth,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_twom" = mod_data <- lm(log(def_value) ~ poly(p_twom,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_threem" = mod_data <- lm(log(def_value) ~ poly(p_threem,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
           "p_sixm" = mod_data <- lm(log(def_value) ~ poly(p_sixm,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data))
  }
  
  return(mod_data)
  
}

## regress log using felm
l_regress_felm <- function(data, var, level) {
  
  if(level == 1) {
    switch(var, 
           "p_sow" = mod_data <- felm(log(def_value) ~ p_sow | market + yrmnth, data=data),
           "p_grow" = mod_data <- felm(log(def_value) ~ p_grow | market + yrmnth, data=data),
           "p_harv" = mod_data <- felm(log(def_value) ~ p_harv | market + yrmnth, data=data),
           "p_sup" = mod_data <- felm(log(def_value) ~ p_sup | market + yrmnth, data=data),
           "p_onemonth" = mod_data <- felm(log(def_value) ~ p_onemonth | market + yrmnth, data=data),
           "p_twom" = mod_data <- felm(log(def_value) ~ p_twom | market + yrmnth, data=data),
           "p_threem" = mod_data <- felm(log(def_value) ~ p_threem | market + yrmnth, data=data),
           "p_sixm" = mod_data <- felm(log(def_value) ~ p_sixm | market + yrmnth, data=data))
    
  } else if(level == "log") {
    
    log <- function(x) ifelse(x <= 0, 0, base::log(x))
    
    switch(var, 
           "p_sow" = mod_data <- felm(log(def_value) ~ log(p_sow) | market + yrmnth, data=data),
           "p_grow" = mod_data <- felm(log(def_value) ~ log(p_grow) | market + yrmnth, data=data),
           "p_harv" = mod_data <- felm(log(def_value) ~ log(p_harv) | market + yrmnth, data=data),
           "p_sup" = mod_data <- felm(log(def_value) ~ log(p_sup) | market + yrmnth, data=data),
           "p_onemonth" = mod_data <- felm(log(def_value) ~ log(p_onemonth) | market + yrmnth, data=data),
           "p_twom" = mod_data <- felm(log(def_value) ~ log(p_twom) | market + yrmnth, data=data),
           "p_threem" = mod_data <- felm(log(def_value) ~ log(p_threem) | market + yrmnth, data=data),
           "p_sixm" = mod_data <- felm(log(def_value) ~ log(p_sixm) | market + yrmnth, data=data))
  } else {
    
    log <- function(x) ifelse(x <= 0, 0, base::log(x))
    
    switch(var, 
           "p_sow" = mod_data <- felm(log(def_value) ~ poly(p_sow,level,raw=T) | market + yrmnth, data=data),
           "p_grow" = mod_data <- felm(log(def_value) ~ poly(p_grow,level,raw=T) | market + yrmnth, data=data),
           "p_harv" = mod_data <- felm(log(def_value) ~ poly(p_harv,level,raw=T) | market + yrmnth, data=data),
           "p_sup" = mod_data <- felm(log(def_value) ~ poly(p_sup,level,raw=T) | market + yrmnth, data=data),
           "p_onemonth" = mod_data <- felm(log(def_value) ~ poly(p_onemonth,level,raw=T) | market + yrmnth, data=data),
           "p_twom" = mod_data <- felm(log(def_value) ~ poly(p_twom,level,raw=T) | market + yrmnth, data=data),
           "p_threem" = mod_data <- felm(log(def_value) ~ poly(p_threem,level,raw=T) | market + yrmnth, data=data),
           "p_sixm" = mod_data <- felm(log(def_value) ~ poly(p_sixm,level,raw=T) | market + yrmnth, data=data))
  }
  
  return(mod_data)
  
}

#var is the time period we are aiming for. Should be a column in data
#level is either a value 1-i or "log." referes to independent variable
#log is if we want the dependent variable (value) to be logged before running the regression
get_model_regression <- function(data, var, level, log=TRUE) {
  
  if(log){
    
    if(level == 1) {
      switch(var, 
             "p_sow" = mod_data <- felm(log(def_value) ~ p_sow | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm(log(def_value) ~ p_grow | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm(log(def_value) ~ p_harv | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm(log(def_value) ~ p_sup | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm(log(def_value) ~ p_onemonth | market + yrmnth, data=data),
             "p_twom" = mod_data <- felm(log(def_value) ~ p_twom | market + yrmnth, data=data),
             "p_threem" = mod_data <- felm(log(def_value) ~ p_threem | market + yrmnth, data=data),
             "p_sixm" = mod_data <- felm(log(def_value) ~ p_sixm | market + yrmnth, data=data))
      
    } else if(level == "log") {
      log <- function(x) ifelse(x <= 0, 0, base::log(x))
      switch(var, 
             "p_sow" = mod_data <- felm(log(def_value) ~ log(p_sow) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm(log(def_value) ~ log(p_grow) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm(log(def_value) ~ log(p_harv) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm(log(def_value) ~ log(p_sup) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm(log(def_value) ~ log(p_onemonth) | market + yrmnth, data=data),
             "p_twom" = mod_data <- felm(log(def_value) ~ log(p_twom) | market + yrmnth, data=data),
             "p_threem" = mod_data <- felm(log(def_value) ~ log(p_threem) | market + yrmnth, data=data),
             "p_sixm" = mod_data <- felm(log(def_value) ~ log(p_sixm) | market + yrmnth, data=data))
    } else {
      switch(var, 
             "p_sow" = mod_data <- felm(log(def_value) ~ poly(p_sow,level,raw=T) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm(log(def_value) ~ poly(p_grow,level,raw=T) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm(log(def_value) ~ poly(p_harv,level,raw=T) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm(log(def_value) ~ poly(p_sup,level,raw=T) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm(log(def_value) ~ poly(p_onemonth,level,raw=T) | market + yrmnth, data=data),
             "p_twom" = mod_data <- felm(log(def_value) ~ poly(p_twom,level,raw=T) | market + yrmnth, data=data),
             "p_threem" = mod_data <- felm(log(def_value) ~ poly(p_threem,level,raw=T) | market + yrmnth, data=data),
             "p_sixm" = mod_data <- felm(log(def_value) ~ poly(p_sixm,level,raw=T) | market + yrmnth, data=data))
    }
    
    return(mod_data)
    
  } else {
    #run regression but with original price value
    if(level == 1) {
      switch(var, 
             "p_sow" = mod_data <- felm((def_value) ~ p_sow | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm((def_value) ~ p_grow | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm((def_value) ~ p_harv | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm((def_value) ~ p_sup | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm((def_value) ~ p_onemonth | market + yrmnth, data=data),
             "p_twom" = mod_data <- felm((def_value) ~ p_twom | market + yrmnth, data=data),
             "p_threem" = mod_data <- felm((def_value) ~ p_threem | market + yrmnth, data=data),
             "p_sixm" = mod_data <- felm((def_value) ~ p_sixm | market + yrmnth, data=data))
      
    } else if(level == "log") {
      switch(var, 
             "p_sow" = mod_data <- felm((def_value) ~ log(p_sow) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm((def_value) ~ log(p_grow) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm((def_value) ~ log(p_harv) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm((def_value) ~ log(p_sup) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm((def_value) ~ log(p_onemonth) | market + yrmnth, data=data),
             "p_twom" = mod_data <- felm((def_value) ~ log(p_twom) | market + yrmnth, data=data),
             "p_threem" = mod_data <- felm((def_value) ~ log(p_threem) | market + yrmnth, data=data),
             "p_sixm" = mod_data <- felm((def_value) ~ log(p_sixm) | market + yrmnth, data=data))
    } else {
      switch(var, 
             "p_sow" = mod_data <- felm((def_value) ~ poly(p_sow,level,raw=T) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm((def_value) ~ poly(p_grow,level,raw=T) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm((def_value) ~ poly(p_harv,level,raw=T) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm((def_value) ~ poly(p_sup,level,raw=T) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm((def_value) ~ poly(p_onemonth,level,raw=T) | market + yrmnth, data=data),
             "p_twom" = mod_data <- felm((def_value) ~ poly(p_twom,level,raw=T) | market + yrmnth, data=data),
             "p_threem" = mod_data <- felm((def_value) ~ poly(p_threem,level,raw=T) | market + yrmnth, data=data),
             "p_sixm" = mod_data <- felm((def_value) ~ poly(p_sixm,level,raw=T) | market + yrmnth, data=data))
    }
    
    return(mod_data)
  }
  
}

##### BOOTSTRAP #####

##bootstrap and graph
#if you would like a saved out version, please include a name in the arguments
#level stays blank if linear, given a value if poly
bootstrap_data <- function(data, mod, var, short=F, name="", xrange, level, log) {
  
  #check to see if we've run this before. If so just return it
  bootfile <- paste0("bootstraps/", name)
  #if(file.exists(bootfile) && name != "") return(readRDS(bootfile))
  
  num <- ifelse(short, 100, 1000)
  
  coef <- matrix(nrow=num, ncol=ifelse(level=="log", 1, level))
  ll = dim(data)[1]
  
  for (i in 1:num)  {
    samp <- sample(1:ll,size=ll,replace=T)  
    newdata = data[samp,]
    #estimate our regression y = b1*T + b2*T^2
    model <- get_model_regression(newdata, var, level, log)
    #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
    coef[i,] <- coef(model) 
    print(i)  #print this out so you can watch progress 
  
  }
  
  #save it out for the next run if name was provided
  returnlist <- list(coef)
  if(name != "") saveRDS(returnlist, bootfile)
  
  return(returnlist)
  
}

get_bootname <- function(type, buf, time_period, run_daily, include_zeros) {
  
  bootname <- paste0(type, "_", buf, "_", time_period, "_")
  bootname <- ifelse(run_daily, paste0(bootname, "average_"), paste0(bootname, "accumulated_"))
  bootname <- ifelse(include_zeros, paste0(bootname, "zeros.rds"), paste0(bootname, "nozeros.rds"))
  
  return(bootname)
  
}


##### EXTREME VALUES #####

##get extreme days
#currently just getting the quantiles and picking everything at or above 95% percentile BY LOCATION
get_extreme_days <- function(data) {
  
  to_return <- c()
  
  for(loc in unique(data$location)) {
    
    subset <- data %>% filter(location == loc)
    cutoff <- quantile(data$avg_rainfall, probs=c(0.95))
    to_return <- rbind(to_return, subset %>% filter(avg_rainfall > cutoff))
  }
  
  return(to_return)
}

## Make table of average price before and after each day

#data should be unique to one commodity type please

#type is how we want to pick values:
##average is the mean of all prices in date range. 
##single is the single closest value to the day difference

calc_extreme <- function(data, mnts) {
  
  extreme_dates <- get_extreme_days(data)
  to_return <- c()
  
  for(i in 1:nrow(extreme_dates)) {
    
    event_date <- extreme_dates$date[i]
    early_date <- event_date %m-% months(mnts)
    late_date <- event_date %m+% months(mnts) 
    
    dat <- data %>% filter(location == extreme_dates$location[i])
    
    pre_event <- dat %>% filter(date >= early_date) %>% filter(date <= event_date) %>% group_by(location, type) %>% summarise(rain_before = mean(avg_rainfall)) 
    post_event <- dat %>% filter(date > event_date) %>% filter(date <= late_date) %>% group_by(location, type) %>% summarise(rain_after = mean(avg_rainfall))
    
    paired <- left_join(pre_event, post_event, by = c("location", "type"))
    
    to_return <- rbind(to_return, paired)
    
  }
  
  return(to_return)

}




