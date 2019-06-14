setwd("/Users/amina/Documents/Stanford/precip-price")
library(lfe)
library(dplyr)

#If you don't need to extract, just use this to get pp_data together
buf <- 2

rdsname <- paste0("precip/", buf, "_precip.rds")
precip <- readRDS(rdsname)
precipname <- paste0("precip/", buf, "_ppdata.csv")

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

pp_data <- clean_precip(precip)

#divide deflation by 100, then devide rpie y that
#run log of the converted value not the usd con#pp_data <- clean_precip(precip)
#### Set of functions for standardizing regression runs

##correctly filter out the data for analysis
filter_grain <- function(data, grain) {
  
  if(grain == "Sorghum") {
    
    dta <- data[str_detect(data$product, "Sorghum"),]
    dta <- dta[!str_detect(dta$product, "Sorghum Flour"),]
    
  } else if(grain == "Millet") {
    
    dta <- data[str_detect(data$product, "Millet"),]
    
  } else if(grain == "Maize") {
    
    dta <- data[str_detect(data$product, "Maize"),]
    dta <- dta[!str_detect(dta$product, "Meal"),]
    dta <- dta %>% filter(country != "Zimbabwe")
    
  }
  
  return(dta)
}

##get the seasons and merge data
merge_seasons <- function(data, grn) {
  
  szn <- read.csv("downloaded/growing_seasons.csv")
  szn <- szn %>% filter(grain == grn)
  merged <- left_join(data, szn)
  return(merged)
}

##fix dates
#s < max == minus 1 year
#s > max == same year 
filter_dates <- function(data, grn) {
  
  breakdown <- data %>% group_by(location, product) %>% 
    summarize(minyear = as.Date(format(min(period_date, na.rm = T), "%m/%d/%Y"), "%m/%d/%Y"),
              maxyear = as.Date(format(max(period_date, na.rm = T), "%m/%d/%Y"), "%m/%d/%Y"))
  
  #note that these dates were chosen by checking for maximum overlap in data. Earliest date is 1 year before 
  #the intended time period for use in precip calculation. It is removed after precip calcs are done
  dates <- switch(grn, "Sorghum" = c(as.Date("03/01/2008", "%m/%d/%Y"), as.Date("09/01/2017", "%m/%d/%Y")),
                  "Maize" = c(as.Date("05/02/2011", "%m/%d/%Y"), as.Date("12/27/2017", "%m/%d/%Y")),
                  "Millet" = c(as.Date("02/01/2010", "%m/%d/%Y"), as.Date("08/30/2018", "%m/%d/%Y")))
  
  data_markets <- breakdown %>% 
    filter(minyear >= dates[1]) %>% 
    filter(maxyear <= dates[2])
  
  data <- data %>% filter(period_date >= dates[1]) %>% 
    filter(period_date <= as.Date("01/01/2018", "%m/%d/%Y")) %>%
    filter(location %in% data_markets$location)
  
  return(data)
}

##calculate precip
calc_precip <- function(data) {
  
  #check first to see if we've already calculated this.
  calcname <- paste0("precip/calc_", buf, "_ppdata.rds")
  if(file.exists(calcname)) return(readRDS(calcname))
  
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
      group_by(country, location, product, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- sum(rain_dates$avg_rainfall)
    
    data$p_sow[i] <- rain
    
    #growing season
    yr <- ifelse(cur_month > data$ge[i], cur_yr, cur_yr - 1)
    early_date <- as.Date(paste0(yr, "-", data$gs[i], "-01"), format = "%Y-%m-%d") 
    late_date <- as.Date(paste0(yr, "-", data$ge[i], "-01"), format = "%Y-%m-%d") 
    
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      filter(date <= late_date) %>%
      group_by(country, location, product, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- sum(rain_dates$avg_rainfall)
    
    data$p_grow[i] <- rain
    
    #harvest season
    yr <- ifelse(cur_month > data$he[i], cur_yr, cur_yr - 1)
    early_date <- as.Date(paste0(yr, "-", data$hs[i], "-01"), format = "%Y-%m-%d") 
    late_date <- as.Date(paste0(yr, "-", data$he[i], "-01"), format = "%Y-%m-%d") 
    
    rain_dates <- data %>%
      filter(location == data$location[i]) %>%
      filter(date >= early_date) %>%
      filter(date <= late_date) %>%
      group_by(country, location, product, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- sum(rain_dates$avg_rainfall)
    
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
      group_by(country, location, product, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- sum(rain_dates$avg_rainfall)
    
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
      group_by(country, location, product, date) %>%
      summarise(avg_rainfall = mean(avg_rainfall, na.rm = T))
    rain <- sum(rain_dates$avg_rainfall)
    
    data$p_onemonth[i] <- rain
  }
  
  #Get rid of the first year, just there for calculation purposes
  data <- data %>% filter(period_date >= min(period_date) + 365) 
  
  saveRDS(data, calcname)
  return(data)
  
}

##### REGRESSIONS #####

## regress as linear
linear_regression <- function(data, var) {
  
  switch(var, 
         "p_sow" = mod_data <- felm(log(def_value) ~ p_sow | market + yrmnth, data=data),
         "p_grow" = mod_data <- felm(log(def_value) ~ p_grow | market + yrmnth, data=data),
         "p_harv" = mod_data <- felm(log(def_value) ~ p_harv | market + yrmnth, data=data),
         "p_sup" = mod_data <- felm(log(def_value) ~ p_sup | market + yrmnth, data=data),
         "p_onemonth" = mod_data <- felm(log(def_value) ~ p_onemonth | market + yrmnth, data=data))
  
  return(mod_data)
  
}

##regress as quadratic
quad_regression <- function(data, var) {
  
  switch(var, 
         "p_sow" = mod_data <- felm(log(def_value) ~ poly(p_sow,2,raw=T) | market + yrmnth, data=data),
         "p_grow" = mod_data <- felm(log(def_value) ~ poly(p_grow,2,raw=T) | market + yrmnth, data=data),
         "p_harv" = mod_data <- felm(log(def_value) ~ poly(p_harv,2,raw=T) | market + yrmnth, data=data),
         "p_sup" = mod_data <- felm(log(def_value) ~ poly(p_sup,2,raw=T) | market + yrmnth, data=data),
         "p_onemonth" = mod_data <- felm(log(def_value) ~ poly(p_onemonth,2,raw=T) | market + yrmnth, data=data))
  
  return(mod_data)
  
}

## linear interaction model with fixed effects
linear_regression_interaction <- function(data, var) {
  
  switch(var, 
         "p_sow" = mod_data <- lm(log(def_value) ~ p_sow +  p_sow * country  + as.factor(market) + as.factor(yrmnth), data=data),
         "p_grow" = mod_data <- lm(log(def_value) ~ p_grow +  p_grow * country  + as.factor(market) + as.factor(yrmnth), data=data),
         "p_harv" = mod_data <- lm(log(def_value) ~ p_harv +  p_harv * country  + as.factor(market) + as.factor(yrmnth), data=data),
         "p_sup" = mod_data <- lm(log(def_value) ~ p_sup +  p_sup * country  + as.factor(market) + as.factor(yrmnth), data=data),
         "p_onemonth" = mod_data <- lm(log(def_value) ~ p_onemonth +  p_onemonth * country  + as.factor(market) + as.factor(yrmnth), data=data))
  
  return(mod_data)
  
}


##### BOOTSTRAPS #####

## linear bootstrap
bootstrap_data_lin <- function(data, mod, var, short=F, name="") {
  
  #check to see if we've run this before. If so just return it
  bootfile <- paste0("boostraps/", buf, "_linear_", name, "_", var)
  if(file.exists(bootfile) && name != "") return(readRDS(bootfile))
  
  num <- ifelse(short, 100, 1000)
  x = 0:700
  yy = x*mod$coefficients[1] 
  
  coef <- matrix(nrow=num,ncol=2)  
  ll = dim(data)[1]  #the number of observations we have in the original dataset
  for (i in 1:num)  {
    samp <- sample(1:ll,size=ll,replace=T)  
    newdata = data[samp,]
    #estimate our regression y = b1*T + b2*T^2
    switch(var, 
           "p_sow" = model <- felm(log(def_value) ~ p_sow | market + yrmnth, data=newdata),
           "p_grow" = model <- felm(log(def_value) ~ p_grow | market + yrmnth, data=newdata),
           "p_harv" = model <- felm(log(def_value) ~ p_harv | market + yrmnth, data=newdata),
           "p_sup" = model <- felm(log(def_value) ~ p_sup | market + yrmnth, data=newdata),
           "p_onemonth" = model <- felm(log(def_value) ~ p_onemonth | market + yrmnth, data=newdata))
    #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
    coef[i,] <- coef(model) 
    print(i)  #print this out so you can watch progress :)
  }
  
  #save it out for the next run if name was provided
  returnlist <- list(coef, x, yy)
  if(name != "") saveRDS(returnlist, bootfile)
  
  return(returnlist)
  
}

##bootstrap and graph
#if you would like a saved out version, please include a name in the arguments
bootstrap_data <- function(data, mod, var, short=F, name="") {
  
  #check to see if we've run this before. If so just return it
  bootfile <- paste0("boostraps/", buf, "_quad_", name, "_", var)
  if(file.exists(bootfile) && name != "") return(readRDS(bootfile))
  
  num <- ifelse(short, 100, 1000)
  x = 0:700
  yy = x*mod$coefficients[1] 
  
  coef <- matrix(nrow=num,ncol=2)  
  ll = dim(data)[1]  #the number of observations we have in the original dataset: 6584
  for (i in 1:num)  {
    samp <- sample(1:ll,size=ll,replace=T)  
    newdata = data[samp,]
    #estimate our regression y = b1*T + b2*T^2
    switch(var, 
           "p_sow" = model <- felm(log(def_value) ~ poly(p_sow,2,raw=T) | market + yrmnth, data=newdata),
           "p_grow" = model <- felm(log(def_value) ~ poly(p_grow,2,raw=T) | market + yrmnth, data=newdata),
           "p_harv" = model <- felm(log(def_value) ~ poly(p_harv,2,raw=T) | market + yrmnth, data=newdata), 
           "p_sup" = model <- felm(log(def_value) ~ poly(p_sup,2,raw=T) | market + yrmnth, data=newdata),
           "p_onemonth" = model <- felm(log(def_value) ~ poly(p_onemonth,2,raw=T) | market + yrmnth, data=newdata))
    #extract the coefficient estimates of b1 and b2 and store them in the matrix we made above
    coef[i,] <- coef(model) 
    print(i)  #print this out so you can watch progress :)
  }
  
  #save it out for the next run if name was provided
  returnlist <- list(coef, x, yy)
  if(name != "") saveRDS(returnlist, bootfile)
  
  return(returnlist)
  
}
