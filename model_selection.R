### Running some basic code to decide on ideal model for each commodity

# Source files with functions useful/data prep
source('~/Documents/Stanford/precip-price/format_price.R')
all_final <- c()
bufs <- c(.25,1,2)
#If you don't need to extract, just use this to get pp_data together. If you do, you'll need to run precip_extract
for(i in 1:3) {
  
  buf <- bufs[i]
  rdsname <- paste0("precip/", buf, "_precip.rds")
  precip <- readRDS(rdsname)
  precipname <- paste0("precip/", buf, "_ppdata.csv")
  
  source('~/Documents/Stanford/precip-price/Functions for Analysis.R')
  
  ## Get data for each commodity
  #lets get the sorghum we want specificially & merge in seasonality
  sorghum <- filter_grain(pp_data, "Sorghum")
  sorghum <- merge_seasons(sorghum, "sorghum")
  #get locations with the right set of dates
  sorghum <- filter_dates(sorghum, "Sorghum")
  #calcualte precip for correct set of dates
  sorghum <- calc_precip(sorghum)
  sorghum <- sorghum %>% filter(price_type == "Wholesale")
  
  #lets get the millet we want specificially & merge in seasonality
  millet <- filter_grain(pp_data, "Millet")
  millet <- merge_seasons(millet, "millet")
  #get locations with the right set of dates
  millet <- filter_dates(millet, "Millet")
  #calcualte precip for correct set of dates
  millet <- calc_precip(millet)
  millet <- millet %>% filter(price_type == "Wholesale")
  
  #lets get the maize we want specificially & merge in seasonality
  maize <- filter_grain(pp_data, "Maize")
  maize <- merge_seasons(maize, "maize")
  #get locations with the right set of dates
  maize <- filter_dates(maize, "Maize")
  #calcualte precip for correct set of dates
  maize <- calc_precip(maize)
  maize <- maize %>% filter(price_type == "Wholesale")
  
  ##### REGRESSIONS #####
  
  ## regress data with fixed effects
  regress <- function(data, var, level) {
    
    if(level == 1) {
      switch(var, 
             "p_sow" = mod_data <- lm((def_value) ~ p_sow + as.factor(market) + as.factor(yrmnth), data=data),
             "p_grow" = mod_data <- lm((def_value) ~ p_grow + as.factor(market) + as.factor(yrmnth), data=data),
             "p_harv" = mod_data <- lm((def_value) ~ p_harv + as.factor(market) + as.factor(yrmnth), data=data),
             "p_sup" = mod_data <- lm((def_value) ~ p_sup + as.factor(market) + as.factor(yrmnth), data=data),
             "p_onemonth" = mod_data <- lm((def_value) ~ p_onemonth + as.factor(market) + as.factor(yrmnth), data=data))
      
    } else if(level == "log") {
      switch(var, 
             "p_sow" = mod_data <- lm((def_value) ~ log(p_sow) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_grow" = mod_data <- lm((def_value) ~ log(p_grow) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_harv" = mod_data <- lm((def_value) ~ log(p_harv) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_sup" = mod_data <- lm((def_value) ~ log(p_sup) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_onemonth" = mod_data <- lm((def_value) ~ log(p_onemonth) + as.factor(market) + as.factor(yrmnth), data=data))
    } else {
      switch(var, 
             "p_sow" = mod_data <- lm((def_value) ~ poly(p_sow,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_grow" = mod_data <- lm((def_value) ~ poly(p_grow,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_harv" = mod_data <- lm((def_value) ~ poly(p_harv,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_sup" = mod_data <- lm((def_value) ~ poly(p_sup,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_onemonth" = mod_data <- lm((def_value) ~ poly(p_onemonth,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data))
    }
    
    return(mod_data)
    
  }
  
  ## regress using felm
  regress_felm <- function(data, var, level) {
    
    if(level == 1) {
      switch(var, 
             "p_sow" = mod_data <- felm((def_value) ~ p_sow | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm((def_value) ~ p_grow | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm((def_value) ~ p_harv | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm((def_value) ~ p_sup | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm((def_value) ~ p_onemonth | market + yrmnth, data=data))
      
    } else if(level == "log") {
      switch(var, 
             "p_sow" = mod_data <- felm((def_value) ~ log(p_sow) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm((def_value) ~ log(p_grow) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm((def_value) ~ log(p_harv) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm((def_value) ~ log(p_sup) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm((def_value) ~ log(p_onemonth) | market + yrmnth, data=data))
    } else {
      switch(var, 
             "p_sow" = mod_data <- felm((def_value) ~ poly(p_sow,level,raw=T) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm((def_value) ~ poly(p_grow,level,raw=T) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm((def_value) ~ poly(p_harv,level,raw=T) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm((def_value) ~ poly(p_sup,level,raw=T) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm((def_value) ~ poly(p_onemonth,level,raw=T) | market + yrmnth, data=data))
    }
    
    return(mod_data)
    
  }
  
  ## regress data with fixed effects
  l_regress <- function(data, var, level) {
    
    if(level == 1) {
      switch(var, 
             "p_sow" = mod_data <- lm(log(def_value) ~ p_sow + as.factor(market) + as.factor(yrmnth), data=data),
             "p_grow" = mod_data <- lm(log(def_value) ~ p_grow + as.factor(market) + as.factor(yrmnth), data=data),
             "p_harv" = mod_data <- lm(log(def_value) ~ p_harv + as.factor(market) + as.factor(yrmnth), data=data),
             "p_sup" = mod_data <- lm(log(def_value) ~ p_sup + as.factor(market) + as.factor(yrmnth), data=data),
             "p_onemonth" = mod_data <- lm(log(def_value) ~ p_onemonth + as.factor(market) + as.factor(yrmnth), data=data))
      
    } else if(level == "log") {
      switch(var, 
             "p_sow" = mod_data <- lm(log(def_value) ~ log(p_sow) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_grow" = mod_data <- lm(log(def_value) ~ log(p_grow) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_harv" = mod_data <- lm(log(def_value) ~ log(p_harv) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_sup" = mod_data <- lm(log(def_value) ~ log(p_sup) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_onemonth" = mod_data <- lm(log(def_value) ~ log(p_onemonth) + as.factor(market) + as.factor(yrmnth), data=data))
    } else {
      switch(var, 
             "p_sow" = mod_data <- lm(log(def_value) ~ poly(p_sow,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_grow" = mod_data <- lm(log(def_value) ~ poly(p_grow,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_harv" = mod_data <- lm(log(def_value) ~ poly(p_harv,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_sup" = mod_data <- lm(log(def_value) ~ poly(p_sup,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data),
             "p_onemonth" = mod_data <- lm(log(def_value) ~ poly(p_onemonth,level,raw=T) + as.factor(market) + as.factor(yrmnth), data=data))
    }
    
    return(mod_data)
    
  }
  
  ## regress using felm
  l_regress_felm <- function(data, var, level) {
    
    if(level == 1) {
      switch(var, 
             "p_sow" = mod_data <- felm(log(def_value) ~ p_sow | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm(log(def_value) ~ p_grow | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm(log(def_value) ~ p_harv | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm(log(def_value) ~ p_sup | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm(log(def_value) ~ p_onemonth | market + yrmnth, data=data))
      
    } else if(level == "log") {
      switch(var, 
             "p_sow" = mod_data <- felm(log(def_value) ~ log(p_sow) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm(log(def_value) ~ log(p_grow) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm(log(def_value) ~ log(p_harv) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm(log(def_value) ~ log(p_sup) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm(log(def_value) ~ log(p_onemonth) | market + yrmnth, data=data))
    } else {
      switch(var, 
             "p_sow" = mod_data <- felm(log(def_value) ~ poly(p_sow,level,raw=T) | market + yrmnth, data=data),
             "p_grow" = mod_data <- felm(log(def_value) ~ poly(p_grow,level,raw=T) | market + yrmnth, data=data),
             "p_harv" = mod_data <- felm(log(def_value) ~ poly(p_harv,level,raw=T) | market + yrmnth, data=data),
             "p_sup" = mod_data <- felm(log(def_value) ~ poly(p_sup,level,raw=T) | market + yrmnth, data=data),
             "p_onemonth" = mod_data <- felm(log(def_value) ~ poly(p_onemonth,level,raw=T) | market + yrmnth, data=data))
    }
    
    return(mod_data)
    
  }
  
  ## run linear, quad, and 3rd regression and make a table
  commods <- list(sorghum, millet, maize)
  types <- c("p_sow", "p_grow", "p_harv", "p_sup", "p_onemonth")
  final <- c()
  
  for(com in commods) {
    for(t in types) {
      
      #run the three regressions
      linear <- regress(com, t, 1)
      quad <- regress(com, t, 2)
      third <- regress(com, t, 3)
      log <- regress(com, t, "log")
      
      #run logs
      llinear <- l_regress(com, t, 1)
      lquad <- l_regress(com, t, 2)
      lthird <- l_regress(com, t, 3)
      llog <- l_regress(com, t, "log")
      
      #pvals? 
      #run the three regressions
      flinear <- regress_felm(com, t, 1)
      fquad <- regress_felm(com, t, 2)
      fthird <- regress_felm(com, t, 3)
      flog <- regress_felm(com, t, "log")
      
      #run logs
      fllinear <- l_regress_felm(com, t, 1)
      flquad <- l_regress_felm(com, t, 2)
      flthird <- l_regress_felm(com, t, 3)
      fllog <- l_regress_felm(com, t, "log")
      
      p_linear <- all(summary(flinear)$coefficients[,4] < 0.05)
      p_quad <- all(summary(fquad)$coefficients[,4] < 0.05)
      p_third <- all(summary(fthird)$coefficients[,4] < 0.05)
      p_log <- all(summary(flog)$coefficients[,4] < 0.05)
      p_llinear <- all(summary(fllinear)$coefficients[,4] < 0.05)
      p_lquad <- all(summary(flquad)$coefficients[,4] < 0.05)
      p_lthird <- all(summary(flthird)$coefficients[,4] < 0.05)
      p_llog <- all(summary(fllog)$coefficients[,4] < 0.05)
      
      gr <- as.character(com$type[1])
      outcomes <- cbind(gr, t, AIC(linear), p_linear, AIC(quad), p_quad, AIC(third), p_third, AIC(log), p_log,
                        AIC(llinear), p_llinear, AIC(lquad), p_lquad, AIC(lthird), p_lthird, AIC(llog), p_llog)
      final <- rbind(final, outcomes)
      
    }
  }
  
  colnames(final) <- c("Grain", "Time Period", "Linear_AIC", "Linear_Pvals", "2_AIC", "2_Pvals", "3_AIC", "3_Pvals", "Log_AIC", "Log_Pvals",
                       "Log_Linear_AIC", "Log_Linear_Pvals", "Log_2_AIC", "Log_2_Pvals", "Log_3_AIC", "Log_3_Pvals", "Log_Log_AIC", "Log_Log_Pvals")
  final <- as.data.frame(final)
  final <- cbind(final, rep(buf, nrow(final)))
  all_final <- rbind(all_final, final)
}















final[,3:5] <- lapply(final[,3:5], as.character)
final[,3:5] <- lapply(final[,3:5], as.numeric)
difs <- cbind(final[,1:2], rep(buf, nrow(final)), na.omit(final[,3]/final[,4]), na.omit(final[,4]/final[,5]))

#estimates
lin_estimates <- c()
quad_estimates <- c()
cube_estimates <- c()
for(com in commods) {
  for(t in types) {
    
    #run regressions
    linear <- summary(regress_felm(com, t, 1))$coefficients
    quad <- summary(regress_felm(com, t, 2))$coefficients
    third <- summary(regress_felm(com, t, 3))$coefficients
    
    gr <- as.character(com$type[1])
    
    lin_estimates <- rbind(lin_estimates, cbind(gr, linear))
    quad_estimates <- rbind(quad_estimates, cbind(rep(gr, 2), quad))
    cube_estimates <- rbind(cube_estimates, cbind(rep(gr, 3), third))
  }
}

# panel regression
mod_sorghum_sow <- linear_regression_fe(sorghum, "p_sow")
mod_sorghum_grow <- quad_regression(sorghum, "p_grow")
mod_sorghum_harv <- linear_regression(sorghum, "p_harv")
mod_sorghum_sup <- linear_regression(sorghum, "p_sup")
mod_sorghum_onem <- quad_regression(sorghum, "p_onemonth")
