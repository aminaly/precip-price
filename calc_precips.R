## Script to run to do a bunch of calculations 
bufs <- c(.25, 1, 2)

for(i in bufs) {
  
  buf <- bufs[i]
  
  #daily without zeros
  rdsname <- paste0("precip/", buf, "_precip.rds")
  precip <- readRDS(rdsname)
  precipname <- paste0("precip/", buf, "_ppdata.csv")
  run_daily <- TRUE
  include_zeros <- FALSE
  
  ## Get data for each commodity
  #lets get the sorghum we want specificially & merge in seasonality
  sorghum <- filter_grain(pp_data, "Sorghum")
  sorghum <- merge_seasons(sorghum, "sorghum")
  #get locations with the right set of dates
  sorghum <- filter_dates(sorghum, "Sorghum")
  #calcualte precip for correct set of dates
  sorghum <- calc_precip(sorghum, daily = run_daily, zeros = include_zeros)
  
  #lets get the millet we want specificially & merge in seasonality
  millet <- filter_grain(pp_data, "Millet")
  millet <- merge_seasons(millet, "millet")
  #get locations with the right set of dates
  millet <- filter_dates(millet, "Millet")
  #calcualte precip for correct set of dates
  millet <- calc_precip(millet, daily = run_daily, zeros = include_zeros)
  
  #lets get the maize we want specificially & merge in seasonality
  maize <- filter_grain(pp_data, "Maize")
  maize <- merge_seasons(maize, "maize")
  #get locations with the right set of dates
  maize <- filter_dates(maize, "Maize")
  #calcualte precip for correct set of dates
  maize <- calc_precip(maize, daily = run_daily, zeros = include_zeros)
  
  #total accumulated without zeros
  run_daily <- FALSE
  include_zeros <- FALSE
  
  ## Get data for each commodity
  #lets get the sorghum we want specificially & merge in seasonality
  sorghum <- filter_grain(pp_data, "Sorghum")
  sorghum <- merge_seasons(sorghum, "sorghum")
  #get locations with the right set of dates
  sorghum <- filter_dates(sorghum, "Sorghum")
  #calcualte precip for correct set of dates
  sorghum <- calc_precip(sorghum, daily = run_daily, zeros = include_zeros)
  
  #lets get the millet we want specificially & merge in seasonality
  millet <- filter_grain(pp_data, "Millet")
  millet <- merge_seasons(millet, "millet")
  #get locations with the right set of dates
  millet <- filter_dates(millet, "Millet")
  #calcualte precip for correct set of dates
  millet <- calc_precip(millet, daily = run_daily, zeros = include_zeros)
  
  #lets get the maize we want specificially & merge in seasonality
  maize <- filter_grain(pp_data, "Maize")
  maize <- merge_seasons(maize, "maize")
  #get locations with the right set of dates
  maize <- filter_dates(maize, "Maize")
  #calcualte precip for correct set of dates
  maize <- calc_precip(maize, daily = run_daily, zeros = include_zeros)
  
  #daily with zeros
  run_daily <- TRUE
  include_zeros <- TRUE
  
  ## Get data for each commodity
  #lets get the sorghum we want specificially & merge in seasonality
  sorghum <- filter_grain(pp_data, "Sorghum")
  sorghum <- merge_seasons(sorghum, "sorghum")
  #get locations with the right set of dates
  sorghum <- filter_dates(sorghum, "Sorghum")
  #calcualte precip for correct set of dates
  sorghum <- calc_precip(sorghum, daily = run_daily, zeros = include_zeros)
  
  #lets get the millet we want specificially & merge in seasonality
  millet <- filter_grain(pp_data, "Millet")
  millet <- merge_seasons(millet, "millet")
  #get locations with the right set of dates
  millet <- filter_dates(millet, "Millet")
  #calcualte precip for correct set of dates
  millet <- calc_precip(millet, daily = run_daily, zeros = include_zeros)
  
  #lets get the maize we want specificially & merge in seasonality
  maize <- filter_grain(pp_data, "Maize")
  maize <- merge_seasons(maize, "maize")
  #get locations with the right set of dates
  maize <- filter_dates(maize, "Maize")
  #calcualte precip for correct set of dates
  maize <- calc_precip(maize, daily = run_daily, zeros = include_zeros)
  
  #accumulated with zeros
  run_daily <- TRUE
  include_zeros <- TRUE
  
  ## Get data for each commodity
  #lets get the sorghum we want specificially & merge in seasonality
  sorghum <- filter_grain(pp_data, "Sorghum")
  sorghum <- merge_seasons(sorghum, "sorghum")
  #get locations with the right set of dates
  sorghum <- filter_dates(sorghum, "Sorghum")
  #calcualte precip for correct set of dates
  sorghum <- calc_precip(sorghum, daily = run_daily, zeros = include_zeros)
  
  #lets get the millet we want specificially & merge in seasonality
  millet <- filter_grain(pp_data, "Millet")
  millet <- merge_seasons(millet, "millet")
  #get locations with the right set of dates
  millet <- filter_dates(millet, "Millet")
  #calcualte precip for correct set of dates
  millet <- calc_precip(millet, daily = run_daily, zeros = include_zeros)
  
  #lets get the maize we want specificially & merge in seasonality
  maize <- filter_grain(pp_data, "Maize")
  maize <- merge_seasons(maize, "maize")
  #get locations with the right set of dates
  maize <- filter_dates(maize, "Maize")
  #calcualte precip for correct set of dates
  maize <- calc_precip(maize, daily = run_daily, zeros = include_zeros)
  
  
}
