##Use this file to run and save bootstraps (for use in Sherlock)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
b <- as.numeric(args[1])
bufs <- c(3, 4, 5)
buf <- bufs[b]

rdsname <- paste0("precip/", buf, "_precip.rds")
precip <- readRDS(rdsname)
source(paste0(getwd(), "/functions_for_analysis.R"))
daily <- c(TRUE, TRUE, FALSE, FALSE)
zeros <- c(TRUE, FALSE, TRUE, FALSE)

for(run in 1:4) {
  
  run_daily <- daily[run]
  include_zeros <- zeros[run]
  
  ## Get data for each commodity
  #lets get the data we want specificially & merge in seasonality
  pp_data <- clean_precip(precip)
  sorghum <- get_grain_data(pp_data, "sorghum", run_daily, include_zeros)
  millet <- get_grain_data(pp_data, "millet", run_daily, include_zeros)
  maize <- get_grain_data(pp_data, "maize", run_daily, include_zeros)
  
  comps <- read.csv("model-comps/Master_Model_Comps.csv", stringsAsFactors = F)
  
  comps_plot <- comps %>% filter(buffer == buf) %>% filter(daily == run_daily) %>% filter(zeros == include_zeros)
  
  for(i in 1:nrow(comps_plot)) {
    
    #select the current row, and make sure it's usable
    row <- comps_plot[i,]

    if(row$model_choice == "") next
    
    #get the right data and level
    data <- switch(row$grain,
                   "sorghum" = sorghum,
                   "maize" = maize,
                   "millet" = millet)
    level <- switch(as.character(row$model_choice),
                    "log_linear" = 1,
                    "log_2" = 2,
                    "log_3" = 3,
                    "log_log" = "log")
    
    #get model
    mod <- get_model_regression(data, row$time_period, level, log=TRUE)
    
    #create bootstrap name based on grain, buffer, time period, avg/accumulated, and zeroes/nozeros
    bootname <- get_bootname(data$type[1], buf, row$time_period, run_daily, include_zeros)
    boots <- bootstrap_data(data, mod, row$time_period, level=level, log=TRUE, short = F, name = bootname)
    
  }
  
}

