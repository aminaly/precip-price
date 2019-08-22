##Use this file to run and save bootstraps (for use in Sherlock)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
b <- as.numeric(args[1])
bufs <- c(0.25, 1, 2)
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
  sorghum <- get_grain_data(pp_data, "sorghum", run_daily, include_zeros)
  millet <- get_grain_data(pp_data, "millet", run_daily, include_zeros)
  maize <- get_grain_data(pp_data, "maize", run_daily, include_zeros)
  
  comps <- read.csv("model-comps/Master_Model_Comps.csv", stringsAsFactors = F)
  
  comps_plot <- comps %>% filter(buffer == buf) %>% filter(daily == run_daily) %>% filter(zeros == include_zeros)
  
  for(i in 1:nrow(comps_plot)) {
    
    #select the current row, and make sure it's usable
    row <- comps_plot[i,]
    title <- paste(row$grain, row$time_period, "avg=", run_daily, "zeros=", include_zeros , "buf=", buf)
    
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
    
    #if there is no level, plot title and move on
    if(is.na(row$model_choice)) {
      
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste(title, "No Ideal Model"), cex = 1.6, col = "black")
      next
    }
    
    #get model
    mod <- get_model_regression(data, row$time_period, level, log=TRUE)
    
    #create bootstrap name based on grain, buffer, time period, avg/accumulated, and zeroes/nozeros
    xrng <- ifelse(run_daily, 0:400, 0:2000)
    bootname <- paste0(data$type[1], "_", buf, "_", row$time_period, "_")
    bootname <- ifelse(run_daily, paste0(bootname, "average_"), paste0(bootname, "accumulated_"))
    bootname <- ifelse(include_zeros, paste0(bootname, "zeros.rds"), paste0(bootname, "nozeros.rds"))
    boots <- bootstrap_data(data, mod, row$time_period, xrange=xrng, level=level, log=TRUE, short = F, name = bootname)
    
  }
  
}

