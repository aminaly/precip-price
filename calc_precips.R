ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
	setwd("/Users/amina/Documents/Stanford/precip-price"),
	setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
price <- readRDS("saved-output/formatted-price.rds")

## Script to run to do a bunch of calculations 
bufs <- c(3, 4, 5)

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
b <- as.numeric(args[1])
buf <- bufs[b]

daily <- c(TRUE, TRUE, FALSE, FALSE)
zeros <- c(TRUE, FALSE, TRUE, FALSE)

for(run in 1:4) {
  
  #daily without zeros
  rdsname <- paste0("precip/", buf, "_precip.rds")
  precip <- readRDS(rdsname)
  source(paste0(getwd(), "/functions_for_analysis.R"))
  run_daily <- daily[run]
  include_zeros <- zeros[run]
  
  #get the right pp_Data
  pp_data <- clean_precip(precip)
  get_grain_data(pp_data, "sorghum", run_daily, include_zeros)
  get_grain_data(pp_data, "millet", run_daily, include_zeros)
  get_grain_data(pp_data, "maize", run_daily, include_zeros)
  
}
