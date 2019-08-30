### Running some basic code to decide on ideal model for each commodity

# Source files with functions useful/data prep
setwd("/Users/amina/Documents/Stanford/precip-price")
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
args <- commandArgs(trailingOnly = TRUE)
b <- as.numeric(args[1])
bufs <- c(3, 4, 5)
buf <- bufs[b]

# Arguments to change depending on what you want to run
daily <- c(TRUE, TRUE, FALSE, FALSE)
zeros <- c(TRUE, FALSE, TRUE, FALSE)

#If you don't need to extract, just use this to get pp_data together. If you do, you'll need to run precip_extract
for(run in 1:4) {
  
  all_final <- c()
  include_zeros <- zeros[run]
  run_daily <- daily[run]
  filename <- "model-comps/model_selection"
  filename <- ifelse(run_daily, paste0(filename, "_average"), paste0(filename, "_accumulated"))
  filename <- ifelse(include_zeros, paste0(filename, "_zeros.csv"), paste0(filename, "_no_zeros.csv"))
  if(file.exists(paste0(getwd(), "/", filename))) next
  
  for(buf in bufs) {
  
    rdsname <- paste0("precip/", buf, "_precip.rds")
    precip <- readRDS(rdsname)
    source('~/Documents/Stanford/precip-price/functions_for_analysis.R')

    #get the right pp_Data
    pp_data <- clean_precip(precip)
    sorghum <- get_grain_data(pp_data, "sorghum", run_daily, include_zeros)
    millet <- get_grain_data(pp_data, "millet", run_daily, include_zeros)
    maize <- get_grain_data(pp_data, "maize", run_daily, include_zeros)
    
    ## run linear, quad, and 3rd regression and make a table
    commods <- list(sorghum, millet, maize)
    types <- c("p_sow", "p_grow", "p_harv", "p_sup", "p_onemonth")
    final <- c()
    
     for(com in commods) {
      for(t in types) {
        #run logs
        llinear <- l_regress(com, t, 1)
        lquad <- l_regress(com, t, 2)
        lthird <- l_regress(com, t, 3)
        llog <- l_regress(com, t, "log")
        
        #run logs
        fllinear <- l_regress_felm(com, t, 1)
        flquad <- l_regress_felm(com, t, 2)
        flthird <- l_regress_felm(com, t, 3)
        fllog <- l_regress_felm(com, t, "log")
        
        p_llinear <- all(summary(fllinear)$coefficients[,4] < 0.05)
        p_lquad <- all(summary(flquad)$coefficients[,4] < 0.05)
        p_lthird <- all(summary(flthird)$coefficients[,4] < 0.05)
        p_llog <- all(summary(fllog)$coefficients[,4] < 0.05)
        
        gr <- as.character(com$type[1])
        outcomes <- cbind(gr, t, AIC(llinear), p_llinear, 
                          AIC(lquad), p_lquad, AIC(lthird), 
                          p_lthird, AIC(llog), p_llog)
        final <- rbind(final, outcomes)
        
      }
    }
    
    colnames(final) <- c("grain", "time_period", "log_linear", "ll_pvals", "log_2", "l2_pvals",
                         "log_3", "l3_pvals", "log_log", "lg_pvals")
    final <- as.data.frame(final)
    final <- cbind(final, rep(buf, nrow(final)))
    all_final <- rbind(all_final, final)
  }
  
  write.csv(all_final, filename)
  
}





