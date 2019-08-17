##Use this file to run and save bootstraps (for use in Sherlock)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
buf <- 1

rdsname <- paste0("precip/", buf, "_precip.rds")
precip <- readRDS(rdsname)
precipname <- paste0("precip/", buf, "_ppdata.csv")
source(paste0(getwd(), "/functions_for_analysis.R"))
run_daily <- FALSE
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


## mins and maxs
ymaxs <- c(1, .5, 1.2, 1, .2)
ymins <- c(-1, -.5, -1, -1, -.2)
ys <- cbind(rep("sorghum", 5), ymaxs, ymins)
ymaxs <- c(.5, .5, 1, 3, .5)
ymins <- c(-.5, -.5, -1, -12, -.5)
ys <- rbind(ys, cbind(rep("millet", 5), ymaxs, ymins))
ymaxs <- c(.5, 1, 1, .2, .2)
ymins <- c(-1, -.5, -1, -.2, -.2)
ys <- rbind(ys, cbind(rep("maize", 5), ymaxs, ymins))


par(mfrow=c(3,2))
comps <- read.csv("model-comps/Master_Model_Comps.csv", stringsAsFactors = F)
types <- c("p_sow", "p_grow", "p_harv", "p_sup", "p_onemonth")

comps_plot <- comps %>% filter(buffer == buf) %>% filter(daily == run_daily) %>% filter(zeros == include_zeros)

for(i in 1:nrow(comps_plot)) {
  
  #select the current row, and make sure it's usable
  row <- comps_plot[i,]
  title <- paste(row$grain, row$time_period, "avg=", run_daily, "zeros=", include_zeros , "buf=", buf)
  
  #If this is the first of a new section, plot just simple title before continuing
  if((i %% 5) == 1) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(row$grain), cex = 1.6, col = "black")
  }
  
  #get the right data and level
  data <- switch(row$grain,
                 "sorghum" = sorghum,
                 "maize" = maize,
                 "millet" = millet)
  level <- switch(as.character(row$model_choice),
                  "Log_Linear" = 1,
                  "Log_2" = 2,
                  "Log_3" = 3,
                  "Log_Log" = "log")
  
  #if there is no level, plot title and move on
  if(is.na(row$model_choice)) {
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(title, "No Ideal Model"), cex = 1.6, col = "black")
    next
  }
  
  #get model
  mod <- get_model_regression(data, row$time_period, level, log=TRUE)
  
  #create bootstrap name based on grain, buffer, time period, avg/accumulated, and zeroes/nozeros
  bootname <- paste0(data$type[1], "_", buf, "_", row$time_period, "_")
  bootname <- ifelse(run_daily, paste0(bootname, "average_"), paste0(bootname, "accumulated_"))
  bootname <- ifelse(include_zeros, paste0(bootname, "zeros.rds"), paste0(bootname, "nozeros.rds"))
  boots <- bootstrap_data(data, mod, row$time_period, xrange=0:400, level=level, log=TRUE, short = F, name = bootname)
  
}

