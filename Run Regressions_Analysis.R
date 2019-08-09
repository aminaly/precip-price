# Let's get hte price data
price <- readRDS("saved-output/formatted-price.rds")
#If you don't need to extract, just use this to get pp_data together. If you do, you'll need to run precip_extract
buf <- 2
rdsname <- paste0("precip/", buf, "_precip.rds")
precip <- readRDS(rdsname)

source('~/Documents/Stanford/precip-price/Functions for Analysis.R')

## SORGHUM
#lets get the sorghum we want specificially & merge in seasonality
sorghum <- filter_grain(pp_data, "Sorghum")
sorghum <- merge_seasons(sorghum, "sorghum")

#get locations with the right set of dates
sorghum <- filter_dates(sorghum, "Sorghum")

#calcualte precip for correct set of dates
#doing this first because otherwise we end up with some zeros at the end
sorghum <- calc_precip(sorghum)

# panel regression
mod_sorghum_sow <- get_model_regression(sorghum, "p_sow", 1)
mod_sorghum_grow <- get_model_regression(sorghum, "p_grow", 2)
mod_sorghum_harv <- get_model_regression(sorghum, "p_harv", 1)
mod_sorghum_sup <- get_model_regression(sorghum, "p_sup", 1)
mod_sorghum_onem <- get_model_regression(sorghum, "p_onemonth", 2)

#bootstrap all three and return information for plotting
sorghum_sow_boot <- bootstrap_data(sorghum, mod_sorghum_sow, "p_sow", short = T)
sorghum_grow_boot <- bootstrap_data(sorghum, mod_sorghum_grow, "p_grow", short = T, level = 2)
sorghum_harv_boot <- bootstrap_data(sorghum, mod_sorghum_harv, "p_harv", short = T)
sorghum_sup_boot <- bootstrap_data(sorghum, mod_sorghum_sup, "p_sup",  short = T)
sorghum_onem_boot <- bootstrap_data(sorghum, mod_sorghum_onem, "p_onemonth", short = T, level = 2)

## leaving this here just for personal testing.
##This isn't output anywhere permanantly. change at will


## Millet
#lets get the millet we want specificially & merge in seasonality
millet <- filter_grain(pp_data, "Millet")
millet <- merge_seasons(millet, "millet")

#get locations with the right set of dates
millet <- filter_dates(millet, "Millet")

#calcualte precip for correct set of dates
#doing this first because otherwise we end up with some zeros at the end
millet <- calc_precip(millet)

# panel regression
mod_millet_sow <- get_model_regression(millet, "p_sow", 2)
mod_millet_grow <- get_model_regression(millet, "p_grow", 2)
mod_millet_harv <- get_model_regression(millet, "p_harv", 1)
mod_millet_sup <- get_model_regression(millet, "p_sup", 2)
mod_millet_onem <- get_model_regression(millet, "p_onemonth", 2)

#bootstrap all three and return information for plotting
millet_sow_boot <- bootstrap_data(millet, mod_millet_sow, "p_sow", short = T, level = 2)
millet_grow_boot <- bootstrap_data(millet, mod_millet_grow, "p_grow", short = T, level = 2)
millet_harv_boot <- bootstrap_data(millet, mod_millet_harv, "p_harv", short = T)
millet_sup_boot <- bootstrap_data(millet, mod_millet_sup, "p_sup", short = T, level = 2)
millet_onem_boot <- bootstrap_data(millet, mod_millet_onem, "p_onemonth", short = T, level = 2)

## Maize
#lets get the maize we want specificially & merge in seasonality
maize <- filter_grain(pp_data, "Maize")
maize <- merge_seasons(maize, "maize")

#get locations with the right set of dates
maize <- filter_dates(maize, "Maize")

#calcualte precip for correct set of dates
#doing this first because otherwise we end up with some zeros at the end
maize <- calc_precip(maize)

# panel regression
mod_maize_sow <- get_model_regression(maize, "p_sow", 1)
mod_maize_grow <- get_model_regression(maize, "p_grow", 1)
mod_maize_harv <- get_model_regression(maize, "p_harv", 2)
mod_maize_sup <- get_model_regression(maize, "p_sup", 1)
mod_maize_onem <- get_model_regression(maize, "p_onemonth", 2)

#bootstrap all three and return information for plotting
maize_sow_boot <- bootstrap_data(maize, mod_maize_sow, "p_sow", short = T)
maize_grow_boot <- bootstrap_data(maize, mod_maize_grow, "p_grow", short = T)
maize_harv_boot <- bootstrap_data(maize, mod_maize_harv, "p_harv", short = T, level = 2)
maize_sup_boot <- bootstrap_data(maize, mod_maize_sup, "p_sup", short = T)
maize_onem_boot <- bootstrap_data(maize, mod_maize_onem, "p_onemonth", short = T, level = 2)

