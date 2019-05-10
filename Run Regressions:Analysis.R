source('~/Documents/Stanford/R - Spring 2019/New Regressions.R')

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
mod_sorghum_sow <- linear_regression(sorghum, "p_sow")
mod_sorghum_grow <- quad_regression(sorghum, "p_grow")
mod_sorghum_harv <- linear_regression(sorghum, "p_harv")
mod_sorghum_sup <- quad_regression(sorghum, "p_sup")
mod_sorghum_onem <- quad_regression(sorghum, "p_onemonth")

#bootstrap all three and return information for plotting
sorghum_sow_boot <- bootstrap_data(sorghum, mod_sorghum_sow, "p_sow")
sorghum_grow_boot <- bootstrap_data(sorghum, mod_sorghum_grow, "p_grow")
sorghum_harv_boot <- bootstrap_data(sorghum, mod_sorghum_harv, "p_harv")
sorghum_sup_boot <- bootstrap_data(sorghum, mod_sorghum_sup, "p_sup")
sorghum_onem_boot <- bootstrap_data(sorghum, mod_sorghum_onem, "p_onemonth", short = T)

## leaving this here just for personal testing.
##This isn't output anywhere permanantly. change at will
x <- sorghum_onem_boot[[2]]
yy <- sorghum_onem_boot[[3]]
coef <- sorghum_onem_boot[[1]]

plot(100,xlim=c(0,400),ylim=c(-.1,.5),las=1,xlab="precip",ylab="value")  
for (i in 1:100) {
  yy <- x*coef[i] + x^2*coef[i,2]  
  #yy <- yy - yy[x=80]
  lines(x,yy,lwd=0.5)
}

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
mod_millet_sow <- quad_regression(millet, "p_sow")
mod_millet_grow <- quad_regression(millet, "p_grow")
mod_millet_harv <- quad_regression(millet, "p_harv")
mod_millet_sup <- quad_regression(millet, "p_sup")


#bootstrap all three and return information for plotting
millet_sow_boot <- bootstrap_data(millet, mod_millet_sow, "p_sow")
millet_grow_boot <- bootstrap_data(millet, mod_millet_grow, "p_grow")
millet_harv_boot <- bootstrap_data_lin(millet, mod_millet_harv, "p_harv", short = T)
millet_sup_boot <- bootstrap_data_lin(millet, mod_millet_harv, "p_sup", short = T)

#plot 
x <- millet_sup_boot[[2]]
yy <- millet_sup_boot[[3]]
coef <- millet_sup_boot[[1]]

plot(50,xlim=c(0,1000),ylim=c(-.5,.5),las=1,xlab="precip",ylab="value")  
for (i in 1:100) {
  yy <- x*coef[i]# + x^2*coef[i,2]  
  yy <- yy - yy[x=20]
  lines(x,yy,lwd=0.5)
}

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
mod_maize_sow <- quad_regression(maize, "p_sow")
mod_maize_grow <- linear_regression(maize, "p_grow")
mod_maize_harv <- quad_regression(maize, "p_harv")
mod_maize_sup <- linear_regression(maize, "p_sup")

#bootstrap all three and return information for plotting
maize_sow_boot <- bootstrap_data(maize, mod_maize_sow, "p_sow")
maize_grow_boot <- bootstrap_data_lin(maize, mod_maize_grow, "p_grow", short = T)
maize_harv_boot <- bootstrap_data(maize, mod_maize_harv, "p_harv", short = T)
maize_sup_boot <- bootstrap_data_lin(maize, mod_maize_sup, "p_sup", short = T)

#plot 
x <- maize_sup_boot[[2]]
yy <- maize_sup_boot[[3]]
coef <- maize_sup_boot[[1]]

plot(50,xlim=c(0,700),ylim=c(-.5,.5),las=1,xlab="precip",ylab="value")  
for (i in 1:100) {
  yy <- x*coef[i]# + x^2*coef[i,2]  
  yy <- yy - yy[x=20]
  lines(x,yy,lwd=0.5)
}


