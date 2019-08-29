## General summary statistics

# summary of price data
sorghum <- filter_grain(price, "Sorghum")
millet <- filter_grain(price, "Millet")
maize <- filter_grain(price, "Maize")

all <- rbind(maize, rbind(sorghum, millet))
all$year <- format(all$period_date, na.rm=T, "%Y")

write.table(all, "shared/filter_price.csv", row.names = FALSE, sep = ",", dec = ".")

data_by_market <- all %>% group_by(country, type) %>% count(market)


breakdown <- price %>% group_by(location, product) %>% summarize(avg_value = mean(def_value))

## price per
price_per_year <- all %>% group_by(country, type, year) %>%
  summarize(avg_value = mean(def_value, na.rm = T)) 

comb <- c()

for (c in c("Chad", "Mauritania", "Nigeria")) {
  for (t in c("sorghum", 'millet', 'maize')) {
    
    use <- all %>% filter(country == c) %>% filter(type == t) %>%
      dplyr::select(c('country', 'def_value', "period_date"))
    hist(use$def_value, main = paste(c, ":", t, "SD =", round(sd(use$def_value, na.rm = T), 3)), xlab = "Deflated Value")
    
  }
}
par(mfrow=(c(3,3)))
c = "Nigeria"
t = "sorghum"
use <- all %>% filter(country == c) %>% filter(type == t) %>%
  dplyr::select(c('country', 'def_value', "period_date"))
hist(use$def_value, main = paste(c, ":", t, "SD =", round(sd(use$def_value, na.rm = T), 3)), xlab = "Deflated Value")

countries <- c("Chad", "Mauritania", "Nigeria")
commodities <- c("sorghum", 'millet', 'maize')

pe <- pr %>% filter(country %in% countries) %>% group_by(country, grain) %>% summarise(c(avg_rain = mean(avg_rainfall, na.rm = T)),
                                                                                      avg_sowing = mean(p_sow, na.rm = T),
                                                                                      avg_growing = mean(p_grow, na.rm = T),
                                                                                      avg_harvest = mean(p_harv, na.rm = T), 
                                                                                      avg_supplychain = mean(p_sup, na.rm = T),
                                                                                      avg_onemnth = mean(p_onemonth, na.rm = T))

mkts <- pr %>% filter(country %in% countries) %>% group_by(country, market, grain) %>% dplyr::select(c('country', 'market', "grain"))
markets <- mkts %>% filter(country %in% countries) %>% summarise(grains = unqiue(mkts %>% filter))


par(mfrow=(c(3,3)))

for (c in c("Chad", "Mauritania", "Nigeria")) {
  for (t in c("sorghum", 'millet', 'maize')) {
    
    use <- pr %>% filter(country == c) %>% filter(grain == t) 
    hist(as.numeric(use$avg_rainfall), main = paste(c, ":", t, "SD =", round(sd(use$avg_rainfall, na.rm = T), 3)), xlab = "Rainfall")
    
  }
}
