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


summary(use)