library(sf)
library(raster)
library(velox)
library(mapview)
library(lfe)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)
b <- as.numeric(args[1])
bufs <- c(3, 4, 5)
buf <- bufs[b]

#setwd("~/scratch/groups/omramom/CHRIPS_monthly/")
#set wd to where the chirps file is first
ifelse(dir.exists("/Users/amina/Desktop/large_precip_price"),
       setwd("/Users/amina/Desktop/large_precip_price"),
       setwd("/oak/stanford/groups/omramom/datasets/CHIRPS/monthly"))

precipitation <- brick("chirps-v2-monthly.nc", band = 2) %>% crop(c(-25.35, 51.41, -46.97, 37.34))  #extent pulled by reading contents of african shapefile. Throws error otherwise due to type


ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))

## loop through every layer, for each unique location label and find precip, and add to table 
locs <- unique(price[,c(1:2,16:17)])
locs <- locs[which(locs$latitude < 999),]

# Transform your markets
markets = st_as_sf(locs,coords=c("longitude","latitude"))
st_crs(markets) <- 3857
markets <- st_transform(markets, 3857)

# transform Africa if you wanna
st_crs(africa) <- 3857
africa <- st_transform(africa,  3857)

# set up the buffers for your markets
markets_buffer = st_buffer(markets, buf)
markets_buffer <- as(markets_buffer, 'Spatial')

# if you want to map over africa, use this 
#markets_africa = st_intersection(markets_buffer, africa_km)

# Run through precipitation brick and extract over the buffers
i <- 1
all_data <- c()
while(i < 446) {
  print(i)
  temp <- c()
  
  temp$date <- rep(substring(names(precipitation[[i]]), 2), nrow(locs))
  temp$location <- locs$market

  velox_obj <- velox(precipitation[[i]])
  temp_pVals <- velox_obj$extract(sp = markets_buffer, small = T)

  temp$temp_mean <- lapply(temp_pVals, function(x){mean(x, na.rm = T)}) %>% unlist()  
  
  all_data <- bind_rows(all_data, temp)
  i <- i+1
}

#save this out to make my life easier
rdsname <- paste0("precip/", buf, "_precip.rds")
saveRDS(all_data, rdsname)
precip <- readRDS(rdsname)


#use only markets that have the same set of years