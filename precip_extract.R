library(sf)
library(raster)
library(velox)
library(mapview)
library(lfe)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)
b <- as.numeric(args[1])
bufs <- c(.25, .5, .75, 1, 2, 3, 4, 5)
buf <- bufs[b]

setwd("/Users/amina/Documents/Stanford/precip-price/")

if(file.exists("saved-output/precipitation.rds")) {
  precipitation <- readRDS("saved-output/precipitation.rds")
} else {
  setwd("/Users/amina/Desktop/large_precip_price")
  precipitation <- brick("chirps-v2-monthly.nc") %>% crop(c(-25.35, 51.41, -46.97, 37.34)  #extent pulled by reading contents of african shapefile. Throws error otherwise due to type
  setwd("/Users/amina/Documents/Stanford/precip-price/")
}

## loop through every layer, for each unique location label and find precip, and add to table 
locs <- unique(price[,c(1:2,16:17)])
locs <- locs[which(locs$latitude < 999),]

# Transform your markets
markets = st_as_sf(locs,coords=c("longitude","latitude"))
st_crs(markets) <- 4326
markets <- st_transform(markets, 4326)

for(buf in bufs) {
  
  #check to see if we've extracted before
  rdsname <- paste0("precip/", buf, "_precip.rds")
  #if(file.exists(paste0(getwd(), rdsname))) stop("Already exists")
  
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
  saveRDS(all_data, paste0(getwd(), "/", rdsname))
  precip <- readRDS(rdsname)
  
}
