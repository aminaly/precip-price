library(sf)
library(raster)
library(velox)
library(mapview)
library(lfe)
library(lubridate)
library(maptools)
source('~/Documents/Stanford/precip-price/format_price.R')
buf <- 2

# read in africa shapefile (if you want to)
#setwd("~/scratch/users/aminaly/winter_2019/")
africa <- read_sf("shapefiles/Africa_SHP/Africa.shp")

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
st_write(markets_buffer, "shapefiles/markets_buffer.shp")
