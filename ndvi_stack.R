## This is the file that will run through every single NDVI and first crop by 'croplands' 
## and then extract over markets

library(sf)
#library(sp)
library(raster)
library(velox)
library(mapview)
library(lfe)
library(lubridate)
library(ncdf4)
library(rgdal)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       ifelse(dir.exists("/Users/aminaly/Box Sync/precip-price"),
              setwd("/Users/aminaly/Box Sync/precip-price"),
              setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price")))

## read in the cropland tif file as raster
#croplands <- raster("../mosaic_cropland.tif", RAT = T)

#get price data
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
years <- c(2007:2017)
args <- commandArgs(trailingOnly = TRUE)
y <- as.numeric(args[1])
year <- years[y]

#get all NDVI files for this year
ndvi_files <- list.files(paste0("../www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/", year), pattern = "*.nc", full.names = T)
num_files <- length(ndvi_files)

#Filter out only those that are croplands (=2) and turn into a bunch of points
#crop_points <- rasterToPoints(croplands, fun=function(x){x==2}, spatial=T)
#saveRDS(crop_points, "../crop_as_points.rds")
crop_points <- readRDS("../crop_as_points.rds")

#get market locations 
locs <- unique(price[,c(1:2,16:17)])
locs <- locs[which(locs$latitude < 999),]

# Transform your markets
markets = st_as_sf(locs,coords=c("longitude","latitude"))
st_crs(markets) <- 4326
markets <- st_transform(markets, 4326)

#Extract over points and get NDVI data only in croplands and then collective NDVI in market area
ndvi_data <- c()
bufs <- c(.25, .5, .75, 1, 2, 3, 4, 5)

#first extract over NDVI in the markets (avg NDVI of croplands within x miles)
for(buf in bufs) {
  
  #check to see if we've extracted before
  rdsname <- paste0("ndvi/", buf, "_ndvi.rds")
  #if(file.exists(paste0(getwd(), rdsname))) stop("Already exists")
  
  # set up the buffers for your markets
  markets_buffer = st_buffer(markets, buf)
  markets_buffer <- as(markets_buffer, 'Spatial')
  
  #first, lets crop the croplands by the markets to reduce the number of points we're dealing with
  cropped_crops <- raster::crop(croplands, markets_buffer)
  cropped_crops <- mask(cropped_crops, markets_buffer)
  #cropped_crops <- trim(cropped_crops, values = NA)
  
  # Run through  ndvi files and extract over the cropland buffers
  for(i in 1:numfiles){
    print(i)
    
    nd <- raster(ndvi_files[i]) %>% crop(extent(croplands))
    
    temp <- c()
    coords <- coordinates(crop_points)
    temp$lon <- coords$x
    temp$lat <- coords$y
    temp$date <- rep(getZ(nd), nrow(temp$coords))
    
    velox_obj <- velox(crop_points)
    temp_ndvi_by_point <- velox_obj$extract(sp = cropped_crops, small = T)
    
    temp$temp_mean <- lapply(temp_ndvi_by_point, function(x){mean(x, na.rm = T)}) %>% unlist()  
    
    ndvi_data <- bind_rows(ndvi_data, temp)
  }
  
  #save this out to make my life easier
  saveRDS(all_data, paste0(getwd(), "/", rdsname))
  precip <- readRDS(rdsname)
  
}





#save this out to make my life easier
saveRDS(ndvi_data, paste0(getwd(), "/saved-output/ndvi_croplands_", year,".rds"))

#Now run through each location and find the inflection points. We want a table of:
# lat, lon, start month, end month

coords <- coordinates(crop_points)
for(i in nrow(coords)) {
  
  temp_coord <- coords[i,]
  temp <- ndvi_data %>% filter(lon == temp_coord[1]) %>% filter(lat == temp_coord[2])
  
  
  
  
}


