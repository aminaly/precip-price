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

# get chirps dataset
#precip_loc <- "/Users/aminaly/Desktop/chirps-v2.0.monthly.nc"
#precipitation <- brick(precip_loc) %>% crop(attributes(croplands)$extent)  #extent pulled by reading contents of african shapefile. 

#get all NDVI files
ndvi_files <- list.files("../www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/", recursive = T, pattern = "*.nc", full.names = T)
num_files <- length(ndvi_files)

#Filter out only those that are croplands (=2) and turn into a bunch of points
#crop_points <- rasterToPoints(croplands, fun=function(x){x==2}, spatial=T)
#saveRDS(crop_points, paste0(getwd(),"/saved-output/crop_as_points.rds"))
crop_points <- readRDS("~/saved-output/crop_as_points.rds")

#Extract over points and get NDVI data
# Run through precipitation brick and extract over the buffers
ndvi_data <- c()

for(i in 1:numfiles){
  print(i)
  
  nd <- raster(ndvi_files[1]) %>% crop(extent(croplands))
  
  temp <- c()
  temp$date <- getZ(nd) 

  velox_obj <- velox(nd)
  temp_ndvi_by_point <- velox_obj$extract_points(sp = crop_points)
  
  temp$temp_mean <- lapply(temp_pVals, function(x){mean(x, na.rm = T)}) %>% unlist()  
  
  all_data <- bind_rows(all_data, temp)
  i <- i+1
}

#save this out to make my life easier
saveRDS(all_data, paste0(getwd(), "/", rdsname))
precip <- readRDS(rdsname)
lon <- ncvar_get(croplandsty)


