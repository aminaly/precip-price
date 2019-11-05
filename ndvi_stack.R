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
#saveRDS(crop_points, paste0(getwd(),"/saved-output/crop_as_points.rds"))
crop_points <- readRDS("~/saved-output/crop_as_points.rds")

#Extract over points and get NDVI data
# Run through precipitation brick and extract over the buffers
ndvi_data <- c()

for(i in 1:numfiles){
  print(i)
  
  nd <- raster(ndvi_files[i]) %>% crop(extent(croplands))
  
  temp <- c()
  temp$coords <- coordinates(crop_points)
  temp$date <- rep(getZ(nd), nrow(temp$coords))

  velox_obj <- velox(nd)
  temp_ndvi_by_point <- velox_obj$extract_points(sp = crop_points)
  
  temp$temp_mean <- lapply(temp_ndvi_by_point, function(x){mean(x, na.rm = T)}) %>% unlist()  
  
  ndvi_data <- bind_rows(ndvi_data, temp)
}

#save this out to make my life easier
saveRDS(ndvi_data, paste0(getwd(), paste0("/saved-output/ndvi_croplands_", year,".rds"))



