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
croplands <- raster("saved-output/mosaic_cropland.tif", RAT = T)

## read in precip and crop using croplands

# get chirps dataset
precip_loc <- "/Users/aminaly/Desktop/chirps-v2.0.monthly.nc"
precipitation <- brick(precip_loc) %>% crop(attributes(croplands)$extent)  #extent pulled by reading contents of african shapefile. 

## read in ndvi and crop uding croplands
nd1 <- raster(paste0(getwd(), "/downloaded/ndvi/AVHRR-Land_v005_AVH13C1_NOAA-18_20070101_c20170331233614.nc"))
nd2 <- raster(paste0(getwd(), "/downloaded/ndvi/AVHRR-Land_v005_AVH13C1_NOAA-18_20070102_c20170331234943.nc"))
nd_stack <- stack(nd1, nd2)

#get all NDVI files
ndvi_files <- list.files("../www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/", recursive = T, pattern = "*.nc" )
num_files <- length(ndvi_files)
                         
#loop thorugh and add each layer
for(i in 3:num_files) {
  
  nd_i <- raster(ndvi_files[i])
  nd_stack <- addLayer(nd_stack, nd_i)
  
}

#set z for all of these so they have the right dates
nd_stack <- setZ(b, as.Date('2007-1-1') + 0:num_files)

#grab lats and lons of cropland locations and make spatial points
#cp <- rasterToPoints(croplands, spatial=T)
#cp_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#pts <- spsample(as(croplands@extent, "SpatialPolygons"), 100, type = 'random')
#croplands_pts  <- which(extract(croplands == 2, pts) == 2)

#Filter out only those that are croplands (=2) and turn into a bunch of points
crop_points <- rasterToPoints(croplands, fun=function(x){x==2}, spatial=T)

#Extract over points and get NDVI data
# Run through precipitation brick and extract over the buffers
ndvi_data <- c()

for(i in 1:numfiles){
  print(i)
  temp <- c()
  
  temp$date <- rep(substring(names(precipitation[[i]]), 2), nrow(locs))
  temp$location <- locs$market
  
  velox_obj <- velox(nd_stack[[i]])
  temp_pVals <- velox_obj$extract_points(sp = croplands_pts, small = T)
  
  temp$temp_mean <- lapply(temp_pVals, function(x){mean(x, na.rm = T)}) %>% unlist()  
  
  all_data <- bind_rows(all_data, temp)
  i <- i+1
}

#save this out to make my life easier
saveRDS(all_data, paste0(getwd(), "/", rdsname))
precip <- readRDS(rdsname)
lon <- ncvar_get(croplandsty)


