## This is the file that will run through every single NDVI and first crop by 'croplands' 
## and then extract over markets

### SETUP
library(sf); library(sp); library(raster)
library(velox); library(lfe); library(ncdf4)
library(rgdal); library(lubridate); library(mapview)
library(inflection)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       ifelse(dir.exists("/Users/aminaly/Box Sync/precip-price"),
              setwd("/Users/aminaly/Box Sync/precip-price"),
              setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price")))

#get price data
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
years <- c(2007:2017)
args <- commandArgs(trailingOnly = TRUE)
y <- as.numeric(args[1])
year <- years[y]

### CROPLAND PREP
## This doesn't have to be run more than once so it's commented out
## read in the cropland tif file as raster and prep it
#croplands <- raster("../mosaic_cropland.tif", RAT = T)
#cl <- croplands %>% crop(c(20,30,0,10))
#cl <- clamp(cl, lower=2, useValues=FALSE)

# percent of cell that is croplands
#perc <- function(x, na.rm = F) {
#  return( sum(!is.na(x)) / length(x))
#}

# aggregate the cropland data to match ndvi
#cl_aggregated <- raster::aggregate(cl, (37106/200), fun = perc)
# zeros should become NA
#cl_aggregated[cl_aggregated == 0] <- NA
#saveRDS(cl_aggregated, "./saved-output/aggregated_croplands.rds")
cl_aggregated <- readRDS("./saved-output/aggregated_croplands.rds")

#get all NDVI directories (one a year)
ndvi_folders <- list.dirs(paste0("../www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/"), full.names = T)
num_files <- length(ndvi_files)

### MARKETS SETUP
#get market locations 
locs <- unique(price[,c(1:2,16:17)])
locs <- locs[which(locs$latitude < 999),]

# Transform your markets
markets = st_as_sf(locs,coords=c("longitude","latitude"))
st_crs(markets) <- 4326
markets <- st_transform(markets, 4326)

### CALCULATIONS FOR NDVI
#Extract over points and get NDVI data only in croplands and then collective NDVI in market area
ndvi_data <- c()
bufs <- c(.25, .5, .75, 1, 2, 3, 4, 5)

# get the date of the inflection point and make a raster of it
# this is done by using ese in the 'inflection' package
get_inflection <- function(y) {
  y <- na.omit(y)
  xs <- 1:length(y)
  plot(xs, y, add = T)
  ind <- check_curve(xs, y)$index
  inflec <- ese(xs, y, ind)[3]
  return(inflec)
}

for(buf in bufs) {
  
  #check to see if we've extracted before
  #rdsname <- paste0("ndvi/", buf, "_ndvi.rds")
  #if(file.exists(paste0(getwd(), rdsname))) stop("Already exists")
  
  # set up the buffers for your markets
  markets_buffer = st_buffer(markets, buf)
  markets_buffer <- as(markets_buffer, 'Spatial')
  
  # Run through  ndvi stacks and: 
  # 1. mask by croplands then 
  # 2. flatten and return single raster of inflection month then 
  # 3. extract over buffers
  for(i in 1:numfiles){
    print(i)
    
    #Get NDVI and prep it to be masked by croplands
    ndvi_files <- list.files(ndvi_folders[i], full.names = T, pattern = "*.nc")
    nd_stack <- stack(ndvi_files) %>% crop(c(20,30,0,10))
    
    #mask by croplands first
    cl_agg_resampled <- projectRaster(cl_aggregated, nd_stack, method='bilinear')
    m <- mask(nd_stack, cl_agg_resampled)
    
    #flatten the stack to only get the inflection points
    nd_flat <- calc(nd_stack, fun = get_inflection)

    temp <- c()
    coords <- coordinates(crop_points)
    temp$lon <- coords$x
    temp$lat <- coords$y
    temp$date <- rep(getZ(nd), nrow(temp$coords))
    
    velox_obj <- velox(crop_points)
    temp_ndvi_by_point <- velox_obj$extract_points(sp = cropped_crops, small = T)
    
    temp$temp_mean <- lapply(temp_ndvi_by_point, function(x){mean(x, na.rm = T)}) %>% unlist()  
    
    ndvi_data <- bind_rows(ndvi_data, temp)
  }
  
  #save this out to make my life easier
  saveRDS(all_data, paste0(getwd(), "/", rdsname))
  precip <- readRDS(rdsname)
  
}




