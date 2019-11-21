## This is the file that will run through every single NDVI and first crop by 'croplands' 
## and then extract over markets

### SETUP
library(sf); library(sp); library(raster)
library(velox); library(lfe); library(ncdf4)
library(rgdal); library(lubridate); library(inflection)
library(stringr)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       ifelse(dir.exists("/Users/aminaly/Box Sync/precip-price"),
              setwd("/Users/aminaly/Box Sync/precip-price"),
              setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price")))

#get price data
price <- readRDS("saved-output/formatted-price.rds")

## pick up args from commandline/sbatch
# years <- c(2007:2017)
# args <- commandArgs(trailingOnly = TRUE)
# y <- as.numeric(args[1])
# year <- years[y]

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
ndvi_folders <- list.dirs("../www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access", full.names = T)
num_folders <- length(ndvi_folders)

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

# Used by calc to get the date of the inflection point and make a flattened raster of it
# this is done by using ese in the 'inflection' package
get_inflection <- function(y, yr, timing) {
  
  if(all(is.na(y))) return(-1)

  #find the max NDVI value. This is peak growing season 
  peak_grow_date <- which.max(y)
  
  # get rid of NAs and select only time after or before peak growing based on user input
  y <- na.omit(y)
  if(timing =="pre") {
    y_p <- y[1:peak_grow_date]
  } else {
    y_p <- y[peak_grow_date:length(y)]
  }
  
  xs <- 1:length(y_p)
  
  ind <- tryCatch({
    check_curve(xs, y_p)$index
    }, error = function(e) {
      -1
    })
  
  if(ind == -1) return(-1)
  inflec <- ese(xs, y_p, ind)[3]
  
  ifelse(timing == "pre", day <- peak_grow_date - inflec, day <- peak_grow_date + inflec)
  timing <- to_month(day, yr)
  return(timing)
}

## Given a day and year, return a date object of the correct month
to_month <- function(day, year) {
  first <- as.Date(paste0("1/1/", year), format="%m/%d/%Y")
  
  return(month(first+(day-1)))
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
  for(i in 2:numfiles){
    print(i)
    
    #Get NDVI and prep it to be masked by croplands
    ndvi_files <- list.files(ndvi_folders[i], full.names = T, pattern = "*.nc")
    nd_stack <- stack(ndvi_files) %>% crop(c(20,30,0,10))
    
    #mask by croplands first
    cl_agg_resampled <- projectRaster(cl_aggregated, nd_stack, method='bilinear')
    masked_nd <- mask(nd_stack, cl_agg_resampled)
    
    #flatten the stack to only get the inflection points
    yr <- str_sub(ndvi_folders[i], start = -4)
    nd_flat_pre <- calc(masked_nd, function(x){get_inflection(x, yr, "pre")})
    nd_flat_post <- calc(masked_nd, function(x){get_inflection(x, yr, "post")})

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




