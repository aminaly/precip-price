## This is the file that will run through every single NDVI and first crop by 'croplands' 
## and then extract over markets

### SETUP
library(sf); library(raster)
library(velox); library(lfe); library(ncdf4)
library(rgdal); library(lubridate); library(inflection)
library(stringr); library(dplyr); library(RColorBrewer)

ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       ifelse(dir.exists("/Users/aminaly/Box Sync/precip-price"),
              setwd("/Users/aminaly/Box Sync/precip-price"),
              setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price")))

#get price data
price <- readRDS("saved-output/formatted-price.rds")
#keep only the countries we care about 
price <- price %>% filter(country %in% c("Mauritania", "Chad", "Nigeria"))

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
ƒ
## Given a day and year, return a date object of the correct month
to_month <- function(day, year) {
  first <- as.Date(paste0("1/1/", year), format="%m/%d/%Y")
  
  return(month(first+(day-1)))
}

# Run through  ndvi stacks and: 
# 1. mask by croplands then 
# 2. flatten and return single raster of inflection month then 
# 3. extract over buffers
yrs <- c()

for(i in 2:num_folders){
  print(i)
  
  #Get NDVI and prep it to be masked by croplands
  ndvi_files <- list.files(ndvi_folders[i], full.names = T, pattern = "*.nc")
  plot  
  #mask by croplands first
  cl_agg_resampled <- projectRaster(cl_aggregated, nd_stack, method='bilinear')
  masked_nd <- mask(nd_stack, cl_agg_resampled)
  
  #flatten the stack to only get the inflection points
  yr <- str_sub(ndvi_folders[i], start = -4)
  yrs <- c(yrs, yr)
  nd_flat_pre <- calc(masked_nd, function(x){get_inflection(x, yr, "pre")})
  nd_flat_post <- calc(masked_nd, function(x){get_inflection(x, yr, "post")})
  nd_flat_max <- calc(masked_nd, function(x){max(x, na.rm=T)})
  
  
  #add to the stack
  if(!exists('season_start')) {
    season_start <- nd_flat_pre
    season_end <- nd_flat_post
    season_mid <- nd_flat_max
  } else {
    season_start <- stack(season_start, nd_flat_pre)
    season_end <- stack(season_end, nd_flat_post)
    season_mid <- stack(season_mid, nd_flat_max)
  } 
  
}

#set z values
season_start <- setZ(season_start, yrs)
season_end <- setZ(season_end, yrs)
season_mid <- setZ(season_mid, yrs)

#save out the season stacks
writeRaster(season_end, paste0(getwd(), "/saved-output/ndvi_seasonality/season_end_2015.tif"))
writeRaster(season_start, paste0(getwd(), "/saved-output/ndvi_seasonality/season_start_2015.tif"))
writeRaster(season_start, paste0(getwd(), "/saved-output/ndvi_seasonality/season_mid_2015.tif"))

##time for some light plotting

#lets get the africa outline
#africa <- read_sf("./downloaded/Africa_SHP/Africa.shp")
#st_crs(africa) <- 3857
#africa <- st_transform(africa,  3857)

plot(season_start, col = brewer.pal(12, 'YlGn'), main = "Season Start")
plot(season_end, col = brewer.pal(12, 'YlGn'), main = "Season End")
plot(season_mid, col = brewer.pal(10, "YlGn"), main = "Max NDVI")

### MARKETS SETUP
#get market locations 
locs <- unique(price[,c(1:2,16:17)])
locs <- locs[which(locs$latitude < 999),]

# Transform your markets
markets = st_as_sf(locs,coords=c("longitude","latitude"))
st_crs(markets) <- 3857
markets <- st_transform(markets, 3857)

for(buf in bufs) {
  markets_buffer = st_buffer(markets, buf)
  markets_buffer <- as(markets_buffer, 'Spatial')
  markets_buffer <- st_as_sf(markets_buffer)
  st_write(markets_buffer, paste0("./saved-output/buffers/market_buffer_", buf, ".shp"), update=T)
  
}

## Now lets do some extractions based on different buffers
for(buf in bufs) {
  
  year <- 2015
  #check to see if we've extracted before
  rdsname <- paste0("saved-output/ndvi_seasonality/", buf, "_avg_season.rds")
  if(file.exists(paste0(getwd(), rdsname))) stop("Already exists")
  
  # set up the buffers for your markets
  markets_buffer = st_buffer(markets, buf)
  markets_buffer <- as(markets_buffer, 'Spatial')

  #lets extract some averages 
  #within the area of each of these markets
  season_end <- raster(paste0("./saved-output/ndvi_seasonality/season_end_", year, ".tif"))
  season_start <- raster(paste0("./saved-output/ndvi_seasonality/season_start_", year, ".tif"))
  season_mid <- raster(paste0("./saved-output/ndvi_seasonality/season_mid_", year, ".tif"))
  
  seasonality_by_market <- c()
  for(j in 1:length(yrs)){
    temp <- c()
    #extract in each market buffer and find weighted average
    velox_obj <- velox(season_end)
    temp_ndvi_by_point <- velox_obj$extract(sp = markets_buffer, small = T, fun=function(x){mean(x, na.rm=TRUE)})
    
    temp$end_mean <- lapply(temp_ndvi_by_point, 
                                             function(x){ raster::weighted.mean(x, cl_aggregated, na.rm = T) }) %>% unlist()  
    
    velox_obj <- velox(season_start)
    temp_ndvi_by_point <- velox_obj$extract(sp = markets_buffer, small = T, fun=function(x){mean(x, na.rm=TRUE)})
    
    temp$start_mean <- lapply(temp_ndvi_by_point, 
                                             function(x){ mean(x, na.rm = T) }) %>% unlist()
    
    velox_obj <- velox(season_mid)
    temp_ndvi_by_point <- velox_obj$extract(sp = markets_buffer, small = T)
    
    temp$mid_mean <- lapply(temp_ndvi_by_point, 
                              function(x){ raster::weighted.mean(x, cl_aggregated, na.rm = T) }) %>% unlist()
    
    seasonality_by_market <- bind_rows(seasonality_by_market, temp)
  }
}




