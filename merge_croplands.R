
library(raster)
# Source files with functions useful/data prep
ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))

list_files <- list.files(paste0(getwd(), "/downloaded/GFSAD30AFCE_CROPPED/"), full.names = T)

comb <- raster(list_files[1])

for(i in 2:15) {
  
  file <- raster(list_files[i])
  comb <- raster::merge(comb, file)
  
}