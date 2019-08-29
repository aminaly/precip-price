## Use this file to plot regressions with ideal models
library(ggplot2)
library(gridExtra)

#set up directories and pull in relevant data
ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
price <- readRDS("saved-output/formatted-price.rds")



source(paste0(getwd(), "/functions_for_analysis.R"))

# variables that might change
run_daily <- TRUE
include_zeros <- FALSE
bufs <- c(.25, 1, 2)
par(mar=c(2,4,4,.5))

## adjust mins and maxs if you must
ymaxs <- c(.3, .5, .5, .1, .1)
ymins <- c(-.2, -.2, -.1, -.1, -.1)
ys <- cbind(rep("sorghum", 5), ymaxs, ymins)
ymaxs <- c(40, .2, .5, 4, 2)
ymins <- c(-12, -.2, -1, -18, -8)
ys <- rbind(ys, cbind(rep("millet", 5), ymaxs, ymins))
ymaxs <- c(.5, 1, 1, .5, .1)
ymins <- c(-3, -.5, -1, -.5, -.1)
ys <- rbind(ys, cbind(rep("maize", 5), ymaxs, ymins))

comps <- read.csv("model-comps/Master_Model_Comps.csv", stringsAsFactors = F)
comps <- comps %>% filter(daily == run_daily) %>% filter(zeros == include_zeros)

pdf("plots/Average_NoZeros.pdf")
par(mfrow=c(2,1))

for(i in 1:(nrow(comps))) {
  
  #get the current row we're working with 
  row <- comps[i,]
  buf <- row$buffer
  
  #data for use by histogram
  rdsname <- paste0("precip/", buf, "_precip.rds")
  precip <- readRDS(rdsname)
  pp_data <- clean_precip(precip)
  sorghum <- get_grain_data(pp_data, "sorghum", run_daily, include_zeros)
  millet <- get_grain_data(pp_data, "millet", run_daily, include_zeros)
  maize <- get_grain_data(pp_data, "maize", run_daily, include_zeros)
  
  #get the data for this row
  data <- switch(row$grain,
                 "sorghum" = sorghum,
                 "maize" = maize,
                 "millet" = millet)
  
  #set the title of this plot
  title <- get_bootname(row$grain, "all", row$time_period, run_daily, include_zeros)
  title <-  str_sub(title, end=-5)
  
  #if there is no ideal model we'll just say so and move on. Else start the plot
  if(row$model_choice == "") {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(title, "No Ideal Model"), cex = 1.6, col = "black")
    hist(data[,row$time_period])
    next
  }
  
  #get the right bootstrapped data
  bootname <- get_bootname(row$grain, buf, row$time_period, run_daily, include_zeros)
  boots <- readRDS(paste0(getwd(), "/bootstraps/", bootname))
  
  #plot with the title
  x <- 0:max(data[,row$time_period], na.rm = T)
  coef <- boots[[1]]
  level <- row$model_choice
  
  bts <- matrix(nrow=1000,ncol=length(x))
  
  #using bootstraps, lets make all the lines
  if(level == "log_2") {
    for (j in 1:1000) {
      yy <- x*coef[j,1] + x^2*coef[j,2]  
      yy <- yy - yy[x=80]
      bts[j,] <- yy 
    }
    
  } else if (level == "log_3") {
    for (j in 1:1000) {
      yy <- x*coef[j,1] + x^2*coef[j,2] + x^3*coef[j,3] 
      yy <- yy - yy[x=80]
      bts[j,] <- yy
    }
  } else {
    for (j in 1:1000) {
      yy <- x*coef[j]
      yy <- yy - yy[x=80]
      bts[j,] <- yy
    }
  }
  
  #get confidence intervals
  colors <- c("orangered", "goldenrod", "navy")
  confint <- apply(bts,2,function(x) quantile(x,probs=c(0.05,0.5,0.95), na.rm = T)) 
  colorpick <- switch(as.character(buf), 
                      "0.25" = colors[1],
                      "1" = colors[2],
                      "2" = colors[3])
  
  #plot
  #ifelse(run_daily, xl <- c(0, 400), xl <- c(0, 2000))
  plot(x, confint[2,], type = "l", las=1,xlab="precip",ylab="value", col=colorpick, main=paste(title, level))   #median estimate across bootstraps
  polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col=adjustcolor(colorpick, alpha=.3),border = NA)

  #add the histogram underneath
  hist(data[,row$time_period],main = NULL, ann=F, xlim = c(0, max(x)))
  
}

dev.off()




