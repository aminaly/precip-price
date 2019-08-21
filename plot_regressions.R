## Use this file to plot regressions with ideal models

#set up directories and pull in relevant data
ifelse(dir.exists("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/Users/amina/Documents/Stanford/precip-price"),
       setwd("/oak/stanford/groups/omramom/group_members/aminaly/precip-price"))
price <- readRDS("saved-output/formatted-price.rds")

# variables that might change
run_daily <- FALSE
include_zeros <- FALSE
bufs <- c(.25, 1, 2)

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

par(mfrow=c(6,2))
comps <- read.csv("model-comps/Master_Model_Comps.csv", stringsAsFactors = F)
comps <- comps %>% filter(daily == run_daily) %>% filter(zeros == include_zeros)

for(i in 1:(nrow(comps)/3)) {
  
  #get the current row we're working with 
  row <- comps[i,]
  
  #if this is the first of the grains, plot title first
  if((i %% 5) == 1) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, row$grain, cex = 1.6, col = "black")
  }

  #set the title of this plot
  title <- paste0(row$grain, "_", row$time_period, "_")
  title <- ifelse(run_daily, paste0(title, "average_"), paste0(title, "accumulated_"))
  title <- ifelse(include_zeros, paste0(title, "zeros"), paste0(title, "nozeros"))
  
  #if there is no ideal model we'll just say so and move on. Else start the plot
  if(row$model_choice == "") {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(title, "No Ideal Model"), cex = 1.6, col = "black")
    next
  } else {
    #xl <- ifelse(run_daily, c(0, 400), c(0, 2000))
    plot(100,xlim=c(0, 2000),ylim=c(as.numeric(ys[i,3]),as.numeric(ys[i,2])),las=1,xlab="precip",ylab="value", main=title)  
  }
  
  for(b in 1:3) {
    
    buf <- bufs[b]
    #get the right precip data
    rdsname <- paste0("precip/", buf, "_precip.rds")
    precip <- readRDS(rdsname)
    source(paste0(getwd(), "/functions_for_analysis.R"))
    
    
    ## Get data for each commodity
    #lets get the data we want specificially & merge in seasonality
    sorghum <- get_grain_data(pp_data, "sorghum", run_daily, include_zeros)
    millet <- get_grain_data(pp_data, "millet", run_daily, include_zeros)
    maize <- get_grain_data(pp_data, "maize", run_daily, include_zeros)
    
    #set title for bootstrap
    title <- paste0(row$grain, "_", buf, "_", row$time_period, "_")
    title <- ifelse(run_daily, paste0(title, "average_"), paste0(title, "accumulated_"))
    title <- ifelse(include_zeros, paste0(title, "zeros"), paste0(title, "nozeros"))
    
    #get the right model given the buffer 
    model_choice <- comps %>% filter(buffer == buf) %>% filter(time_period == row$time_period) %>% filter(grain == row$grain)
    model_choice <- model_choice$model_choice
      
    #get the right data and level
    data <- switch(row$grain,
                   "sorghum" = sorghum,
                   "maize" = maize,
                   "millet" = millet)
    level <- switch(as.character(model_choice),
                    "Log_Linear" = 1,
                    "Log_2" = 2,
                    "Log_3" = 3,
                    "Log_Log" = "log")
    
    #get model
    mod <- get_model_regression(data, row$time_period, level, log=TRUE)
    #create bootstrap name based on grain, buffer, time period, avg/accumulated, and zeroes/nozeros
    xrge <- ifelse(run_daily, 0:400, 0:2000)
    boots <- bootstrap_data(data, mod, row$time_period, xrange=xrge, level=level, log=TRUE, short = F, name = paste0(title, ".rds"))
    
    #plot with the title
    x <- boots[[2]]
    yy <- boots[[3]]
    coef <- boots[[1]]
    
    bts <- matrix(nrow=1000,ncol=length(x))
    
    #using bootstraps, lets make all the lines
    if(level == 2) {
      for (j in 1:1000) {
        yy <- x*coef[j,1] + x^2*coef[j,2]  
        yy <- yy - yy[x=80]
        bts[j,] <- yy 
      }
      
    } else if (level == 3) {
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
    confint <- apply(bts,2,function(x) quantile(x,probs=c(0.05,0.5,0.95))) 
    polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col=adjustcolor(colors[b], alpha=.3),border = NA)
    lines(x,confint[2,], col=colors[b])  #median estimate across bootstraps
    
  }
  
}





