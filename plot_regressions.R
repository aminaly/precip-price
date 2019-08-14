#### Regression plots ####
## Plot Sorghum
lst <- list(sorghum_sow_boot, sorghum_grow_boot, sorghum_harv_boot, sorghum_sup_boot, sorghum_onem_boot)
nmes <- c("Sowing (Linear)", "Growing (Quadratic)", "Harvest (Linear)", "Supply Chain (Linear)", "One Month Prior to Sale (Quadratic)")
par(mfrow=c(3,2))

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste("Sorghum"), cex = 1.6, col = "black")
ymaxs <- c(35, 55, 20, 5, 12)
ymins <- c(-5, -15, -15, -5, -7)

for(nm in 1:5) {
  
  l <- lst[[nm]]
  nme <- nmes[nm]
  
  x <- l[[2]]
  yy <- l[[3]]
  coef <- l[[1]]
  
  if(ncol(coef) > 1) {
    plot(100,xlim=c(0,700),ylim=c(ymins[nm], ymaxs[nm]),las=1,xlab="precip",ylab="value", main=nme )  
    print("made it here")
    print(ncol(coef))
    for (i in 1:100) {
      yy <- x*coef[i,1] + x^2*coef[i,2]  
      yy <- yy - yy[x=80]
      lines(x,yy,lwd=0.5)
    }
  } else {
    plot(100,xlim=c(0,700),ylim=c(ymins[nm], ymaxs[nm]),las=1,xlab="precip",ylab="value", main=nme)  
    for (i in 1:100) {
      yy <- x*coef[i]
      yy <- yy - yy[x=80]
      lines(x,yy,lwd=0.5)
    }
  }
}

## Plot Millet
lst <- list(millet_sow_boot, millet_grow_boot, millet_harv_boot, millet_sup_boot, millet_onem_boot)
nmes <- c("Sowing (Quadratic)", "Growing (Quadratic)", "Harvest (Linear)", "Supply Chain (Quadratic)", "One Month Prior to Sale (Quadratic)")
par(mfrow=c(3,2))
ymaxs <- c(30, 30, 22, 5, 15)
ymins <- c(-50, -5, -50, -10, -10)

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste("Millet"), cex = 1.6, col = "black")

for(nm in 1:5) {
  
  l <- lst[[nm]]
  nme <- nmes[nm]
  
  x <- l[[2]]
  yy <- l[[3]]
  coef <- l[[1]]
  
  if(ncol(coef) > 1) {
    plot(100,xlim=c(0,700),ylim=c(ymins[nm], ymaxs[nm]),las=1,xlab="precip",ylab="value", main=nme )  
    print(ncol(coef))
    for (i in 1:100) {
      yy <- x*coef[i,1] + x^2*coef[i,2]  
      yy <- yy - yy[x=80]
      lines(x,yy,lwd=0.5)
    }
  } else {
    plot(100,xlim=c(0,700),ylim=c(ymins[nm], ymaxs[nm]),las=1,xlab="precip",ylab="value", main=nme)  
    for (i in 1:100) {
      yy <- x*coef[i]
      yy <- yy - yy[x=80]
      lines(x,yy,lwd=0.5)
    }
  }
}

##Plot Maize
lst <- list(maize_sow_boot, maize_grow_boot, maize_harv_boot, maize_sup_boot, maize_onem_boot)
nmes <- c("Sowing (Quadratic)", "Growing (Quadratic)", "Harvest (Linear)", "Supply Chain (Quadratic)", "One Month Prior to Sale (Quadratic)")
par(mfrow=c(3,2))

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste("Maize"), cex = 1.6, col = "black")
ymaxs <- c(22, 10, 10, 5, 7)
ymins <- c(-5, -15, -15, -5. -7)

for(nm in 1:5) {
  
  l <- lst[[nm]]
  nme <- nmes[nm]
  
  x <- l[[2]]
  yy <- l[[3]]
  coef <- l[[1]]
  
  if(ncol(coef) > 1) {
    plot(100,xlim=c(0,700),ylim=c(ymins[nm], ymaxs[nm]),las=1,xlab="precip",ylab="value", main=nme )  
    print(ncol(coef))
    for (i in 1:100) {
      yy <- x*coef[i,1] + x^2*coef[i,2]  
      yy <- yy - yy[x=80]
      lines(x,yy,lwd=0.5)
    }
  } else {
    plot(100,xlim=c(0,700),ylim=c(ymins[nm], ymaxs[nm]),las=1,xlab="precip",ylab="value", main=nme)  
    for (i in 1:100) {
      yy <- x*coef[i]
      yy <- yy - yy[x=80]
      lines(x,yy,lwd=0.5)
    }
  }
}


#### Let's do some demeaned plots ####

demean_plot <- function(data, to_log=FALSE) {
  
  #get the name of this data
  name <- data$type[1]
  
  #cut out unnecessary data and set columns as factors
  short_data <- data[,c(14,21,24,25,34:38)]
  short_data$location <- as.factor(short_data$location)
  short_data$yrmnth <- as.factor(short_data$yrmnth)
  
  #set up the plot
  par(mfrow=c(3,2))
  
  title <- ifelse(to_log, paste0(name, " demeaned. Log Value \n"), paste0(name, "demeaned"))
  
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, title, cex = 1.6, col = "black")
  
  for(nm in 1:5) {
    
    sm <- short_data[,c(1,3,4,nm + 4)]
    sm <- sm[complete.cases(sm),]
    demean_data <- demeanlist(sm, list(sm$location, sm$yrmnth))
    
    ylabel <- ifelse(to_log, "log value", "value")
    
    if(to_log) {
      plot(x = demean_data[,2], y =log(demean_data[,4]), las=1,xlab="precip",
         ylab=ylabel, main=colnames(sm)[4], pch = 19, col="darkgrey")  
      plot_model 
    } else {
      plot(x = demean_data[,2], y =demean_data[,4], las=1,xlab="precip",
           ylab=ylabel, main=colnames(sm)[4], pch = 19, col="darkgrey")
    }
  }
}

#demeaned plots
demean_plot(sorghum)
demean_plot(millet)
demean_plot(maize)

#demeaned plots- log values
demean_plot(sorghum, to_log=TRUE)
demean_plot(millet, to_log=TRUE)
demean_plot(maize, to_log=TRUE)


#### Regression plots with ideal model ####
buf <- 1
rdsname <- paste0("precip/", buf, "_precip.rds")
precip <- readRDS(rdsname)
precipname <- paste0("precip/", buf, "_ppdata.csv")
run_daily <- TRUE
include_zeros <- TRUE

## Get data for each commodity
#lets get the sorghum we want specificially & merge in seasonality
sorghum <- filter_grain(pp_data, "Sorghum")
sorghum <- merge_seasons(sorghum, "sorghum")
#get locations with the right set of dates
sorghum <- filter_dates(sorghum, "Sorghum")
#calcualte precip for correct set of dates
sorghum <- calc_precip(sorghum, daily = run_daily, zeros = include_zeros)

#lets get the millet we want specificially & merge in seasonality
millet <- filter_grain(pp_data, "Millet")
millet <- merge_seasons(millet, "millet")
#get locations with the right set of dates
millet <- filter_dates(millet, "Millet")
#calcualte precip for correct set of dates
millet <- calc_precip(millet, daily = run_daily, zeros = include_zeros)

#lets get the maize we want specificially & merge in seasonality
maize <- filter_grain(pp_data, "Maize")
maize <- merge_seasons(maize, "maize")
#get locations with the right set of dates
maize <- filter_dates(maize, "Maize")
#calcualte precip for correct set of dates
maize <- calc_precip(maize, daily = run_daily, zeros = include_zeros)


## mins and maxs
ymaxs <- c(1, .5, 1.2, 1, .2)
ymins <- c(-1, -.5, -1, -1, -.2)
ys <- cbind(rep("sorghum", 5), ymaxs, ymins)
ymaxs <- c(.5, .5, 1, 3, .5)
ymins <- c(-.5, -.5, -1, -12, -.5)
ys <- rbind(ys, cbind(rep("millet", 5), ymaxs, ymins))
ymaxs <- c(.5, 1, 1, .2, .2)
ymins <- c(-1, -.5, -1, -.2, -.2)
ys <- rbind(ys, cbind(rep("maize", 5), ymaxs, ymins))


par(mfrow=c(3,2))
comps <- read.csv("model-comps/Model_Comps_Master.csv", stringsAsFactors = F)
types <- c("p_sow", "p_grow", "p_harv", "p_sup", "p_onemonth")

#lib: 4 = standard, 5 = retail only, 6 = Daily Average, 7 = Daily Average No Days below 1mm
# 8 = New dates (more points) Daily Average No Days below 1mm
type <- 8

comps_plot <- comps %>% filter(Buffer == buf)

for(i in 1:nrow(comps_plot)) {
  
  #select the current row, and make sure it's usable
  row <- comps_plot[i,]
  title <- paste(row$Grain, row$time_period, row[type], "buf=", buf, ", > 0, averages")
  
  #If this is the first of a new section, plot just simple title before continuing
  if((i %% 5) == 1) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(row$Grain), cex = 1.6, col = "black")
  }
  
  #get the right data and level
  data <- switch(row$Grain,
                 "sorghum" = sorghum,
                 "maize" = maize,
                 "millet" = millet)
  level <- switch(as.character(row[type]),
                 "Log_Linear" = 1,
                 "Log_2" = 2,
                 "Log_3" = 3,
                 "Log_Log" = "log")
  
  #if there is no level, plot title and move on
  if(is.na(row[type])) {
    
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, paste(title, "No Ideal Model"), cex = 1.6, col = "black")
    next
  }
  
  #get model and bootstrap
  mod <- get_model_regression(data, row$time_period, level, log=TRUE)
  boots <- bootstrap_data(data, mod, row$time_period, xrange=0:400, level=level, log=TRUE, short = T)

  #plot with the title
  x <- boots[[2]]
  yy <- boots[[3]]
  coef <- boots[[1]]
  
  bts <- matrix(nrow=100,ncol=length(x))
  if(level == 2) {
    for (j in 1:100) {
      yy <- x*coef[j,1] + x^2*coef[j,2]  
      yy <- yy - yy[x=80]
      bts[j,] <- yy 
    }
    
  } else if (level == 3) {
    for (j in 1:100) {
      yy <- x*coef[j,1] + x^2*coef[j,2] + x^3*coef[j,3] 
      yy <- yy - yy[x=80]
      bts[j,] <- yy
    }
  } else {
    for (j in 1:100) {
      yy <- x*coef[j]
      yy <- yy - yy[x=80]
      bts[j,] <- yy
    }
  }
  
  confint <- apply(bts,2,function(x) quantile(x,probs=c(0.05,0.5,0.95))) 
  plot(100,xlim=c(0,400),ylim=c(as.numeric(ys[i,3]),as.numeric(ys[i,2])),las=1,xlab="precip",ylab="value", main=title)  
  polygon(c(x,rev(x)),c(confint[1,],rev(confint[3,])),col="darkolivegreen3",border = NA)
  lines(x,confint[2,])  #median estimate across bootstraps
  
}

