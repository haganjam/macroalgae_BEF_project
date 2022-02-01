
# Project: Tile experiment

# Title: Function to calibrate the depth relative to RH2000 given ViVa app measurement error

# function to calculate depth relative to the RH2000 standard of a focal sample
# using the ViVa depth and the actual depth measured concurrently

# the function uses a fitted model between ViVa depth and SMHI published depth
# to correct the ViVa depth measurement

# depth relative to the RH2000 standard is then calculated as:
# depth relative to RH2000 standard = f(ViVa depth) + actual depth

# the function calculates the mean prediction, se and 95% upper and lower CIs

# arguments:
# depth_measured = actual measured depth in the field
# depth_viva = ViVa depth taken concurrently with the field depth measurement
# output = chosen output ("mean", "se", "lower_ci_95", "upper_ci_95")

depth_correcter <- function(depth_measured, depth_viva, output = "mean") {
  
  # check if groundhog is loaded
  if( !("groundhog" %in% installed.packages()[,1] )  ) {
    
    stop("error! this function requires groundhog")
    
  }
  
  # load libraries using groundhog
  library(groundhog)
  groundhog.day <- "2022-01-01"
  pkgs <- c("here", "dplyr")
  groundhog.library(pkgs, groundhog.day)
  
  warning("this requires a model to have been fit, see readme for details")
  
  # read in the gam model
  gam.1 <- readRDS(file = here("analysis_data/sea_cal_gam.rds"))
  
  df <- data.frame(water_level_cm_viva = depth_viva)
  
  df.pred <- predict(gam.1, newdata = df, se.fit = TRUE)
  
  if (output == "mean") {
    
    z <- df.pred$fit
    
  } else if (output == "se") {
    
    z <- df.pred$se.fit
    
  } else if (output == "lower_ci_95") {
    
    z <- df.pred$fit - (2 * df.pred$se.fit)
    
  } else if (output == "upper_ci_95") {
    
    z <- df.pred$fit + (2 * df.pred$se.fit)
    
  } else {
    
    stop("specify correct output variable")
    
  }
  
  (z + depth_measured)
  
}

### END
