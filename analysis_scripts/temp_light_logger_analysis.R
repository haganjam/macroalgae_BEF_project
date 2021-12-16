
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the logger data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "tidyr", "readr", "ggplot2")
groundhog.library(pkgs, groundhog.day)

# check the loaded packages for their correct versions
sessionInfo()

# read in the cleaned logger data
logvars <- read_rds(path = here("analysis_data/temp_light_logger_data.rds"))

# which variables would we like to generate?

# classic summary statistics for both temperature and light

# 1. maximum 
# 2. minimum 
# 3. range
# 4. mean
# 5. coefficient of variation

# biologically meaningful temperature variables

# 27 degrees is the upper tolerance for F. vesiculosus: https://www.sciencedirect.com/science/article/pii/S0022098115001276?casa_token=MVT95rCJjKIAAAAA:lFAFLcQy7v3NTwhVUV9lpSnPgykRdccV34sWmyvEySd4QdQV1Urlnt61ebUSMHY4402-vMpGhv4)
# Ascophyllum nodosum: https://www.sciencedirect.com/science/article/pii/S1385110105000365?casa_token=iUfVLnbMJ-4AAAAA:6x8Mso59wCSegKSVKVHFGLjFRNB65G4ykDsQ2hYCDBDGvi_C-gvfiRPo-1CBoIra38R0RHlHcXI
# Fucus serratus: https://www.sciencedirect.com/science/article/pii/S1874778713000871?casa_token=9bE-J_MvWoUAAAAA:pCq4lCynQ5V7sAjcERYNef2ksPQXj10j_G8R7roEAeejBO4oEo7qvQTqI_HUtAKPdUv3qYMo0sk
# Fucus spiralis: ?

# 1. total hours over 27 degrees
# 2. mean length of consecutive periods with temperatures over 27 degrees
# 3. longest consecutive period with temperatures over 27 degrees

logvars_summary <- 
  lapply(logvars, function(data) {
    
    df <- 
      data %>%
      mutate(exceed_27 = if_else(temperature_C > 27, 1, 0))
    
    # calculate periods of consecutive hours that exceeded 27 degrees
    x <- rle(df$exceed_27)
    y <- x$lengths[x$values == 1]
    
    z <- 
      df %>%
      group_by(site_code, water_level_treat) %>%
      summarise(hours_exceeding_27 = sum(exceed_27),
                con_hours_exceeding_27_mean = if_else(length(y) == 0, 0, as.numeric(mean(y)) ),
                con_hours_exceeding_27_max = if_else(length(y) == 0, 0, as.numeric(max(y)) ),
                mean_temp_C = mean(temperature_C),
                sd_temp_C = sd(temperature_C),
                max_temp_C = max(temperature_C),
                min_temp_C = min(temperature_C),
                range_temp_C = diff(range(temperature_C)),
                cv_temp_C = sd(temperature_C)/mean(temperature_C),
                mean_light_C = mean(intensity_lux),
                sd_light_C = sd(intensity_lux),
                max_light_C = max(intensity_lux),
                min_light_C = min(intensity_lux),
                range_light_C = diff(range(intensity_lux)),
                cv_light_C = sd(intensity_lux)/mean(intensity_lux))
    
    z
    
  })

# bind this into a data.frame
# ignore the warning
logvars_summary <- 
  logvars_summary %>%
  bind_rows(.) %>% 
  rename(depth_treatment = water_level_treat) %>%
  arrange(site_code, depth_treatment)

View(logvars_summary)

# plot the light and temperature information


### END
