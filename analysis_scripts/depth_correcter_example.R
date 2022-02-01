
# Project: Tile experiment

# Title: Example of using the depth_correcter.R function

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2022-01-01"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2")
groundhog.library(pkgs, groundhog.day)

# link to source functions
source(here("functions/sea_level_function.R"))
source(here("functions/depth_correcter.R"))

# generate some example data
df.test <- tibble(plant_id = c(1, 2, 3, 4, 5),
                  depth_cm = c(2, 5, 3, -5, 10),
                  depth_cm_viva = c(4, -5, 5, 5, 15)
                  )
# correct the depths using the depth_correcter.R function
df.test <- 
  df.test %>%
  mutate(depth_cm_corrected = depth_correcter(depth_measured = depth_cm,
                                              depth_viva = depth_cm_viva, 
                                              output = "mean"))

df.test

### END
