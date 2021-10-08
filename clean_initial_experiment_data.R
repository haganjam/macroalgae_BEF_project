
# Project: Tile experiment

# Title: Clean the experiment data from the initial measurements

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)

# load the raw initial data
init_dat <- read_csv(file = here("experiment_data/tile_experiment_data_plants_pre.csv"),
                     col_types = list(date = col_character(), 
                                      time = col_character(),
                                      tile_id = col_character(),
                                      depth_treatment = col_character(),
                                      plant_no = col_integer(),
                                      plant_id = col_character(),
                                      binomial_code = col_character(),
                                      initial_wet_weight_g = col_double(),
                                      initial_length_cm = col_double(),
                                      observers = col_character(),
                                      origin_site_code = col_character(),
                                      final_length_cm = col_character(),
                                      final_wet_weight_g = col_character(),
                                      final_dry_weight_g = col_character(),
                                      Surface_area_mm2 = col_double(),
                                      Perimeter_mm = col_double(),
                                      notes = col_character(),
                                      sex_fu_ve = col_character(),
                                      `Who did ImageJ` = col_character() ) )

# select the relevant columns
init_dat <- 
  init_dat %>%
  select(date, time, tile_id, depth_treatment, plant_no, plant_id, origin_site_code, binomial_code,
         sex_fu_ve, initial_wet_weight_g, initial_length_cm, observers, notes)

# make separate columns for site, horizontal position and depth treatment from tile_id
# regular expressions: https://www.journaldev.com/36776/regular-expressions-in-r
# can also use simply substring
init_dat <- 
  init_dat %>%
  mutate(site_code = substr(tile_id, 1, 1),
         hor_pos = substr(tile_id, 2, 2),
         depth_treatment = substr(tile_id, 3, 3))

# check if these inputs are correct
unique(init_dat$site_code)
length(unique(init_dat$site_code)) == 5

unique(init_dat$hor_pos)
length(unique(init_dat$hor_pos)) == 4

unique(init_dat$depth_treatment)
length(unique(init_dat$depth_treatment)) == 4


