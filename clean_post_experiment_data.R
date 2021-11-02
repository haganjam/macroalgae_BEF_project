
# Project: Tile experiment

# Title: Clean the experiment data from the initial measurements

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)

# check that the correct folder is present
if(! dir.exists(here("experiment_data"))){
  print("make a folder called experiment_data in the working directory and save the initial experiment data, see README for details")
}

# clear the objects in the environment
rm(list = ls())

# load the raw initial data
post_dat <- read_csv(file = here("experiment_data/tile_experiment_post.csv"),
                     col_types = list(date = col_character(),
                                      tile_id = col_character(),
                                      plant_id = col_character(),
                                      binomial_code = col_character(),
                                      lost_0_1 = col_integer(),
                                      number_of_bladders = col_integer(),
                                      number_receptacles = col_integer(),
                                      epiphyte_wet_weight_g = col_character(),
                                      photo_id_time = col_character(),
                                      brittleness = col_integer(),
                                      X35 = col_character(),
                                      Obs = col_character(),
                                      person_photo = col_character(),
                                      person_measure = col_character(),
                                      person_writing = col_character(),
                                      `elena measurement error, add to length` = col_double() )
                                     )

# parsing warning said that epiphyte wet-weight was not a numeric variable
# this was based on row 701
# here, we find out why and correct it then convert the variable into a double
post_dat[701, ]$epiphyte_wet_weight_g <- "1.5"

# convert epiphyte wet weight into a numeric variable
post_dat <- 
  post_dat %>%
  mutate(epiphyte_wet_weight_g = as.numeric(epiphyte_wet_weight_g))

# check that this worked
post_dat[701, ]$epiphyte_wet_weight_g
str(post_dat$epiphyte_wet_weight_g)

# ignore missing column warning as this will be corrected in the script

# check the basic data structures
head(post_dat)
str(post_dat)
summary(post_dat)

# remove the missing column as this was used as a spacing variable when entering the data
post_dat <- 
  post_dat %>%
  select(-X35)

# make separate columns for site, horizontal position and depth treatment from tile_id
# regular expressions: https://www.journaldev.com/36776/regular-expressions-in-r
# can also use simply substring
post_dat <- 
  post_dat %>%
  mutate(site_code = substr(tile_id, 1, 1),
         hor_pos = substr(tile_id, 2, 2),
         depth_treatment = substr(tile_id, 3, 3),
         plant_no = substr(plant_id, 4, 4))

# check if these inputs are correct
unique(post_dat$tile_id)
length(unique(post_dat$tile_id)) == (5*4*4)

unique(post_dat$site_code)
length(unique(post_dat$site_code)) == 5

unique(post_dat$hor_pos)
length(unique(post_dat$hor_pos)) == 4

unique(post_dat$depth_treatment)
length(unique(post_dat$depth_treatment)) == 4

# how many data.rows do we have?
# there is one missing plant
nrow(post_dat)

# how to deal with the missing data
sum(is.na(post_dat$total_length_cm))
sum(is.na(post_dat$tray_weight_rest_g))
sum(is.na(post_dat$wet_weight_g))

# check for dates that are missing
sum(is.na(post_dat$date))

# load the date_fixer function
source(here("functions/date_fixer.R"))

# run the date_fixer function to fill in the dates
# the date_fixer function assumes that each tile was measured on a certain date
# thus, missing date values are filled in based on the date of their tile

# if multiple dates are associated with a tile, then they are assigned randomly

# if no dates are associated with a tile, then the values remain NAs
unique(post_dat$date)

# fix the incorrect direction dates
post_dat <- 
  post_dat %>%
  mutate(date = if_else(date == "2021_09_02", "02_09_2021", date)) %>%
  mutate(date = if_else(date == "2021_09_01", "01_09_2021", date))

unique(post_dat$date)

post_dat <- 
  post_dat %>%
  group_by(tile_id) %>%
  mutate(date_corrected = date_fixer(date))

sum(is.na(post_dat$date_corrected))

# get rows where we have wet_weights because only these data are usable for comparison
post_dat <- 
  post_dat %>%
  filter(!is.na(wet_weight_g))

sum(is.na(post_dat$date_corrected))








