
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
  mutate(date_corrected = date_fixer(date)) %>%
  ungroup()

sum(is.na(post_dat$date_corrected))

# get rows where we have wet_weights because only these data are usable for comparison
post_dat <- 
  post_dat %>%
  filter(!is.na(wet_weight_g))

sum(is.na(post_dat$date_corrected))

# add Elena's measurement error
# during the measurements, we noticed the one of the researchers mis-read the length measurement
# consistently by one cm
# we correct this by adding one centimeter

post_dat <- 
  post_dat %>%
  mutate(total_length_cm2 = if_else(is.na(`elena measurement error, add to length`), total_length_cm, (total_length_cm + `elena measurement error, add to length`) )) %>%
  select(-`elena measurement error, add to length`)

# test if this worked and remove the measurement error column
max(post_dat$total_length_cm2 - post_dat$total_length_cm, na.rm = TRUE)

# remove the uncorrected total_length_cm and rename the corrected one
post_dat <- 
  post_dat %>%
  select(-total_length_cm) %>%
  rename(total_length_cm = total_length_cm2)

# reorder and rename the columns where necessary
names(post_dat)

post_dat <- 
  post_dat %>%
  select(-date) %>%
  rename(date_end = date_corrected)

# subset out the basic data
final_dat <- 
  post_dat %>%
  select(date_end, site_code, hor_pos, depth_treatment, tile_id, plant_no,
         plant_id, binomial_code, wet_weight_g, total_length_cm, brittleness,
         epiphyte_wet_weight_g, person_photo, person_measure, person_writing, Obs) %>%
  rename(final_wet_weight_g = wet_weight_g, final_length_cm = total_length_cm,
         final_observer_photo = person_photo, 
         final_observer_measure = person_measure, 
         final_observer_writing = person_writing, final_notes = Obs)

# here, we need to add the area measurements
final_dat

# subset out the trait_data
# this will also include area measurements!
names(post_dat)

bt <- 
  post_dat %>%
  select(starts_with("blade_thickness"))

blt <- 
  post_dat %>%
  select(starts_with("bladder_thickness"))

mdt <- 
  post_dat %>%
  select(starts_with("midrib"))

trait_dat <- 
  post_dat %>%
  select(date_end, site_code, hor_pos, depth_treatment, tile_id, plant_no, plant_id, binomial_code,
         number_of_bladders, number_receptacles, stipe_thickness_mm,
         contains("tray"))

# summarise blade thickness measurements
trait_dat$blade_thickness_mean <- apply(bt, 1, mean)
trait_dat$blade_thickness_cv <- apply(bt, 1, sd)/apply(bt, 1, mean)

# summarise bladder thickness measurements
trait_dat$bladder_thickness_mean <- apply(blt, 1, mean)
trait_dat$bladder_thickness_cv <- apply(blt, 1, sd)/apply(blt, 1, mean)

# summarise midrib thickness measurements
trait_dat$midrib_mean <- apply(mdt, 1, mean)
trait_dat$midrib_cv <- apply(mdt, 1, sd)/apply(mdt, 1, mean)

rm(bt, blt, mdt)

# check the trait data
View(trait_dat)

# remove the tray weights from the rest and blade measurements
trait_dat <- 
  trait_dat %>%
  mutate(dry_weight_blade_g = (dry_weight_g_blade_with_tray - tray_weight_blade_g) ) %>%
  mutate(dry_weight_total_g = ((dry_weight_g_rest_with_tray - tray_weight_rest_g) + dry_weight_blade_g) ) %>%
  select(-contains("tray"))

View(trait_dat)

### END
