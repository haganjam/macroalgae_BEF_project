#'
#' @title: Clean the experiment data from the initial measurements
#' 
#' @description: Script to clean and output a cleaned version of the initial experiment
#' data. This uses a combination of the direct measurements and the measurements from the
#' processed images.
#' 
#' @authors: James G. Hagan (james_hagan(at)outlook.com) and Benedikt Schrofner-Brunner (bschrobru(at)gmail.com)
#' 

# load relevant libraries
require(here)
require(groundhog)

# load the relevant libraries using groundhog for package management
source(here("01_functions/get_groundhog_date.R"))
groundhog.day <- get_groundhog_date()

# it is possible that groundhog asks for this package
# require(stringi)

pkgs <- c("here", "dplyr", "readr", "lubridate", "stringr")

# use groundhog for package management? TRUE or FALSE
gh <- FALSE

if(gh) {
  
  # load the relevant libraries using groundhog for package management
  require(groundhog)
  source(here("01_functions/get_groundhog_date.R"))
  groundhog.day <- get_groundhog_date()
  groundhog.library(pkgs, groundhog.day)
  
} else {
  
  # load the packages manually
  sapply(pkgs, require, character.only = TRUE)
  
}


# check that the correct folder is present
if(! dir.exists(here("ResearchBox 435"))){
  print("download the ResearchBox contents and save it in the current directory")
}

# load the date_fixer function
source(here("01_functions/date_fixer.R"))

# load the raw initial data
init_dat <- read_csv(file = here("ResearchBox 435/Data/tile_experiment_data_plants_pre.csv"),
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
init_dat <- 
  init_dat %>%
  mutate(site_code = substr(tile_id, 1, 1),
         hor_pos = substr(tile_id, 2, 2),
         depth_treatment = substr(tile_id, 3, 3))

# check if these inputs are correct by matching them with the known replicates from the experiment
unique(init_dat$tile_id)
length(unique(init_dat$tile_id)) == (5*4*4)

unique(init_dat$site_code)
length(unique(init_dat$site_code)) == 5

unique(init_dat$hor_pos)
length(unique(init_dat$hor_pos)) == 4

unique(init_dat$depth_treatment)
length(unique(init_dat$depth_treatment)) == 4

# use the date_fixer function to fill in missing dates
init_dat <- 
  init_dat %>%
  group_by(tile_id) %>%
  mutate(date_corrected = date_fixer(date) )

# test if this assigned the dates properly  
# if this sums to zero, then all new assigned dates correspond to the old dates
init_dat %>%
  filter(!is.na(date)) %>%
  mutate(date_correct = if_else(date == date_corrected, 0, 1)) %>%
  pull(date_correct) %>%
  sum(.)

# check how many tiles have missing dates
init_dat %>%
  group_by(tile_id) %>%
  summarise(date_yes = first(date_corrected)) %>%
  View()

# how many rows still have NAs for dates
init_dat %>%
  filter(is.na(date_corrected) ) %>%
  nrow()

# most tiles do not have dates assigned
# to fix this, we will assign dates on a per site basis as this is how the experiment was conducted
init_dat %>%
  group_by(site_code) %>%
  summarise(date_corrected = (unique(date_corrected)))

# before this, we must get the dates into a common format
unique(init_dat$date[1])

fix_dates_df <- 
  lapply(init_dat$date_corrected, function(x) {
  
  if (!is.na(x)) {
    
    z <- strsplit(x, split = "\\.|\\:")[[1]]
    
  } else {
    
    z <- c(NA, NA, NA)
    
  }
  
  names(z) <- c("day", "month", "year")
  
  z
  
})

fix_dates_df <- bind_rows(fix_dates_df)

# what's wrong with the day variable? 1 must be converted to 01
unique(fix_dates_df$day)
fix_dates_df$day <- ifelse(fix_dates_df$day == "1", "01", fix_dates_df$day)

# what's wrong with the month variable? nothing
unique(fix_dates_df$month)

# what's wrong with the year variable? 21 must be converted to 2021
unique(fix_dates_df$year)
fix_dates_df$year <- ifelse(fix_dates_df$year == "21", "2021", fix_dates_df$year)

# make a new date_corrected variable
fix_dates_df$date_corrected2 <- ifelse(!is.na(fix_dates_df$day),
                                      paste(fix_dates_df$day, fix_dates_df$month, fix_dates_df$year, sep = "-"),
                                      NA)

# write this new date_corrected2 variable into the init_dat data
init_dat$date_corrected2 <- fix_dates_df$date_corrected2

# this should sum to zero
sum( is.na(init_dat$date_corrected) != is.na(init_dat$date_corrected2) )

# add dates based on site for tiles that do not have associated dates
# check if site (i.e. site_codes) have multiple dates
# no, but does not have a date
init_dat %>%
  group_by(site_code) %>%
  summarise(unique_dates = unique(date_corrected2) )

# check if imputing works by adding correct date on a per site basis
init_dat %>%
  group_by(site_code) %>%
  mutate(date_corr_imputed = date_fixer(date_corrected2) ) %>%
  group_by(site_code) %>%
  summarise(unique_dates = unique(date_corr_imputed ) )

# overwrite the init_dat data
init_dat <- 
  init_dat %>%
  group_by(site_code) %>%
  mutate(date_corr_imputed = date_fixer(date_corrected2) ) %>%
  ungroup()

# check lab notes to try and figure out the date for site_code: X (assuming other imputed dates are correct)

# subset the relevant columns after correcting the dates
names(init_dat)

init_dat <- 
  init_dat %>%
  select(date_corrected, date_corrected2, date_corr_imputed, site_code, hor_pos, depth_treatment,
         tile_id, plant_no, plant_id, origin_site_code, binomial_code, sex_fu_ve,
         initial_wet_weight_g, initial_length_cm, observers, notes)

# check if rows correspond to the correct number of plants
nrow(init_dat)
16*5*9

# there is a missing row
init_dat %>%
  group_by(tile_id) %>%
  summarise(plant_count = length(unique(plant_no))) %>%
  filter(plant_count != 9)

# yes, the missing row in tile XDE. Which plant_no is it?
z <- 
  init_dat %>%
  filter(tile_id == "XDE") %>%
  pull(plant_no)

c(1:9)[ !(1:9 %in% z) ] # 2

# XDE2 is missing which means it was not measured in this initial phase

# during fieldwork, we noticed that we accidentally placed the tile labelled WAG at depth H
# likewise, we placed the tile labelled WCH at depth G

# add information about this under the notes column
init_dat %>%
  filter(tile_id %in% c("WAG", "WCH")) %>%
  View()

# the raw data is unchanged but we will correct once we have joined the image data

# just use the imputed date for simplicity
init_dat <- 
  init_dat %>%
  select(-date_corrected, -date_corrected2) %>%
  rename(date_start = date_corr_imputed)

# read in the image processing data

# load the area data
image_dat_initial <- read_csv(here("ResearchBox 435/Data/tiles_image_analysis_before.csv.csv"))

# apply labeling scheme
image_dat_initial$plant_id <- image_dat_initial$id

# change names of the area and perimeter measurements
image_dat_initial$initial_area_cm2 <- image_dat_initial$area_cm2
image_dat_initial$initial_perimeter_cm <- image_dat_initial$perimeter_cm
image_dat_initial <- select(image_dat_initial, plant_id:initial_perimeter_cm)

# join the image data to the other initial data
init_dat <- left_join(image_dat_initial, init_dat, by= c("plant_id" ))

# output the cleaned csv file into the analysis data folder
if(!dir.exists("analysis_data")){ 
  dir.create("analysis_data") 
}

# write this into a .csv file
write_csv(init_dat, here("analysis_data/initial_data_clean.csv"))

### END
