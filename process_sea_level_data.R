
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the sea-level data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)

# check the loaded packages for their correct versions
sessionInfo()

# download the raw sea level data from ResearchBox: https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX
# save this into a folder called sea_level_data

# make a folder to export the cleaned data
if(! dir.exists(here("sea_level_data"))){
  print("make a folder called sea_level_data in the working directory and save the raw sea level data file into this directory")
}

# load the raw sea level data
sea_dat <- read_csv(here("sea_level_data/sea_level_data_raw_2015_2021.csv"))

# convert the UTC time into CEST to harmonise with the field measurements

# check the time zone names available
OlsonNames()

# with_tz() with tzone = "Europe/Berlin" harmonises to CET and CEST
head(sea_dat)

# convert time from UTC to CET
sea_dat <- 
  sea_dat %>%
  mutate(date_time_CET = with_tz(date_time_UTC, tzone = "Europe/Berlin")) %>%
  select(date_time_CET, water_level_cm, quality, measure_depth)

head(sea_dat)

# test the time and sea-level with the reported sea-levels from the field
sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-06-23 12:00:00", tz = "CET")) %>%
  View()

# we will need a more robust testhing mechanism once the data are all inputted
# in addition, we will need to decide whether to use the time or the water level

# explore the data a bit
summary(sea_dat)

sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-08-23 12:00:00", tz = "CET")) %>%
  summary()
  
sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-08-23 12:00:00", tz = "CET")) %>%
  pull(water_level_cm) %>%
  hist(.)

ggplot(data = sea_dat,
       mapping = aes(x = date_time_CET, y = water_level_cm)) +
  geom_line()

sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-08-23 12:00:00", tz = "CET")) %>%
  ggplot(data = .,
         mapping = aes(x = date_time_CET, y = water_level_cm)) +
  geom_line() +
  geom_hline(yintercept = -40)


# generate some ecologically meaningful variables from these time-series data
# given a particular water height

# we will have to define the depth of each point relative to the RH2000 standard

# e.g. water level -22, depth + 2

# before we do this, we need to calibrate the RH2000 to determine when something is or isn't submerged
# to do this, we utilise preliminary data that we collected on depths in the field along with
# recorded water levels





# we should do these calculations for the last 6 years but then also for
# the study period specifically

# use the sea level function to calculate these variables for each depth
source(here("functions/sea_level_function.R"))

# what are the depths of our tiles?
tile_depths <- tibble(depth_treatment = c("E", "F", "G", "H"),
                      depth_cm = c(-5, -12, -28, -40))
tile_depths

# generate the summary variables

# output_variable: 
# - "time_submerged_mins"
# - "time_exposed_mins"
# - "mean_length_submerged_mins"
# - "mean_length_exposed_mins"
# - "frequency_dessication_2_hours"
# - "top_5%_dessication_length_mins"

# make a vector of output variable names
output_names <- c("time_submerged_mins",
                 "time_exposed_mins",
                 "mean_length_submerged_mins",
                 "mean_length_exposed_mins",
                 "frequency_dessication_2_hours",
                 "top_5%_dessication_length_mins")

# last 5 years of data
# loop over each of these variables and add it to the tile_depths data
for(i in 1:length(output_names)) {
  
  x.in <- 
    sea_level_func(focal_depth = tile_depths$depth_cm,
                   sea_data = sea_dat,
                   date_col = "date_time_CET",
                   sea_level_col = "water_level_cm",
                   output_variable = output_names[i]
                   )
  
  tile_depths[[paste("year_5_", output_names[i], sep = "")]] <- x.in
  
}

# study period variables
for(i in 1:length(output_names)) {
  
  x.in <- 
    sea_level_func(focal_depth = tile_depths$depth_cm,
                   sea_data = sea_dat,
                   date_col = "date_time_CET",
                   start_date = as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
                   end_date = as.POSIXct("2021-08-01 11:00:00", tz = "CET"),
                   sea_level_col = "water_level_cm",
                   output_variable = output_names[i]
    )
  
  tile_depths[[paste("study_", output_names[i], sep = "")]] <- x.in
  
}

View(tile_depths)

# output these cleaned files into an analysis data folder

# make a folder to export the cleaned data
if(! dir.exists(here("analysis_data"))){
  dir.create(here("analysis_data"))
}

# output a cleaned .csv file of tile_depths

# output a cleaned .csv file of the sea_dat data file for plotting

### END
