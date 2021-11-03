
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the sea-level data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)

# check the loaded packages for their correct versions
sessionInfo()

# load the plotting theme
# source(here())

# make a folder to export the cleaned data
if(! dir.exists(here("analysis_data"))){
  dir.create(here("analysis_data"))
}

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

# use the preliminary supporting data to test whether RH2000 water levels taken
# using the Viva app in the field correspond to the sea_level_data we have

# download two files from from ResearchBox: https://researchbox.org/435&PEER_REVIEW_passcode=ECOTGX
# these files can be found under the preliminary_supporting_data section:
# 1. sample_data_biomass_allometry.csv
# 2. transect_data.csv

# save this into a folder on your computer called: preliminary_supporting_data

# load the biomass allometry data
allo_dat <- read_csv(file = here("preliminary_supporting_data/sample_data_biomass_allometry.csv"),
                     col_types = list(sample_id = col_character()))
str(allo_dat)
head(allo_dat)

# for this, we need dates, times and water levels so we extract these columns as complete cases
allo_dat <- allo_dat[complete.cases(allo_dat[, c("date", "time", "water_level_cm")]), ]
unique(allo_dat$date)
unique(allo_dat$time)

# add a data_set identifier
allo_dat$data_set <- "allometry"

# get those columns
allo_sub <- 
  allo_dat %>%
  select(data_set, date, time, water_level_cm)
# rm(allo_dat)

# load the transect data
tra_dat <- read_csv(file = here("preliminary_supporting_data/transect_data.csv"))
str(tra_dat)
head(tra_dat)

# for this, we need dates, times and water levels so we extract these columns as complete cases
tra_dat <- tra_dat[complete.cases(tra_dat[, c("date", "time", "water_level_cm")]), ]

# get an identifier for the dataset
tra_dat$data_set <- "transect"

# get those columns
tra_sub <- 
  tra_dat %>%
  select(data_set, date, time, water_level_cm)
# rm(tra_dat)

# bind these data together
pre_dat <- bind_rows(allo_sub, tra_sub)
head(pre_dat)

# check that the time variables are all correct
x <- grepl(pattern = "[0-9]{2}[h][0-9]{2}", pre_dat$time)
any(x == FALSE)

# correct these mistakes
pre_dat[!(x), ]$time <- c("09h52", "09h57", "10h53")

# check that the time variables are all correct
x <- grepl(pattern = "[0-9]{2}[h][0-9]{2}", pre_dat$time)
any(x == FALSE)

# get the data and time into the correct format
x <- gsub(pattern = "_", replacement = "-", pre_dat$date)
y <- paste(gsub(pattern = "h", replacement = ":", pre_dat$time), "00", sep = ":")
z <- paste(x, y, sep = " ")

pre_dat$date_time_CET <- as.POSIXct(z, tz="CET")
rm(x, y, z)

# select the correct columns
pre_dat <- 
  pre_dat %>%
  select(data_set, date_time_CET, water_level_cm_viva = water_level_cm)
head(pre_dat)

# filter the sea_dat by the dates where have data for
lev_comp <- inner_join(sea_dat, pre_dat, by = "date_time_CET" )
head(lev_comp)
nrow(lev_comp)
nrow(pre_dat)

ggplot(data = lev_comp, 
       mapping = aes(x = water_level_cm, y = water_level_cm_viva)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-22.5, 31.5)) +
  scale_y_continuous(limits = c(-22.5, 31.5)) +
  theme_classic()

cor.test(lev_comp$water_level_cm, lev_comp$water_level_cm_viva, method = "pearson")

# test the mean absolute difference between the water-levels
y <- abs(lev_comp$water_level_cm - lev_comp$water_level_cm_viva)
mean(y)
sd(y)
hist(y)

# remove the very large outliers which are unlikely to be a problem for
# the tile experiment (but should be checked for Merle's data)
lev_sub <- 
  lev_comp %>% 
  filter(water_level_cm_viva < 20)

ggplot(data = lev_sub, 
       mapping = aes(x = water_level_cm, y = water_level_cm_viva)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
  theme_classic()

cor.test(lev_sub$water_level_cm, lev_sub$water_level_cm_viva, method = "pearson")

# test the mean absolute difference between the water-levels
y <- abs(lev_sub$water_level_cm - lev_sub$water_level_cm_viva)
mean(y)
sd(y)
hist(y)

# test if the values we recorded are particularly extreme
range(pre_dat$water_level_cm_viva)
range(sea_dat$water_level_cm)

# what do we need to do here?
# we essentially need to correct this somehow

# write a .csv file out so that we can model this and try to correct it
write_csv(x = lev_comp, path = here("analysis_data/sea_level_viva_calibration_data.csv"))


### explore the sea-level data

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

# how to do this?

# we will have to define the depth of each point relative to the RH2000 standard
# e.g. water level -22, depth + 2

# given this depth correction i.e. we can use the following formula:
# depth in relation to the RH2000 = RH2000 water level + depth

# it follows logically that we can calculate depth with
# depth in relation to the RH2000 and RH2000 water level as:
# depth = depth in relation to the RH2000 - RH2000 water level

# this means that for each tile height, we can derive time-series
# where we know a tile was above or below water

# we do these calculations over two times periodss:

# 1. the last 6 years
# 2. the study period specifically

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

# output a cleaned .csv file of tile_depths

# output a cleaned .csv file of the sea_dat data file for plotting

### END
