
# Project: Tile experiment

# Title: Clean the experiment data from the initial measurements

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2", "lubridate")
groundhog.library(pkgs, groundhog.day)


library(here)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)


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
View(final_dat)

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
#View(trait_dat)

# remove the tray weights from the rest and blade measurements

trait_dat <- 
  trait_dat %>%
  mutate(dry_weight_blade_g = (dry_weight_g_blade_with_tray - tray_weight_blade_g) ) %>%
  mutate(dry_weight_rest_g = (dry_weight_g_rest_with_tray - tray_weight_rest_g)) %>%
  select(-contains("tray"))

# set the NAs for dry weight 0 -> for Ascophyllum, dry weight blade is not NA, sometimes no "rest" was measured
trait_dat$dry_weight_blade_g[is.na(trait_dat$dry_weight_blade_g)]=0
trait_dat$dry_weight_rest_g[is.na(trait_dat$dry_weight_rest_g)]=0

trait_dat$dry_weight_total_g = trait_dat$dry_weight_rest_g + trait_dat$dry_weight_blade_g
trait_dat = trait_dat %>% select(-dry_weight_rest_g)

#View(trait_dat)

# load the area data
library(readr)
library(stringr)
image_dat_post <- read_csv("experiment_data/tiles_image_analysis_after.csv")
View(image_dat_post)

# apply labeling scheme
image_dat_post$plant_id = image_dat_post$id
image_dat_post$site_code = str_sub(image_dat_post$plant_id,start = 1,end = 1)
image_dat_post$hor_pos  = str_sub(image_dat_post$plant_id,start = 2,end = 2)
image_dat_post$depth_treatment  = str_sub(image_dat_post$plant_id,start = 3,end = 3)
image_dat_post$tile_id = str_sub(image_dat_post$plant_id,start = 1,end = 3)

# add a column for final area (total)
final_blade = image_dat_post %>% filter(thallus_blade=="blade")
final_thallus = image_dat_post %>% filter(thallus_blade=="thallus")

final_thallus$final_area_cm2 = final_thallus$area_cm2
final_thallus$final_perimeter_cm = final_thallus$perimeter_cm
final_thallus = final_thallus %>% select(plant_id:final_perimeter_cm)

final_blade$final_blade_area_cm2 = final_blade$area_cm2
final_blade$final_blade_perimeter_cm = final_blade$perimeter_cm
final_blade = final_blade %>% select(plant_id:final_blade_perimeter_cm)


# add to final table
final_dat=left_join(final_dat,final_thallus,by= c("plant_id","site_code","hor_pos","depth_treatment","tile_id" ))
final_dat=left_join(final_dat,final_blade,by= c("plant_id","site_code","hor_pos","depth_treatment","tile_id" ))
rm(final_blade,final_thallus,image_dat_post)

# import the pre csv file
library(readr)
initial_data_clean <- read_csv("experiment_data/initial_data_clean.csv")[-1]

# merge everything up
initial_data_clean$plant_no = as.character(initial_data_clean$plant_no)
pre_post = left_join(initial_data_clean,final_dat,
                          by = c("site_code","hor_pos","depth_treatment",
                                 "tile_id","plant_id","binomial_code","plant_no")  )
rm(initial_data_clean,final_dat,post_dat)

#merge_trait_data
names(trait_dat)

analysis_data = left_join(pre_post,trait_dat,
                          by = c("site_code","hor_pos","depth_treatment",
                                  "tile_id","plant_id","binomial_code","plant_no"))


analysis_data = analysis_data %>% filter(!plant_id=="XDE2")

#create treatment column
analysis_data$depth_id=analysis_data$depth_treatment
analysis_data$depth_treatment[analysis_data$depth_id=="E"] = -5
analysis_data$depth_treatment[analysis_data$depth_id=="F"] = -12
analysis_data$depth_treatment[analysis_data$depth_id=="G"] = -28
analysis_data$depth_treatment[analysis_data$depth_id=="H"] = -40

table(analysis_data$depth_treatment)

#error correction for wrong construction of WAG and WCH, both ascophyllum
# during fieldwork, we noticed that we accidentally placed the tile labelled WAG at depth H
# likewise, we placed the tile labelled WCH at depth G
analysis_data$depth_treatment[analysis_data$tile_id=="WAG"] = "-40"
analysis_data$depth_treatment[analysis_data$tile_id=="WCH"] = "-28"


#VDH1 - impute wet weight
#create model with area
fuve_dat= analysis_data %>% filter(binomial_code=="fu_ve")
lm1=lm(fuve_dat$final_wet_weight_g[fuve_dat$plant_id!="VDH1"] ~ fuve_dat$final_area_cm2[fuve_dat$plant_id!="VDH1"])
summary(lm1)
coef(lm1)
#impute wront data
analysis_data$final_wet_weight_g[analysis_data$plant_id=="VDH1"]=as.numeric(coef(lm1)[1] + coef(lm1)[2]*analysis_data$final_area_cm2[analysis_data$plant_id=="VDH1"])
rm(lm1,fuve_dat)

##Fetch missing dates from time stamp data
timestamps= read_csv("experiment_data/transplant_image_timestamps.csv")

start_dates = timestamps %>% filter(time=="t0")
end_dates = timestamps %>% filter(time=="t1")

for(i in 1:length(analysis_data$date_start)) {
  
  #fetch start dates
  if(is.na(analysis_data$date_start[i])){
    date_id=analysis_data$plant_id[i]
    
    #paste date as dd-mm-yyyy
    analysis_data$date_start[i]=paste(start_dates$day[start_dates$id==date_id],
                                      "-", start_dates$month[start_dates$id==date_id],"-","2021",sep = "")
    rm(date_id)
  }
  #fetch end dates
  if(is.na(analysis_data$date_end.x[i])){
    date_id=analysis_data$plant_id[i]
    
    #paste date as dd_mm_yyyy
    #check if not available
    if(!is.na(analysis_data$final_wet_weight_g))
    analysis_data$date_end.x[i]=paste(end_dates$day[end_dates$id==date_id],
                                      "_", end_dates$month[end_dates$id==date_id],"_","2021",sep = "")
    rm(date_id)
  }
  
}

# output the cleaned csv file for the initial and final data
if(!dir.exists("analysis_data")){dir.create("analysis_data")}



# output this to (analysis_data)
write.csv(analysis_data,file = "analysis_data/analysis_data.csv")




### END
