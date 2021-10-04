
# Project: Functional value of macroalgal biodiversity

# Title: Species x depth gradient

# load relevant libraries
library(googlesheets4)
library(dplyr)
library(here)
library(slider)


# import the transect data

# import the raw data from the Google Sheet
tra_dat <- read_sheet("https://docs.google.com/spreadsheets/d/1H2_RtGusheNz6ibKzoCqLrEAYURznQuDS1bPCPnF_SA/edit#gid=0", sheet = "transect_samples",
                       col_types = c("ccccccccnnncnncc"),
                       na = c("NA"))
names(tra_dat)

# subset the data needed to interpolate the depth to the different points
# correct the depth by the water level from the nearby station
depth_data <- 
  tra_dat %>% 
  mutate(depth_correct = (water_level_cm + depth_cm) ) %>%
  select(date, transect_id, position, depth_correct) %>%
  distinct()

View(depth_data)

# problems with the start of transect 2, remove positions 0 to 4
# we do not have a starting depth...
depth_data <- 
  depth_data %>%
  filter( !(transect_id == 2 & position %in% 0:4) )

# split into a list
depth_list <- split(depth_data, depth_data$transect_id)

# loop over all transects
depth_out <- vector("list", length = length(depth_list))
for(i in 1:length(depth_list)) {
  
  # initialise a data.frame to work with
  df <- depth_list[[i]]
  
  # get the dividers
  dividers <- which(!is.na(df$depth_correct) )
  
  # duplicate middles for which the data are needed for multiple calculations
  if (length(dividers) > 2) {
    
    dups <- dividers[-c(1, length(dividers))]
    
    df <- 
      df[c(1:nrow(df), dups), ] %>%
      arrange(date, transect_id, position)
    
  }
  
  # add an ID column
  x <- vector("list", length = (length(dividers)-1))
  for(j in 1:(length(dividers)-1) ) {
    
    x[[j]] <- rep(j, ( (dividers[j+1] - dividers[j])+1 ) )
    
  }
  
  # write the ID column into the data.frame
  df$position_id <- unlist(x)
  
  # split data by the position ID
  df_list <- split(df, df$position_id)
  
  # for each block, interpolate the depth between the points
  int_depth <- 
    lapply(df_list, function(y){
      
      y %>%
        group_by(position_id) %>%
        mutate(y_int = first(depth_correct),
               d_depth = (last(depth_correct) - first(depth_correct)),
               d_position = last(10*position) - first(10*position)) %>%
        ungroup() %>%
        mutate(slope = d_depth/d_position) %>%
        select(-d_depth, -d_position) %>%
        mutate(distance = 0:(length(position)-1)*10 ) %>%
        mutate(depth_interpolated = y_int + ((distance)*slope) )  %>%
        select(-distance) %>%
        mutate(depth_interpolated = if_else(!is.na(depth_correct), depth_correct, depth_interpolated)) %>%
        select(-y_int, -slope, -position_id)
      
    }) %>%
    bind_rows(.,) %>%
    distinct()
  
  depth_out[[i]] <- int_depth
  
} 

# bind the list into a data.frame
depth_out <- bind_rows(depth_out)

# join this back to the full dataset
tra_a <- 
  full_join(tra_dat, depth_out, by = c("date", "transect_id", "position")) %>%
  select(date, transect_id, site_code, time, position, water_level_cm, 
         depth_cm, depth_correct, depth_interpolated, binomial_code, length_cm, circum_cm, 
         field_observer, notes)

View(tra_a)
  
# work with the transect data
library(ggplot2)
ggplot(data = tra_a %>% filter(binomial_code != -9999),
       mapping = aes(x = binomial_code, y = depth_interpolated)) +
  geom_point()

transect_summary <- 
  tra_a %>% 
  filter(binomial_code != -9999) %>%
  group_by(binomial_code) %>%
  summarise(mean_depth = mean(depth_interpolated, na.rm = TRUE),
            min_depth = min(depth_interpolated, na.rm = TRUE),
            max_depth = max(depth_interpolated, na.rm = TRUE),
            quant_20 = quantile(depth_interpolated, 0.20, na.rm = TRUE),
            quant_80 = quantile(depth_interpolated, 0.80, na.rm = TRUE))

transect_summary$data_id <- "transect"

transect_summary <- 
  transect_summary %>%
  select(data_id, binomial_code, mean_depth:quant_80)


# import raw allometric data from the Google Sheet
allo_dat <- read_sheet("https://docs.google.com/spreadsheets/d/167zCNjbmZ1PV5V1vZeGe9QcIrvhJ4diZ8q2rF9Ggry0/edit#gid=0", sheet = "Sheet1",
                       col_types = c("ccccdcccccdcddddccdddccdccc"),
                       na = c("NA"))

# plot out the depth distribution
allo_dat %>%
  filter(!is.na(depth_cm)) %>%
  filter(!is.na(water_level_cm)) %>%
  mutate(depth_correct = depth_cm + water_level_cm) %>%
  ggplot(data = ., 
         mapping = aes(x = binomial_code, y = depth_correct)) +
  geom_point()

# get summary statistics
allo_summary <- 
  allo_dat %>%
  filter(!is.na(depth_cm)) %>%
  filter(!is.na(water_level_cm)) %>%
  mutate(depth_correct = depth_cm + water_level_cm) %>%
  group_by(binomial_code) %>%
  summarise(mean_depth = mean(depth_correct, na.rm = TRUE),
            min_depth = min(depth_correct, na.rm = TRUE),
            max_depth = max(depth_correct, na.rm = TRUE),
            quant_20 = quantile(depth_correct, 0.20, na.rm = TRUE),
            quant_80 = quantile(depth_correct, 0.80, na.rm = TRUE))

allo_dat %>%
  filter(!is.na(depth_cm)) %>%
  filter(!is.na(water_level_cm)) %>%
  mutate(depth_correct = depth_cm + water_level_cm) %>%
  filter(depth_correct > 10) %>%
  View()

allo_summary$data_id <- "allometry_data"

allo_summary <- 
  allo_summary %>%
  select(data_id, binomial_code, mean_depth:quant_80)

library(readr)
bind_rows(transect_summary, allo_summary) %>%
  write_csv(x = ., file = "C:/Users/james/OneDrive/PhD_Gothenburg/Chapter_2_Fucus_landscape/datasheets_print/zonation_data.csv")






