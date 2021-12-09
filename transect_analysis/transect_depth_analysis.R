
# Project: Functional value of macroalgal biodiversity

# Title: Preliminary analysis of depth data to pick the experimental depths

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2020-06-1"
pkgs <- c("here", "dplyr", "readr", "ggplot2", "slider",
          "ggforce", "gghalves", "ggbeeswarm")
groundhog.library(pkgs, groundhog.day)

# load relevant functions
source(here("functions/function_plotting_theme.R"))

# import the raw transect data
tra_dat <- read_csv(file = here("preliminary_supporting_data/transect_data.csv"),
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

# View(depth_data)

# problems with the start of transect 2, remove positions 0 to 4
# we do not have a starting depth
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
# View(depth_out)

# join this back to the full dataset
tra_a <- 
  full_join(tra_dat, depth_out, by = c("date", "transect_id", "position")) %>%
  select(date, transect_id, site_code, time, position, water_level_cm, 
         depth_cm, depth_correct, depth_interpolated, binomial_code, length_cm, circum_cm, 
         field_observer, notes)
# View(tra_a)

# check the summary statistics
summary(tra_a)

# check for unique values, especially for the binomial codes
lapply(tra_a, function(x) unique(x))

# remove missing binomial codes and the missing values "", NA
tra_a <- 
  tra_a %>%
  filter( !(binomial_code %in% c("-9999", "") | is.na(binomial_code) | is.na(depth_interpolated) )  )
  
# work with the transect data
ggplot(data = tra_a,
       mapping = aes(x = binomial_code, y = depth_interpolated)) +
  geom_point()

transect_summary <- 
  tra_a %>% 
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

# View(transect_summary)


# load the raw allometric data
allo_dat <- read_csv(file = here("preliminary_supporting_data/sample_data_biomass_allometry.csv"),
                     col_types = c("ccccdcccccdcddddccdddccdccc"),
                     na = c("NA"))

# remove the missing values from allo_dat
allo_dat <- 
  allo_dat %>%
  filter(!is.na(depth_cm)) %>%
  filter(!is.na(water_level_cm)) %>%
  mutate(depth_correct = depth_cm + water_level_cm)
  

# plot out the depth distribution
allo_dat %>%
  ggplot(data = ., 
         mapping = aes(x = binomial_code, y = depth_correct)) +
  geom_point()

# get summary statistics
allo_summary <- 
  allo_dat %>%
  group_by(binomial_code) %>%
  summarise(mean_depth = mean(depth_correct, na.rm = TRUE),
            min_depth = min(depth_correct, na.rm = TRUE),
            max_depth = max(depth_correct, na.rm = TRUE),
            quant_20 = quantile(depth_correct, 0.20, na.rm = TRUE),
            quant_80 = quantile(depth_correct, 0.80, na.rm = TRUE))

# allo_dat %>%
  # filter(depth_correct > 10) %>%
  # View()

allo_summary$data_id <- "allometry_data"

allo_summary <- 
  allo_summary %>%
  select(data_id, binomial_code, mean_depth:quant_80)

# View(allo_summary)

# bind_rows(transect_summary, allo_summary) %>%
  # write_csv(x = ., file = "C:/Users/james/OneDrive/PhD_Gothenburg/Chapter_2_Fucus_landscape/datasheets_print/zonation_data.csv")


# plot the depth data for each species
names(allo_dat)
names(tra_a)

# bind these data together
all_depth <- 
  bind_rows(
    
    select(allo_dat, binomial_code, depth_correct ) %>%
      mutate(data_set = "allometry"),
    
    select(tra_a, binomial_code, depth_correct = depth_interpolated) %>%
      mutate(data_set = "transect")
    
  )


# change the order of the binomial codes
all_depth$binomial_code <- factor(all_depth$binomial_code, levels = c("fu_se", "as_no", "fu_ve", "fu_sp" ))
levels(all_depth$binomial_code) = c("F. serratus", "A. nodosum", "F. vesiculosus", "F. spiralis")

all_depth_summary <- 
  all_depth %>%
  group_by(binomial_code) %>%
  summarise(m_depth_correct = mean(depth_correct), 
            sd_depth_correct = sd(depth_correct), .groups = "drop") %>%
  mutate(upper = (m_depth_correct + sd_depth_correct),
         lower = (m_depth_correct - sd_depth_correct))

p1 <- 
  ggplot() +
  geom_errorbarh(data = all_depth_summary,
                mapping = aes(xmin = lower,
                              xmax = upper,
                              y = binomial_code, colour = binomial_code),
                height = 0.1) +
  geom_quasirandom(data = all_depth,
                   mapping = aes(x = depth_correct, y = binomial_code, colour = binomial_code),
                   groupOnX = FALSE, alpha = 0.2, shape = 16) +
  geom_point(data = all_depth_summary, 
             mapping = aes(x = m_depth_correct,
                           y = binomial_code, colour = binomial_code), size = 2) +
  geom_point(mapping = aes(x = sort(c(-5, -12, -28, -40)), y = rep(0.1, 4),
                           colour = all_depth_summary$binomial_code),
             shape = 73, size = 9) +
  scale_x_continuous(limits = c(-55, 18), 
                     breaks = seq(-55, 18, 10)) +
  scale_y_discrete() +
  scale_colour_viridis_d(option = "C") +
  theme_meta() +
  ylab(NULL) +
  xlab("Depth (cm)") +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0.5, size = 9),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10.5))

# make a folder to export the cleaned data
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

ggsave(filename = here("figures/fig_1.png"), p1, units = "cm", dpi = 450,
       width = 8, height = 6)

### END  
  