
# Project: Tile experiment

# Title: Generate ecologically relevant variables from the sea-level data

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2022-07-17"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2", "lubridate", "viridis", "scales")
groundhog.library(pkgs, groundhog.day)

# check the loaded packages for their correct versions
sessionInfo()

if(! dir.exists(here("figures"))){
  print("make a folder called experiment_data in the working directory and save the figures, see README for details")
}

# load the plotting theme
source(here("functions/function_plotting_theme.R"))

# load the cleaned sea-level data
sea_dat <- read_csv(file = here("analysis_data/sea_level_data.csv"))
range(sea_dat$date_time_CET)
diff(range(sea_dat$date_time_CET))
2469.997*24*60/nrow(sea_dat)
head(sea_dat)
str(sea_dat)
nrow(sea_dat)
sea_dat[, 1]

# make a plot for the last five years with the RH2000 depths as hlines

# what are the depths of our tiles?
tile_depths <- tibble(depth_treatment = c("E", "F", "G", "H"),
                      depth_cm = c(-5, -12, -28, -40))

p1 <- 
  sea_dat %>%
  filter(date_time_CET > as.POSIXct("2021-06-23 11:00:00", tz = "CET"),
         date_time_CET < as.POSIXct("2021-08-01 11:00:00", tz = "CET")) %>%
  ggplot(data = ., 
         mapping = aes(x = date_time_CET, y = water_level_cm)) +
  geom_line(alpha = 0.7, size = 0.3) +
  geom_hline(data = tile_depths,
             mapping = aes(yintercept = depth_cm, colour = depth_treatment),
             size = 1) +
  scale_colour_manual(values = c("#fadb25","#ec7853","#9c259f","#0c1787"))+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  theme_meta() +
  ylab("Water depth (cm)") +
  xlab("Date") +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0.5, size = 9),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10.5),
        axis.title.y = element_text(size = 10.5))


cols <- c("#fadb25","#ec7853","#9c259f","#0c1787")
hist_out <- vector("list", length = length(cols))
labels <- c("F. spiralis", "F. vesiculosus", "A. nodosum", "F. serratus")
for ( j in seq_along(hist_out) ) {
  
  df <- data.frame(sea_lev = tile_depths$depth_cm[j] - sea_dat$water_level_cm )
  
  hist_out[[j]] <- 
    ggplot(data = df,
           mapping = aes(x = sea_lev )) +
    geom_density(fill = cols[j], alpha = 0.75) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlab("Water depth (cm)") +
    scale_x_continuous(limits = c(-165, 125)) +
    ylab("Density") +
    ggtitle(labels[j]) +
    theme_meta() +
    theme(axis.text.y = element_blank(),
          plot.title = element_text(size = 12, hjust = 0.5))
  
}

# combine figures p1 and p2
library(ggpubr)

p2 <- 
  ggarrange(hist_out[[1]], hist_out[[2]], hist_out[[3]], hist_out[[4]], ncol = 4, nrow = 1,
            labels = c("b", "c", "d", "e"),
            font.label = list(size = 11, color = "black", face = "plain") )

p12 <- 
  ggarrange(p1, p2, 
            ncol = 1, nrow = 2,
            labels = c("a", " "),
            font.label = list(size = 11, color = "black", face = "plain")
            )
p12

ggsave(filename = here("figures/fig_S1.png"), plot = p12, 
       units = "cm", width = 20, height = 12, dpi = 300)

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

# we do these calculations over two times periods:

# 1. the last 6 years
# 2. the study period specifically

# use the sea level function to calculate these variables for each depth
source(here("functions/sea_level_function.R"))

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

### END
