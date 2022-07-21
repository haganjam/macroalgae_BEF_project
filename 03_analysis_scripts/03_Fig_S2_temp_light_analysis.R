#'
#' @title: Generate ecologically relevant variables from the sea-level data
#' 
#' @description: This scripts generates a set of ecologically meaningful variables from
#' the temperature and light loggers that we used. This is used to report summary statistics
#' in the Methods and materials and to generate Fig. S2 from the supplementary material.
#' 
#' @authors: James G. Hagan (james_hagan(at)outlook.com)
#' 

# load relevant libraries
require(here)
require(groundhog)

# load the relevant libraries using groundhog for package management
source(here("01_functions/get_groundhog_date.R"))
groundhog.day <- get_groundhog_date()
pkgs <- c("dplyr", "readr", "ggplot2", "lubridate", 
          "ggforce", "gghalves", "ggbeeswarm", "ggpubr")
groundhog.library(pkgs, groundhog.day)

# output the cleaned csv file into the analysis data folder
if(!dir.exists("analysis_data")){ 
  print("All cleaning scripts need to be run before this analysis can be run")
}

# read in the cleaned logger data
logvars <- read_rds(here("analysis_data/temp_light_logger_data.rds"))

# which variables would we like to generate?

# classic summary statistics for both temperature and light

# 1. maximum 
# 2. minimum 
# 3. range
# 4. mean
# 5. coefficient of variation

# biologically meaningful temperature variables

# 27 degrees is the upper tolerance for F. vesiculosus: https://www.sciencedirect.com/science/article/pii/S0022098115001276?casa_token=MVT95rCJjKIAAAAA:lFAFLcQy7v3NTwhVUV9lpSnPgykRdccV34sWmyvEySd4QdQV1Urlnt61ebUSMHY4402-vMpGhv4)
# Ascophyllum nodosum: https://www.sciencedirect.com/science/article/pii/S1385110105000365?casa_token=iUfVLnbMJ-4AAAAA:6x8Mso59wCSegKSVKVHFGLjFRNB65G4ykDsQ2hYCDBDGvi_C-gvfiRPo-1CBoIra38R0RHlHcXI
# Fucus serratus: https://www.sciencedirect.com/science/article/pii/S1874778713000871?casa_token=9bE-J_MvWoUAAAAA:pCq4lCynQ5V7sAjcERYNef2ksPQXj10j_G8R7roEAeejBO4oEo7qvQTqI_HUtAKPdUv3qYMo0sk
# Fucus spiralis: ?

# 1. total hours over 27 degrees
# 2. mean length of consecutive periods with temperatures over 27 degrees
# 3. longest consecutive period with temperatures over 27 degrees

logvars_summary <- 
  lapply(logvars, function(data) {
    
    df <- 
      data %>%
      mutate(exceed_27 = if_else(temperature_C > 27, 1, 0))
    
    # calculate periods of consecutive hours that exceeded 27 degrees
    x <- rle(df$exceed_27)
    y <- x$lengths[x$values == 1]
    
    z <- 
      df %>%
      group_by(site_code, water_level_treat) %>%
      summarise(hours_exceeding_27 = sum(exceed_27),
                con_hours_exceeding_27_mean = if_else(length(y) == 0, 0, as.numeric(mean(y)) ),
                con_hours_exceeding_27_max = if_else(length(y) == 0, 0, as.numeric(max(y)) ),
                mean_temp_C = mean(temperature_C),
                sd_temp_C = sd(temperature_C),
                max_temp_C = max(temperature_C),
                min_temp_C = min(temperature_C),
                range_temp_C = diff(range(temperature_C)),
                cv_temp_C = sd(temperature_C)/mean(temperature_C),
                mean_light_C = mean(intensity_lux),
                sd_light_C = sd(intensity_lux),
                max_light_C = max(intensity_lux),
                min_light_C = min(intensity_lux),
                range_light_C = diff(range(intensity_lux)),
                cv_light_C = sd(intensity_lux)/mean(intensity_lux))
    
    return(z)
    
  })

# bind this into a data.frame (warning can be ignored)
logvars_summary <- 
  logvars_summary %>%
  bind_rows(.) %>% 
  rename(depth_treatment = water_level_treat) %>%
  arrange(site_code, depth_treatment)

# view the summarised data
View(logvars_summary)

# summarise across sites
logvars_summary %>%
  group_by(depth_treatment) %>%
  summarise(hours_exceeding_27_m = mean(hours_exceeding_27, na.rm = TRUE),
            hours_exceeding_27_sd = sd(hours_exceeding_27, na.rm = TRUE),
            max_temp_C_m = mean(max_temp_C, na.rm = TRUE),
            max_temp_C_sd = sd(max_temp_C, na.rm = TRUE))


# plot the light and temperature information

# bind the different site, depth combinations into a large data.frame
logvars_df <- bind_rows(logvars, .id = "id")
head(logvars_df)

# make a datetime variable
logvars_df <- 
  logvars_df %>%
  mutate(date_time = ymd_hms(paste(date_corrected, time, sep = " ")))

# change the factors for plotting
logvars_df$water_level_treat <- factor(logvars_df$water_level_treat, levels = c( "H","G","F","E") )
levels(logvars_df$water_level_treat) <- c("-40 cm","-28 cm","-12 cm","-5 cm")

# make a temperature comparison plot
p1 <- 
  ggplot(data = logvars_df, 
       mapping = aes(x = water_level_treat, y = temperature_C, 
                     colour = water_level_treat,
                     group = id)) +
  geom_quasirandom(groupOnX = TRUE, alpha = 0.3, shape = 16, size = 0.5) +
  geom_boxplot(outlier.shape = NA, width = 0.3, notch = TRUE, 
               position = position_dodge(0.5)) +
  scale_colour_manual(values = c("#0c1787","#9c259f", "#ec7853","#fadb25"))+
  xlab("Water level treatment") +
  ylab("Temperature (°C)") +
  theme_meta() +
  theme(legend.position = "none")

# make a light comparison plot
p2 <- 
  ggplot(data = logvars_df, 
       mapping = aes(x = water_level_treat, y = intensity_lux, 
                     colour = water_level_treat,
                     group = id)) +
  geom_quasirandom(groupOnX = TRUE, alpha = 0.3, shape = 16, size = 0.5) +
  scale_colour_manual(values = c("#0c1787","#9c259f", "#ec7853","#fadb25"))+
  xlab("Water level treatment") +
  ylab("Light intensity (lux)") +
  theme_meta() +
  theme(legend.position = "none")

p12 <- 
  ggarrange(p1, p2, 
            ncol = 2, nrow = 1,
            labels = c("a", "b"),
            font.label = list(size = 11, color = "black", face = "plain")
  )
plot(p12)

ggsave(filename = here("figures/fig_S2.pdf"), p12, width = 20, height = 9, units = "cm",
       dpi = 300)

### END
