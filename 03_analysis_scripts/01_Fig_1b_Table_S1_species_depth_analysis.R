#'
#' @title: Analyse the depth distribution of the four focal species
#' 
#' @description: This scripts generates a graph of the depth distribution of the four
#' focal species used in the experiment (Fig. 2 in the manuscript) and a table with summary
#' statistics regarding the observed depths and experimental depths (Table S1).
#' 
#' @authors: James G. Hagan (james_hagan(at)outlook.com)
#' 

# load relevant libraries
require(here)

# list of packages of load
pkgs <- c("dplyr", "readr", "ggplot2", "ggbeeswarm")

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

# load relevant functions
source(here("01_functions/function_plotting_theme.R"))

# load in the cleaned species depth data
all_depth <- read_csv(file = here("analysis_data/species_depth_data.csv"))

# change the order of the binomial codes
all_depth$binomial_code <- factor(all_depth$binomial_code, levels = c("fu_sp","fu_ve", "as_no" , "fu_se" ))
levels(all_depth$binomial_code) = c( "F. spiralis", "F. vesiculosus","A. nodosum", "F. serratus")

all_depth_summary <- 
  all_depth %>%
  group_by(binomial_code) %>%
  summarise(n = n(), 
            m_depth_correct = mean(depth_correct), 
            sd_depth_correct = sd(depth_correct),
            median_depth_correct = median(depth_correct), 
            se = sd_depth_correct/sqrt(n), .groups = "drop") %>%
  mutate(t_val = qt(p = 0.05, df = n)) %>%
  mutate(upper = (m_depth_correct + sd_depth_correct),
         lower = (m_depth_correct - sd_depth_correct),
         upper_ci = (m_depth_correct + se*t_val),
         lower_ci = (m_depth_correct - se*t_val))

# Table S1
#View(all_depth_summary)

write.csv(select(all_depth_summary,
                 binomial_code,
                 n,
                 m_depth_correct,
                 sd_depth_correct,
                 median_depth_correct,
                 ),
          here("figures/Table_S1_depth_dist_summary.csv"))

# check these confidence intervals
all_depth %>%
  filter(binomial_code == "F. vesiculosus") %>%
  pull(depth_correct) %>%
  t.test(x = .)

# check the min and max variables
summary(all_depth)

# make the depth segments
segments <- data.frame(xstart = rep(0, 4),
                       xend = rep(0.3, 4),
                       depth = (c(-5, -12, -28, -40)),
                       binomial_code = all_depth_summary$binomial_code)

p1 <- 
  ggplot() +
  geom_errorbar(data = all_depth_summary,
                mapping = aes(x = binomial_code, colour = binomial_code, 
                              ymin = lower,
                              ymax = upper),
                width = 0.05) +
  geom_quasirandom(data = all_depth,
                   mapping = aes(x = binomial_code, y = depth_correct, colour = binomial_code),
                   alpha = 0.3, shape = 1, width = 0.2) +
  geom_point(data = all_depth_summary, 
             mapping = aes(x = binomial_code,
                           y = m_depth_correct, colour = binomial_code), 
             size = 2.75, shape = 18) +
  geom_segment(data = segments,
               mapping = aes(x = xstart, xend = xend,
                             y = depth, yend = depth, colour = binomial_code),
               size = 1.25) +
  scale_y_continuous(limits = c(-55, 18), 
                     breaks = seq(-50, 18, 10)) +
  scale_x_discrete() +
  scale_colour_manual(values = c("#D97E46", "#7A5414", "#AF994D", "#EAB20A"))+
  theme_meta() +
  xlab(NULL) +
  ylab("Depth (cm)") +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = .8, size = 9,face = "italic",angle = 30),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        panel.border = element_blank())
plot(p1)

ggsave(filename = here("figures/fig_1b.png"), p1, units = "cm", dpi = 450,
       width = 6.5, height = 9)

### END  
