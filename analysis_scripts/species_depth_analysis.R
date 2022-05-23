
# Project: Functional value of macroalgal biodiversity

# Title: Analyse the depth data from the transects and allometry data collections

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2022-01-01"
pkgs <- c("here", "dplyr", "readr", "ggplot2", "slider",
          "ggforce", "gghalves", "ggbeeswarm")
groundhog.library(pkgs, groundhog.day)

# load relevant functions
source(here("functions/function_plotting_theme.R"))

# load in the cleaned species depth data
all_depth <- read_csv(file = here("analysis_data/species_depth_data.csv"))

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
  xlab("depth (cm)") +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0.5, size = 9),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10.5))
p1

# make a folder to export the cleaned data
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

ggsave(filename = here("figures/fig_1.png"), p1, units = "cm", dpi = 450,
       width = 8, height = 6)

### END  
