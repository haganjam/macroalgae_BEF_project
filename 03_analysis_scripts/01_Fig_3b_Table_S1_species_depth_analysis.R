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
require(groundhog)

# load the relevant libraries using groundhog for package management
source(here("01_functions/get_groundhog_date.R"))
groundhog.day <- get_groundhog_date()
pkgs <- c("dplyr", "readr", "ggplot2", "ggbeeswarm")
groundhog.library(pkgs, groundhog.day)

# output the cleaned csv file into the analysis data folder
if(!dir.exists("analysis_data")){ 
  print("All cleaning scripts need to be run before this analysis can be run")
}

# check if a figure folder exists
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# load relevant functions
source(here("01_functions/function_plotting_theme.R"))

# load in the cleaned species depth data
all_depth <- read_csv(file = here("analysis_data/species_depth_data.csv"))

# change the order of the binomial codes
all_depth$binomial_code <- factor(all_depth$binomial_code, levels = c("fu_se", "as_no", "fu_ve", "fu_sp" ))
levels(all_depth$binomial_code) = c("F. serratus", "A. nodosum", "F. vesiculosus", "F. spiralis")

all_depth_summary <- 
  all_depth %>%
  group_by(binomial_code) %>%
  summarise(n = n(), 
            m_depth_correct = mean(depth_correct), 
            sd_depth_correct = sd(depth_correct),
            se = sd_depth_correct/sqrt(n), .groups = "drop") %>%
  mutate(t_val = qt(p = 0.05, df = n)) %>%
  mutate(upper = (m_depth_correct + sd_depth_correct),
         lower = (m_depth_correct - sd_depth_correct),
         upper_ci = (m_depth_correct + se*t_val),
         lower_ci = (m_depth_correct - se*t_val))

# Table S1
View(all_depth_summary)

# check these confidence intervals
all_depth %>%
  filter(binomial_code == "F. vesiculosus") %>%
  pull(depth_correct) %>%
  t.test(x = .)

p1 <- 
  ggplot() +
  geom_errorbarh(data = all_depth_summary,
                 mapping = aes(xmin = lower,
                               xmax = upper,
                               y = binomial_code, colour = binomial_code),
                 height = 0.1) +
  geom_quasirandom(data = all_depth,
                   mapping = aes(x = depth_correct, y = binomial_code, colour = binomial_code),
                   groupOnX = FALSE, alpha = 0.1, shape = 16) +
  geom_point(data = all_depth_summary, 
             mapping = aes(x = m_depth_correct,
                           y = binomial_code, colour = binomial_code), size = 2) +
  geom_point(mapping = aes(x = sort(c(-5, -12, -28, -40)), y = rep(0.1, 4),
                           colour = all_depth_summary$binomial_code),
             shape = 73, size = 9) +
  scale_x_continuous(limits = c(-55, 18), 
                     breaks = seq(-55, 18, 10)) +
  scale_y_discrete() +
  scale_colour_manual(values = c("#0c1787","#9c259f", "#ec7853","#fadb25"))+
  #scale_colour_viridis_d(option = "C") +
  theme_meta() +
  ylab(NULL) +
  xlab("Depth [cm]") +
  theme(legend.position = "none",
        axis.text.y = element_text(hjust = 0.5, size = 9,face = "italic"),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10.5))
plot(p1)

ggsave(filename = here("figures/Fig_3b.pdf"), p1, units = "cm", dpi = 450,
       width = 10, height = 8)

### END  
