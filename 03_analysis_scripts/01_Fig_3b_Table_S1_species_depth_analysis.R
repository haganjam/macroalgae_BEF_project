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
groundhog.library(pkgs, groundhog.day,tolerate.R.version='4.2.2')
lapply(pkgs, require, character.only = TRUE) #if groundhog does not work

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

# check the min and max variables
summary(all_depth)

# make the depth segments
segments <- data.frame(xstart = rep(0, 4),
                       xend = rep(0.20, 4),
                       depth = sort(c(-5, -12, -28, -40)),
                       binomial_code = all_depth_summary$binomial_code)

p1 <- 
  ggplot() +
  geom_errorbar(data = all_depth_summary,
                 mapping = aes(x = binomial_code, colour = binomial_code, 
                               ymin = lower,
                               ymax = upper),
                 width = 0.025) +
  geom_quasirandom(data = all_depth,
                   mapping = aes(x = binomial_code, y = depth_correct, colour = binomial_code),
                   alpha = 0.2, shape = 16, width = 0.2) +
  geom_point(data = all_depth_summary, 
             mapping = aes(x = binomial_code,
                           y = m_depth_correct, colour = binomial_code), size = 2) +
  geom_segment(data = segments,
               mapping = aes(x = xstart, xend = xend,
                             y = depth, yend = depth, colour = binomial_code),
               size = 1.7) +
  scale_y_continuous(limits = c(-55, 18), 
                     breaks = seq(-50, 18, 10)) +
  scale_x_discrete() +
  scale_colour_manual(values = c("#0c1787","#9c259f", "#ec7853","#fadb25"))+
  theme_meta() +
  xlab(NULL) +
  ylab("Depth (cm)") +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = .8, size = 9,face = "italic",angle = 30),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 9))
plot(p1)

ggsave(filename = here("figures/Fig_2b.pdf"), p1, units = "cm", dpi = 400,
       width = 9, height = 9)

### END  
