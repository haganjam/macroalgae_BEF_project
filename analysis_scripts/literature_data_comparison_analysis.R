
# Project: Tile experiment

# Title: Analyse additional data from the literature

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2022-01-01"
pkgs <- c("here", "dplyr", "readr", "tidyr", "ggplot2", "lubridate", "viridis", "vegan")
groundhog.library(pkgs, groundhog.day)

# load relevant functions
source(here("functions/function_plotting_theme.R"))


## Jena data

# load the Jena biomass data
jena_bio <- read_delim(url("https://ndownloader.figshare.com/files/5608847"), delim = ",")
head(jena_bio)
names(jena_bio)

# create a vector of species names
sp_names <- names(jena_bio[, 85:144])

# remove the first plots that were not sown with any species
jena_bio <- filter(jena_bio, !(sowndiv %in% c(0)) )

# remove species presence columns
jena_bio <- select(jena_bio, -all_of(paste0("p", sp_names)))

# create a season variable for jena_bio
unique(jena_bio$month)

jena_bio <- 
  jena_bio %>%
  mutate(season = if_else(month %in% c("May", "Jun"), "spring", "summer"))

# subset out the spring data only
jena_bio <- 
  jena_bio %>%
  filter(season == "spring")

# replace the NAs with zeros
jena_bio <- 
  jena_bio %>%
  mutate(across(.cols = all_of(sp_names), ~replace(., is.na(.), 0)))

# remove rows of the data where there are missing values i.e. -9999 values in the sp_names
jena_bio <- 
  jena_bio %>%
  filter_at(all_of(sp_names), all_vars(. >= 0 ) )

# take the first three sub-samples as not all plots have four sub-samples
unique(jena_bio$subsample)

jena_bio <- 
  jena_bio %>%
  filter(subsample %in% c(1, 2, 3))

# select out the relevant columns
jena_bio <- 
  jena_bio %>%
  select(plotcode, season, time, subsample, sowndiv, target.biomass, all_of(sp_names))

jena_bio <- 
  jena_bio %>%
  filter(time == max(time))

mono <- 
  jena_bio %>%
  filter(sowndiv == 1)

mono <- 
  mono %>%
  pivot_longer(cols = all_of(sp_names),
               names_to = "species",
               values_to = "mono_biomass") %>%
  filter(mono_biomass > 0) %>%
  select(species, mono_biomass) %>%
  group_by(species) %>%
  summarise(mono_biomass = mean(mono_biomass))

mix <- 
  jena_bio %>%
  filter(sowndiv > 1)

mix <- 
  mix %>%
  pivot_longer(cols = sp_names,
               names_to = "species",
               values_to = "biomass") %>%
  filter(biomass > 0) %>%
  mutate(RA = biomass/target.biomass) %>%
  select(species, biomass, RA) %>%
  group_by(species) %>%
  summarise(biomass = mean(biomass),
            RA_mean = mean(RA),
            RA_sd = sd(RA)/sqrt(n()),
            n = n())

# join the monoculture and mixture data
mono_mix <- 
  full_join(mix, mono) %>%
  filter(!is.na(mono_biomass))
mono_mix

# calculate the correlation coefficient
x.cor <- cor.test(mono_mix$biomass, mono_mix$mono_biomass)
x.cor <- paste("Pearson's r = ", paste(round(x.cor$conf.int[1], 3), "-", round(x.cor$conf.int[2], 2), sep = ""), sep = "")

p1 <- 
  ggplot(data = mono_mix,
         mapping = aes(x = mono_biomass, y = RA_mean)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = RA_mean - RA_sd, ymax = RA_mean + RA_sd)) +
  geom_smooth(method = "lm", alpha = 0.25, colour = "black") +
  ylab("Relative abundance in mixture") +
  xlab("Monoculture biomass") +
  annotate(geom = "text", label = x.cor, x = 240, y = 0.67) +
  theme_meta()
p1


## Gamfeldt et al.'s in prep. meta-database

# load the raw data
meta_dat_raw <- read_csv( url("https://ndownloader.figshare.com/files/22647539") )

# the raw data file has extra columns in the csv file structure which we remove
meta_dat_raw <- 
  meta_dat_raw %>% 
  select(-contains("...3"))

# based on our selection criteria, we removed certain data points from our original raw file

# remove 'mixture best' data points in Fridley (2003)
meta_dat_raw <- filter(meta_dat_raw, Mixture_treatment != "mixture_best")

# remove data from Fox (2002) because it is a pure resource manipulation
meta_dat_raw <- filter(meta_dat_raw, Reference != "Fox_2002")

# remove treatment manipulations that relied on disturbance
meta_dat_raw <- filter(meta_dat_raw, Env_type_1 != "disturbance")

# clean the raw data

# create a unique identifier for each experiment
meta_dat_raw <- mutate(meta_dat_raw, Experiment_ID = paste(Reference, Experiment_number, sep = "_"))

# translate ecosystem function values to positive if low value indicates high ecosystem function
meta_dat_raw <- 
  meta_dat_raw %>% 
  group_by(Experiment_ID) %>% 
  mutate(ef_min = min(Ecosystem_function_mean)) %>% 
  ungroup() %>%
  mutate(ef_min = if_else(ef_min < 0, (-ef_min), 0)) %>% 
  mutate(Ecosystem_function_mean = (Ecosystem_function_mean + ef_min)) %>%
  select(-ef_min)
head(meta_dat_raw)

# check the relationship between mixtures across environments in a few examples
exp.list <- unique(meta_dat_raw$Experiment_ID)
n <- 1

meta_dat_raw %>%
  filter(Mixture_treatment != "mixture") %>%
  filter(Experiment_ID == exp.list[n]) %>%
  ggplot(data = .,
       mapping = aes(x = Environment, y = Ecosystem_function_mean, colour = Mixture_ID)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# calculate the species specialisation index
ssi <- 
  meta_dat_raw %>%
  filter(Mixture_treatment == "monoculture") %>%
  group_by(Experiment_ID, Environment) %>%
  filter(Ecosystem_function_mean == max(Ecosystem_function_mean)) %>%
  ungroup() %>%
  group_by(Experiment_ID) %>%
  summarise(n.spp = length(unique(Mixture_ID)),
            n.env = length(unique(Environment)), .groups = "drop") %>%
  mutate(n.spp.prop = (n.spp/n.env) ) %>% 
  mutate(species.specialisation = (n.spp.prop - (1/n.env))/(1-(1/n.env)) ) %>%
  select(Experiment_ID, species.specialisation)

# plot the distribution of species specialisation indices across the experiments
p2 <- 
  ggplot(data = ssi,
       mapping = aes(x = species.specialisation)) +
  geom_histogram(bins = 20, colour = "black", fill = "black") +
  theme_classic() +
  xlab("Species specialisation index (0-1)") +
  ylab("N experiments (n = 26)") +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_x_continuous(breaks = round(unique(ssi$species.specialisation), 2)) +
  theme_meta()
p2

# combine figures p1 and p2
library(ggpubr)

p12 <- 
  ggarrange(p1, p2, ncol = 2, nrow = 1,
            labels = c("a", "b"),
            font.label = list(size = 11, color = "black", face = "plain"))


ggsave(filename = here("figures/fig_6.png"), width = 18, height = 8.5, p12, units = "cm")

### END
