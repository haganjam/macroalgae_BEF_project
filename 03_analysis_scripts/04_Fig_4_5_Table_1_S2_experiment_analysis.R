#'
#' @title: Analyse the relative growth rate data for species and depths
#' 
#' @description: This scripts analyses the effect of our treatments on growth and traits of
#' using data from the experiment. The script also produces Fig. 4, Fig. 5 and the
#' analysis reported in Table 1.
#' 
#' @authors: Benedikt Schrofner-Brunner (bschrobru(at)gmail.com) with minor edits from James G. Hagan (james_hagan(at)outlook.com)
#' 

# load relevant libraries
require(here)
require(groundhog)

# load the relevant libraries using groundhog for package management
source(here("01_functions/get_groundhog_date.R"))
groundhog.day <- get_groundhog_date()
pkgs <- c("here","readr","vegan","dplyr","lme4",
          "MuMIn","jtools","lmerTest","emmeans",
          "ggpubr", "ggfortify", "car", "ggdist", "ggbeeswarm","readr")
groundhog.library(pkgs, groundhog.day)

groundhog.library(pkgs, groundhog.day,tolerate.R.version='4.1.2')
#lapply(pkgs, require, character.only = TRUE) #if gorundhog does not work

# load relevant functions
source(here("01_functions/function_plotting_theme.R"))

# make sure the analysis data folder exists which contains the clean datasets
if(!dir.exists("analysis_data")){ 
  print("All cleaning scripts need to be run before this analysis can be run")
}

# check if a figure folder exists
if(! dir.exists(here("figures"))){
  dir.create(here("figures"))
}

# import cleaned dataset
analysis_data <- read_csv("analysis_data/experiment_analysis_data.csv")

# mortality # TRUE is that it was recovered, FALSE is that it was lost
table(!is.na(analysis_data$date_end.x), analysis_data$depth_treatment)
table(!is.na(analysis_data$date_end.x), analysis_data$binomial_code)

# depth had an effect on overall mortality
chisq.test(table(!is.na(analysis_data$date_end.x), analysis_data$depth_treatment))

# species had generally different mortality
chisq.test(table(!is.na(analysis_data$date_end.x), analysis_data$binomial_code))

mortality=as.data.frame(table(!is.na(analysis_data$date_end.x), analysis_data$binomial_code, analysis_data$depth_treatment, analysis_data$site_code))
names(mortality) = c("survived", "binomial_code","depth_treatment","site_code","freq")

# the current table has counts survived and died, filter to only died plants
mortality = mortality %>% filter(survived == F)

# run anova analysis for each factor
Anova(glm(freq ~ site_code,mortality,family=poisson))
Anova(glm(freq ~ depth_treatment,mortality,family=poisson))
Anova(glm(freq ~ binomial_code,mortality,family=poisson))

ggboxplot(y="freq",facet.by="binomial_code",x = "depth_treatment",jitter=T,data=mortality,add="jitter",shape=3)

#Anova without fucus serratus
Anova(glm(freq ~ binomial_code,mortality[mortality$binomial_code!="fu_se",],family=poisson))


# see if there is an interaction effect of depth and 
mod_mortality=(glm(freq ~ binomial_code*depth_treatment+site_code,mortality,family=poisson))

# plot(mod_mortality)

summary(mod_mortality)

mod_mortality = Anova(mod_mortality, type = 3)

# load the effect size library
library(effectsize)

# calculate the effect size
mod_mortality


mortality %>% group_by(binomial_code,depth_treatment) %>% summarise(mean=mean(freq))


# How many full tiles were lost?

mortality[mortality$freq==9,]

mortality[mortality$freq==8,]


table(mortality$freq)
hist(mortality$freq)



# remove outlier as it is almost certainly an incorrect measurement see end of script
analysis_data <- analysis_data[-493,] 

# calculate duration of the growth experiment
analysis_data$duration <- as.numeric(as.Date(analysis_data$date_end.x,"%d_%m_%Y")-as.Date(analysis_data$date_start,"%d-%m-%Y"))
table(analysis_data$duration)
summary(analysis_data$duration)

# calculate growth with weight, perimeter, area and length
analysis_data$growth_length_cm <- analysis_data$final_length_cm - analysis_data$initial_length_cm
analysis_data$growth_area_cm2 <- analysis_data$final_area_cm2 - analysis_data$initial_area_cm2
analysis_data$growth_wet_weight_g <- analysis_data$final_wet_weight_g - analysis_data$initial_wet_weight_g
analysis_data$growth_perimeter_cm <- analysis_data$final_perimeter_cm - analysis_data$initial_perimeter_cm

# plot histograms of each variable
hist(analysis_data$growth_length_cm)
hist(analysis_data$growth_area_cm2)
hist(analysis_data$growth_wet_weight_g)
hist(analysis_data$growth_perimeter_cm)


# examine the correlation between different measures of growth
cor(na.omit(analysis_data %>% select(contains("growth"))))
plot(na.omit(analysis_data %>% select(contains("growth"))))

plot(na.omit(analysis_data %>% select(dry_weight_total_g,final_area_cm2)))

# correct the species names to the full names
analysis_data$Species <- "Fucus spiralis"
analysis_data$Species[analysis_data$binomial_code == "fu_ve"] <- "Fucus vesiculosus"
analysis_data$Species[analysis_data$binomial_code == "as_no"] <- "Ascophyllum nodosum"
analysis_data$Species[analysis_data$binomial_code == "fu_se"] <- "Fucus serratus"

analysis_data$Species <- factor(analysis_data$Species,ordered = TRUE,
                               levels=c("Fucus serratus",
                                        "Ascophyllum nodosum",
                                        "Fucus vesiculosus",
                                        "Fucus spiralis"
                               ))





# calculating traits after the experiment after https://seaweedtraits.github.io/traits-db.html

# TDMC - Thallus Dry Matter Content (no units): obtained by dividing dry mass (g) by fresh mass (g)
analysis_data$trait_tdmc <- analysis_data$dry_weight_total_g/analysis_data$final_wet_weight_g
hist(analysis_data$trait_tdmc)

# thickness
analysis_data$trait_thickness <- analysis_data$blade_thickness_mean

# Ascophyllum thickness is represented by midrib thickness
analysis_data$trait_thickness[analysis_data$binomial_code=="as_no"] <- analysis_data$midrib_mean[analysis_data$binomial_code=="as_no"]
hist(analysis_data$trait_thickness)

# STA: Specific Thallus Area (mm2 g-1): obtained by dividing the area (mm2) of a sample by its dry mass (g)
analysis_data$trait_STA <- ( analysis_data$final_area_cm2 * 100 ) / analysis_data$dry_weight_total_g
hist(analysis_data$trait_STA)

# SBA Specific Blade Area
analysis_data$trait_SBA <- ( analysis_data$final_blade_area_cm2 * 100 ) / analysis_data$dry_weight_blade_g
hist(analysis_data$trait_SBA)

# SA:P - Surface Area to Perimeter ratio (no units): obtained by dividing the area (mm2) of a sample by its perimeter (mm)
analysis_data$trait_SAP <- ( analysis_data$final_area_cm2 * 100 ) / analysis_data$final_perimeter_cm
hist(analysis_data$trait_SAP)

# other traits:

# lift per DW - bladder volume (obtained by calculating the volume of a sphere from diameter (=thickness)) times bladder count per dry weight
analysis_data$lift <- (4/3)*pi* (analysis_data$bladder_thickness_mean / 2 ) *analysis_data$number_of_bladders
analysis_data$trait_float <- analysis_data$lift / analysis_data$dry_weight_total_g
hist(analysis_data$trait_float)
analysis_data$trait_float[is.na(analysis_data$trait_float)] <- 0

# growth percentages
analysis_data$growth_area_cm2_percent <- analysis_data$growth_area_cm2 / analysis_data$initial_area_cm2 *100
analysis_data$growth_length_cm_percent <- analysis_data$growth_length_cm / analysis_data$initial_length_cm  *100
analysis_data$growth_perimeter_cm_percent <- analysis_data$growth_perimeter_cm / analysis_data$initial_perimeter_cm *100
analysis_data$growth_wet_weight_g_percent <- analysis_data$growth_wet_weight_g / analysis_data$initial_wet_weight_g *100


# predict dry-weight before the experiment using a linear model

# fit a model of the dry weight in the final measurements using wet weight, area and species

#Create multiple models to predict dryweight
candidate_models_dw <- list(
  model1 = lm(dry_weight_total_g ~ final_area_cm2, data = analysis_data),
  model2 = lm(dry_weight_total_g ~ final_area_cm2 + Species, data = analysis_data),
  model3 = lm(dry_weight_total_g ~ final_area_cm2 * Species, data = analysis_data),
  model4 = lm(dry_weight_total_g ~ final_area_cm2 + final_wet_weight_g, data = analysis_data),
  model5 = lm(dry_weight_total_g ~ final_area_cm2 + final_wet_weight_g * Species, data = analysis_data),
  model6 = lm(dry_weight_total_g ~ final_wet_weight_g, data = analysis_data),
  model7 = lm(dry_weight_total_g ~ final_wet_weight_g + Species, data = analysis_data),
  model8 = lm(dry_weight_total_g ~ final_wet_weight_g * Species, data = analysis_data),
  model9 = lm(dry_weight_total_g ~ final_area_cm2 * Species + final_wet_weight_g * Species,data=analysis_data),
  model10 = lm(dry_weight_total_g ~ final_area_cm2 * Species * final_wet_weight_g,data=analysis_data))

#Compare multiple models to predict dryweight

model.comparison = rbind(broom::glance(candidate_models_dw$model1),
      broom::glance(candidate_models_dw$model2),
      broom::glance(candidate_models_dw$model3),
      broom::glance(candidate_models_dw$model4),
      broom::glance(candidate_models_dw$model5),
      broom::glance(candidate_models_dw$model6),
      broom::glance(candidate_models_dw$model7),
      broom::glance(candidate_models_dw$model8),
      broom::glance(candidate_models_dw$model9),
      broom::glance(candidate_models_dw$model10))

model.comparison = cbind(model=
  c("dry weight ~ area",
    "dry weight ~ area + Species",
    "dry weight ~ area * Species",
    "dry weight ~ area + wet weight",
    "dry weight ~ area + wet weight * Species",
    "dry weight ~ wet weight",
    "dry weight ~ wet weight + Species",
    "dry weight ~ wet weight * Species",
    "dry weight ~ area * Species + wet weight * Species",
    "dry weight ~ area * Species * wet weight"),
  model.comparison)

write.csv("model.comparison",here("figures/model.comparison.dw.prediction.csv"))

#Best Model (no 10) is used to predict dryweight

mod_dw <- lm(dry_weight_total_g ~ final_area_cm2 * Species * final_wet_weight_g,data=analysis_data)
summary(mod_dw)

# plot the model fit
plot(mod_dw$fitted.values, mod_dw$model$dry_weight_total_g)

dw_pred_data = cbind(predicted_dry_weight_g = mod_dw$fitted.values, dry_weight_g = mod_dw$model$dry_weight_total_g)

p_S_dry_weight_prediction <- ggplot(dw_pred_data, aes(x = dry_weight_g, y = predicted_dry_weight_g )) + 
  geom_point(size=1,alpha=0.5) +  
  geom_smooth(method="lm",se = F,size=.6,alpha=0.8,col = "black")+ 
  theme_meta()+
  xlab(expression("Dry weight (g)")) + 
  ylab("Predicted dry weight (g)")+
  xlim(c(0,15))+ylim(c(0,15))+
  annotate("text", x=1.2, y=14.5, label= expression(~r^{2}~"= 0.98"))

ggsave(filename = here("figures/fig_S_dry_weight_prediction.pdf"), plot = p_S_dry_weight_prediction, 
       units = "cm", width = 10, height = 10, dpi = 300)


# use this model to predict the dry weight before
pred_dw <- select(analysis_data, initial_area_cm2, initial_wet_weight_g, Species)
colnames(pred_dw) = c("final_area_cm2","final_wet_weight_g","Species")
predict(mod_dw, newdata=pred_dw)

# add the predicted values to the data.frame
analysis_data$dry_weight_total_g_before_predicted <- predict(mod_dw, newdata=pred_dw)

# calculate the dry weight increase from before to end of the experiment
analysis_data$dry_weight_total_g_increase <- analysis_data$dry_weight_total_g - analysis_data$dry_weight_total_g_before_predicted

# calculate relative increase over the whole experiment
analysis_data$dry_weight_total_g_relative_increase_total <- 100 * analysis_data$dry_weight_total_g_increase/analysis_data$dry_weight_total_g_before_predicted

# growth in percent per day - there were slightly differing durations
analysis_data$dry_weight_g_daily_relative_increase <- analysis_data$dry_weight_total_g_relative_increase_total/analysis_data$duration

# export a cleaned version of these data
analysis_data %>%
  select(plant_id, site_code, hor_pos, depth_treatment, tile_id, plant_no,
         binomial_code,
         trait_tdmc, trait_thickness, trait_STA, trait_SBA, trait_SAP, trait_float,
         dry_weight_g_daily_relative_increase) %>%
  rename(dry_weight_g_daily_change = dry_weight_g_daily_relative_increase) %>%
  write_csv("analysis_data/compensation_data.csv")

# calculate a summary table
table(is.na(analysis_data$dry_weight_g_daily_relative_increase))

# epiphyte wet weight per area
analysis_data$epiphyte_wet_weight_g_per_area <- analysis_data$epiphyte_wet_weight_g / analysis_data$final_area_cm2

######Summary Stats Growth######
analysis_data %>% group_by(Species) %>% summarise(mean=mean(dry_weight_total_g_increase,na.rm=T),
                                                  sd=sd(dry_weight_total_g_increase,na.rm=T),
                                                  percent = mean(dry_weight_total_g_relative_increase_total,na.rm=T))

analysis_data %>% group_by(Species,depth_treatment) %>% summarise(mean=mean(dry_weight_total_g_increase,na.rm=T),
                                                                  sd=sd(dry_weight_total_g_increase,na.rm=T),
                                                                  percent = mean(dry_weight_total_g_relative_increase_total,na.rm=T))


analysis_data %>% group_by(Species) %>% summarise(dry_mean=mean(dry_weight_total_g_before_predicted,na.rm=T),
                                                  dry_sd=sd(dry_weight_total_g_before_predicted,na.rm=T),
                                                  length_cm = mean(initial_length_cm,na.rm=T),
                                                  length_sd = sd(initial_length_cm,na.rm=T))


# summary table with initial values and growth per day

summary.data = data.frame(species = analysis_data$Species,
                          depth_treatment = analysis_data$depth_treatment,
                          initial_dryweight_predicted_g = analysis_data$dry_weight_total_g_before_predicted, #initial dry weight
                          dryweight_growth_g_per_day = (analysis_data$dry_weight_total_g - analysis_data$dry_weight_total_g_before_predicted)/analysis_data$duration,
                          initial_wetweight_g = analysis_data$initial_wet_weight_g, #wet weight
                          wetweight_g_per_day = analysis_data$growth_wet_weight_g/analysis_data$duration,
                          initial_area_cm2 = analysis_data$initial_area_cm2,
                          area_cm2_per_day = analysis_data$growth_area_cm2 / analysis_data$duration,
                          initial_max_length_cm = analysis_data$initial_length_cm,
                          max_length_cm_per_day = analysis_data$growth_length_cm / analysis_data$duration)

summary.data = na.omit(summary.data)

#create summary table by species and depth
summary.table_species_depth = summary.data %>%
  group_by(species, depth_treatment) %>%
  summarize(mean_initial_dryweight = mean(initial_dryweight_predicted_g),
            sd_initial_dryweight = sd(initial_dryweight_predicted_g),
            mean_dryweight_growth = mean(dryweight_growth_g_per_day),
            sd_dryweight_growth = sd(dryweight_growth_g_per_day),
            mean_initial_wetweight = mean(initial_wetweight_g),
            sd_initial_wetweight = sd(initial_wetweight_g),
            mean_wetweight_growth = mean(wetweight_g_per_day),
            sd_wetweight_growth = sd(wetweight_g_per_day),
            mean_initial_area = mean(initial_area_cm2),
            sd_initial_area = sd(initial_area_cm2),
            mean_area_growth = mean(area_cm2_per_day),
            sd_area_growth = sd(area_cm2_per_day),
            mean_initial_max_length = mean(initial_max_length_cm),
            sd_initial_max_length = sd(initial_max_length_cm),
            mean_max_length_growth = mean(max_length_cm_per_day),
            sd_max_length_growth = sd(max_length_cm_per_day))

#create summary table by 

summary.table_species = summary.data %>%
  group_by(species) %>%
  summarize(mean_initial_dryweight = mean(initial_dryweight_predicted_g),
            sd_initial_dryweight = sd(initial_dryweight_predicted_g),
            mean_dryweight_growth = mean(dryweight_growth_g_per_day),
            sd_dryweight_growth = sd(dryweight_growth_g_per_day),
            mean_initial_wetweight = mean(initial_wetweight_g),
            sd_initial_wetweight = sd(initial_wetweight_g),
            mean_wetweight_growth = mean(wetweight_g_per_day),
            sd_wetweight_growth = sd(wetweight_g_per_day),
            mean_initial_area = mean(initial_area_cm2),
            sd_initial_area = sd(initial_area_cm2),
            mean_area_growth = mean(area_cm2_per_day),
            sd_area_growth = sd(area_cm2_per_day),
            mean_initial_max_length = mean(initial_max_length_cm),
            sd_initial_max_length = sd(initial_max_length_cm),
            mean_max_length_growth = mean(max_length_cm_per_day),
            sd_max_length_growth = sd(max_length_cm_per_day))


summary.table <- bind_rows(summary.table_species, summary.table_species_depth)
summary.table$depth_treatment[is.na(summary.table$depth_treatment)] = "Total"

summary.table <- summary.table %>%
  arrange(factor(species, levels = c("Fucus spiralis", "Fucus vesiculosus", "Ascophyllum nodosum", "Fucus serratus")),
          factor(depth_treatment, levels = c("-5","-12","-28","-40","Total"))) %>%
  select(species,depth_treatment, everything()) %>%
  mutate_at(vars(contains("initial_")), round, 1) %>%
  mutate_at(vars(ends_with("_growth")), round, 3)

summary.table = summary.table %>% 
mutate(initial_dryweight = paste(as.character(format(mean_initial_dryweight,nsmall=1))," ± ", as.character(format(sd_initial_dryweight,nsmall=1))),
       dryweight_growth = paste(as.character(format(mean_dryweight_growth, nsmall = 3)), " ± ", as.character(format(sd_dryweight_growth, nsmall = 3))),
       initial_wetweight = paste(as.character(format(mean_initial_wetweight, nsmall = 1)), " ± ", as.character(format(sd_initial_wetweight, nsmall = 1))),
       wetweight_growth = paste(as.character(format(mean_wetweight_growth, nsmall = 3)), " ± ", as.character(format(sd_wetweight_growth, nsmall = 3))),
       initial_area = paste(as.character(format(mean_initial_area, nsmall = 1)), " ± ", as.character(format(sd_initial_area, nsmall = 1))),
       area_growth = paste(as.character(format(mean_area_growth, nsmall = 3)), " ± ", as.character(format(sd_area_growth, nsmall = 3))),
       initial_max_length = paste(as.character(format(mean_initial_max_length, nsmall = 1)), " ± ", as.character(format(sd_initial_max_length, nsmall = 1))),
       max_length_growth = paste(as.character(format(mean_max_length_growth, nsmall = 3)), " ± ", as.character(format(sd_max_length_growth, nsmall = 3))))

summary.table <- summary.table %>%
  select(-starts_with("mean"), -starts_with("sd"))

colnames(summary.table) <- c("Species","Depth treatment (cm)","Initial dryweight ± SD (g)","Dryweight growth ± SD (g day-1)", "Initial wetweight ± SD (g)",  
                             "Wetweight growth ± SD (g day-1)", "Initial area ± SD (cm²)", "Area growth ± SD (cm² day-1)",
                             "Initial maximum length ± SD (cm)", "Maximum length growth ± SD (cm day-1)")


write.csv(summary.table, here("figures/table_S5.csv"))
# PCA traits

# run a PCA on the traits of the different species
pca_data <- 
  analysis_data %>% 
  select(Species, depth_treatment, site_code, contains("trait"))

# change depth treatment to factor
pca_data$depth_treatment <- as.factor(pca_data$depth_treatment)

# deselect SBA as this is highly correlated with STA
pca_data <- select(pca_data, -trait_SBA) 

# rename the columns
colnames(pca_data) <- c("Species", "depth","site", "TDMC", "thickness",
                      "STA", "SA:P" , "Pneumatocysts")

# remove any missing values in the data
pca_data <- na.omit(pca_data)

# plot a histogram of each of the different traits by species
gghistogram(pca_data,x="TDMC",facet.by = "Species")
gghistogram(pca_data,x="thickness",facet.by = "Species")
gghistogram(pca_data,x="STA",facet.by = "Species")
gghistogram(pca_data,x="SA:P",facet.by = "Species")
gghistogram(pca_data,x="Pneumatocysts",facet.by = "Species")

# run a PCA on the traits
pca_res <- prcomp(x = pca_data[-c(1,2,3)], scale = TRUE)

# plot the PCA
pca_plot <- 
  autoplot(pca_res, data = pca_data, #shape = 'depth',
                    size=3,
                    loadings = TRUE, loadings.colour = 'black',
                    loadings.label = TRUE, loadings.label.size = 3.5,loadings.label.vjust=c(1.2,-.8,-1,2,-.5),loadings.label.hjust=c(1,.6,0,1,.5),
                    loadings.label.colour="black",colour="Species",legend.position="none",alpha=0.5) + 
  theme_meta() + 
  scale_colour_manual(values = c("#0c1787","#9c259f", "#ec7853","#fadb25")) +
  theme(legend.position = "none")

# plot the PCA
ggsave(filename = here("figures/fig_S_PCA.pdf"), plot = pca_plot, 
       units = "cm", width = 15, height = 14, dpi = 300)
plot(pca_plot)

# check the screeplot and the summary statistics
screeplot(pca_res)
summary(pca_res)


# PERMANOVA on the Traits 

# permanova on traits to test the effect of species and depth on traits

# set the seed to make the analysis reproducible
set.seed(1)

# note that the analysis can take a long time
# set run_permanova <- TRUE if you want to run it
run_permanova <- FALSE

if (run_permanova) {
  
  adonis2(as.matrix(scale(pca_data[-c(1,2,3)])) ~ pca_data$Species*pca_data$depth, method = "euclidean",
          permutations = 99999)
  
}

# plot comparisons of the traits in a univariate
ggboxplot(analysis_data,y="trait_tdmc",x = "binomial_code")
ggboxplot(analysis_data,y="trait_STA",x = "binomial_code")
ggboxplot(analysis_data,y="trait_float",x = "binomial_code")
ggboxplot(analysis_data,y="trait_thickness",x = "binomial_code")
ggboxplot(analysis_data,y="trait_SAP",x = "binomial_code")


# analysis of growth (function) in different depth zones

# linear mixed effects model
model.growth.all.species <- lmer(analysis_data$dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = analysis_data)

# combine output into a table
table_1 <- cbind(anova(model.growth.all.species),
                 r.squaredGLMM(model.growth.all.species), 
                 N = length(resid(model.growth.all.species)), 
                 model="all species"
                )

# check the density plot
densityPlot(resid(model.growth.all.species))

# examine the model output
summ(model.growth.all.species)
anova(model.growth.all.species)

# calculate emmeans to compare groups directly
emm <- emmeans(model.growth.all.species, list(pairwise ~ factor(depth_treatment)/Species), adjust = "tukey")

# make a table of the emmeans output
emm <- as.data.frame(emm$`emmeans of depth_treatment, Species`)

# convert the depth treatment to a factor
emm$depth_treatment <- factor(emm$depth_treatment, levels = c(-5, -12, -28, -40))

# Table S2: write this output from emmeans into a .csv file
write_csv(emm, here("figures/table_S2.csv") )


# post-hoc analysis for each species: fit an individual model to each species

# Fucus serratus
model_fu_se <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id), 
                    data = filter(analysis_data, binomial_code == "fu_se"))
anova(model_fu_se)
densityPlot(resid(model_fu_se))
summ(model_fu_se)

# pull the results into a data.frame
table_1_fu_se <- cbind(anova(model_fu_se), r.squaredGLMM(model_fu_se), 
                       N = length(resid(model_fu_se)), model = "fu_se")

# Fucus spiralis
model_fu_sp <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),
                    data = filter(analysis_data, binomial_code == "fu_sp"))
anova(model_fu_sp)
densityPlot(resid(model_fu_sp))
summ(model_fu_sp)

emm_fusp <- emmeans(model_fu_sp, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")


# pull the results into a data.frame
table_1_fu_sp = cbind(anova(model_fu_sp), r.squaredGLMM(model_fu_sp), 
                      N = length(resid(model_fu_sp)), model = "fu_sp")

# Ascophyllum nodosum
model_as_no <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id), 
                    data = filter(analysis_data, binomial_code == "as_no"))
anova(model_as_no)
densityPlot(resid(model_as_no))
summ(model_as_no)

emm_as_no <- emmeans(model_as_no, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")


# pull the results into a data.frame
table_1_as_no = cbind(anova(model_as_no), r.squaredGLMM(model_as_no),
                      N = length(resid(model_as_no)), model = "as_no")


# Fucus vesiculosus
model_fu_ve <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),
                    data = filter(analysis_data, binomial_code == "fu_ve"))
anova(model_fu_ve)
densityPlot(resid(model_fu_ve))
summ(model_fu_ve)

# pull the results into a data.frame
table_1_fu_ve = cbind(anova(model_fu_ve), r.squaredGLMM(model_fu_ve), 
                      N = length(resid(model_fu_ve)), model = "fu_ve")

# Table 1: bind the combined model and all individual models into a table and export as a .csv
write_csv(rbind(table_1,table_1_fu_sp,table_1_fu_ve,table_1_as_no,table_1_fu_se),"figures/table_1.csv")


# Figure 4a-d

# set the species names
sp_names <- c("Fucus spiralis", "Fucus vesiculosus", "Ascophyllum nodosum", "Fucus serratus") 

# set the maximum and the minima of each graph
ymin <- -2.25
ymax <- 3.4
mid_point <- (ymin + ymax)/2
h <- (ymax - ymin)

# bar height list
sp_bar_height <- list(c(h, 0, 0, 0),
                      c(0, h, 0, 0),
                      c(0, 0, h, 0),
                      c(0, 0, 0, h))

# set-up the significance letters for each species
sp_sig <- list(c("A", "A", "A", "A"),
               c("A", "A", "A", "A"),
               c("AB", "A", "AC", "A"),
               c("B", "B", "A", "A"))

# set-up the xlabels
xlabs <- c("", "", "", "Depth treatment (cm)")

# set-up the colours
cols <- c("#CC6600", "#996600", "#666600", "#CC9900")

plot_list <- vector("list", length = length(sp_names))
# for(i in 1:length(sp_names)) {
  i <- 1
  plot_df <- 
    analysis_data  %>% 
    filter(Species == sp_names[i], !is.na(dry_weight_g_daily_relative_increase)) %>%
    mutate(depth_treatment = factor(depth_treatment, levels = c(-5, -12, -28, -40)))
  
  # add a table with the significance letters
  plot_df_sig <- 
    tibble(depth_treatment = factor(c(-5, -12, -28, -40), levels = c(-5, -12, -28, -40)),
           significance = sp_sig[[i]],
           DW_height = sp_bar_height[[i]])
  
  # get a data.frame with the confidence intervals
  plot_df_ci <- filter(emm, Species == sp_names[i])
  
  p1 <- 
    ggplot() + 
    geom_tile(data = plot_df_sig,
              mapping = aes(x = depth_treatment, y = mid_point, 
                            width = 0.75, height = DW_height),
              alpha = 0.05) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") + 
    ggdist::stat_halfeye(data = plot_df, 
                         mapping = aes(x = depth_treatment, y = dry_weight_g_daily_relative_increase), 
                         adjust = 0.5, width = 0.3, .width = 0, 
                         justification = -0.3, point_colour = NA, fill = cols[i],
                         alpha = 0.75) + 
    gghalves::geom_half_point(data = plot_df, 
                              mapping = aes(factor(depth_treatment), dry_weight_g_daily_relative_increase), 
                              side = "l", range_scale = 0.4,
                              fill = cols[i], colour = cols[i],
                              alpha = 0.75) +
    geom_errorbar(data = plot_df_ci,
                  mapping = aes(x = depth_treatment, 
                                ymin = lower.CL,
                                ymax = upper.CL), 
                  width = 0, colour = "black", size = 0.45) +
    geom_point(data = plot_df_ci,
               mapping = aes(x = depth_treatment, y = emmean), 
               shape = 18, colour = "black", size = 2.5) +
    geom_text(data = plot_df_sig,
              mapping = aes(x = depth_treatment, y = ymin+0.125, label = significance), size = 3) +
    xlab(xlabs[i]) +
    ylab(expression("Dry weight change"~(g~g^{-1}~"%"~day^{-1}) )) +
    scale_y_continuous(limits = c(ymin-0.001, ymax+0.001), breaks = c(-2, -1, 0, 1, 2, 3)) +
    theme_meta() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(colour = "black", size = 0.5),
          axis.line.y = element_line(colour = "black", size = 0.5),
          axis.ticks.x = element_line(size = 0.5),
          axis.ticks.y= element_line(size = 0.5))
  
  plot(p1)
  
  # }



# arrange these plots into a single figure
p_growths <- ggarrange(p_fusp, p_fuve, p_asno, p_fuse ,ncol = 2,nrow = 2,
                       labels = c("a", "b", "c", "d"),
                       font.label = list(size = 11, color = "black", face = "plain"))
plot(p_growths)

# export Fig. 4
ggsave(filename = here("figures/fig_4.pdf"), plot = p_growths, 
       units = "cm", width = 16.5, height = 17, dpi = 300)


# Figure 5 Epiphytes

#Summary Stats
mean(analysis_data$epiphyte_wet_weight_g, na.rm=T)
sd(analysis_data$epiphyte_wet_weight_g, na.rm=T)

mean(analysis_data$epiphyte_wet_weight_g_per_area, na.rm=T)
sd(analysis_data$epiphyte_wet_weight_g_per_area, na.rm=T)

# Epiphytes per area depending on depth and species
mod_epi <- lmer(epiphyte_wet_weight_g_per_area ~ Species*factor(depth_treatment)+(1|site_code/tile_id), 
                data = analysis_data)
summ(mod_epi)
anova(mod_epi)
densityPlot(resid(mod_epi))

# get the emmeans for multiple comparisons
emmeans(mod_epi, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")

# Figure 5a: Fucus spiralis
analysis_data_fu_sp <- 
  analysis_data %>% 
  filter(Species == "Fucus spiralis", !is.na(dry_weight_g_daily_relative_increase))

p_fusp_epi <- 
  ggplot(analysis_data_fu_sp, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(width = .5, outlier.shape = NA,color="#fadb25") +  
  theme_meta() +
  ggtitle("F. spiralis") +
  xlab("Depth (cm)") + 
  ylab(expression("Epiphyte wet weight per fucoid thallus area (g cm"^-2*")")) + 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(0,.16)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11,face="italic"))

# Figure 5b: Fucus vesiculosus
analysis_data_fu_ve <- 
  analysis_data %>% 
  filter(Species=="Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))

p_fuve_epi <- 
  ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(width = .5, outlier.shape = NA,color="#ec7853") + 
  theme_meta() +
  ggtitle("F. vesiculosus") +
  xlab("Depth (cm)") + 
  ylab(" ") + 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(0,.16)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11,face="italic"))

# Figure 5c: Ascophyllum nodosum
analysis_data_as_no <- 
  analysis_data  %>% 
  filter(Species == "Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))

p_asno_epi <- 
  ggplot(analysis_data_as_no, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(width = .5, outlier.shape = NA, color="#9c259f") + 
  theme_meta() +
  ggtitle("A. nodosum") +
  xlab("Depth (cm)") +
  ylab(expression("Epiphyte wet weight per fucoid thallus area (g cm"^-2*")")) + 
  geom_hline(yintercept =0)+
  ylim(ylim=c(0,.16)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11,face="italic"))

# Figure 5d: Fucus serratus
analysis_data_fu_se <- 
  analysis_data %>% 
  filter(Species == "Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))

p_fuse_epi <- 
  ggplot(analysis_data_fu_se, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(width = .5, outlier.shape = NA,color="#0c1787") +  
  theme_meta() +
  ggtitle("F. serratus") +
  xlab("Depth (cm)") + 
  ylab(" ")+
  geom_hline(yintercept =0) + 
  ylim(ylim=c(0,.16)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11,face="italic"))

p_epi <- ggarrange(p_fusp_epi, p_fuve_epi, p_asno_epi, p_fuse_epi, ncol = 2,nrow = 2,
                   labels = c("a", "b", "c", "d"),
                   font.label = list(size = 11, color = "black", face = "plain"))
plot(p_epi)

# Does epiphyte growth restrict fucoid growth?

# fucoid growth ~ total epiphyte ww + Species*depth
mod_epi_growth <- lmer(dry_weight_g_daily_relative_increase ~ epiphyte_wet_weight_g+Species*factor(depth_treatment)+(1|site_code/tile_id), 
                       data = analysis_data)
summ(mod_epi_growth)
anova(mod_epi_growth)
densityPlot(resid(mod_epi_growth))

# fucoid growth ~ epiphyte ww per area + Species*depth
mod_epi <- lmer(dry_weight_g_daily_relative_increase ~ epiphyte_wet_weight_g_per_area+Species*factor(depth_treatment)+(1|site_code/tile_id),
                data = filter(analysis_data, !is.na(epiphyte_wet_weight_g_per_area)))
summ(mod_epi)
anova(mod_epi)

# fucoid growth ~ epiphyte ww per area + Species*depth: standardized beta
mod_epi2 <- lmer(scale(dry_weight_g_daily_relative_increase) ~ scale(epiphyte_wet_weight_g_per_area)+Species*factor(depth_treatment)+(1|site_code/tile_id), 
                 data = analysis_data)
summ(mod_epi2)
anova(mod_epi2)
plot(mod_epi2)

analysis_data2 <- 
  analysis_data %>%
  mutate(Species = as.factor(Species))

# change the levels
levels(analysis_data2$Species) <- c("F. serratus", "A. nodosum", "F. vesiculosus", "F. spiralis")

# Figure 5e:
p_5_reg <- 
  ggplot(analysis_data2, aes(x = epiphyte_wet_weight_g_per_area, y = dry_weight_g_daily_relative_increase, color=Species,linetype=factor(depth_treatment))) + 
  geom_point(size=1,alpha=0.5) +  
  geom_smooth(method="lm",se = F,size=.6,alpha=0.8)+
  theme_meta()+
  scale_linetype_discrete(name="Depth (cm)") +
  scale_colour_manual(name="",values=c("#0c1787","#9c259f", "#ec7853","#fadb25")) +
  xlab(expression("Epiphyte wet weight per fucoid thallus area (g cm"^-2*")")) + 
  ylab(expression("Dry weight change"~(g~g^{-1}~"%"~day^{-1}) )) +
  geom_abline(slope = -2.87, intercept = 0.94,size=1)+theme(legend.key = element_rect(fill = NA, color = NA))+ggtitle("")

plot(p_5_reg)

# combine with the four panel epiphyte plot to plot Fig. 5
p_epi_p5reg <- ggarrange(p_epi, p_5_reg, ncol = 2, nrow = 1,
                    labels = c("", "e"),
                    widths = c(1, 1.25),
                    font.label = list(size = 11, color = "black", face = "plain")
)

plot(p_epi_p5reg)

# export supplementary Fig. epiphytes
ggsave(filename = here("figures/fig_S_epiphyteanalysis.pdf"), plot = p_epi_p5reg, 
       units = "cm", width = 26, height = 22.5, dpi = 300)




###Sensitivity analysis####

sensitivity_runs = list()

for(i in 1:100) {
temp_mod = 
  analysis_data %>% 
  filter(!is.na(dry_weight_g_daily_relative_increase)) %>% 
  group_by(tile_id) %>% 
  sample_n(1)

model.growth.all.species <- lmer(dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code), data = temp_mod)

emm_temp <- emmeans(model.growth.all.species, list(pairwise ~ factor(depth_treatment)/Species), adjust = "tukey")
sensitivity_runs[[i]] = cbind(data.frame(emm_temp$`emmeans of depth_treatment, Species`, runnr = i))
}

df_sensitivity <- do.call("rbind",sensitivity_runs)

df_sensitivity$Species <- factor(df_sensitivity$Species,ordered = TRUE,
                                levels=c("Fucus spiralis","Fucus vesiculosus","Ascophyllum nodosum","Fucus serratus"))
df_sensitivity$depth_treatment <- factor(df_sensitivity$depth_treatment,ordered = TRUE,
                                 levels=c("-5","-12","-28","-40"))

main.analysis = cbind(data.frame(emm),runnr=0)
main.analysis$Species <- factor(main.analysis$Species,ordered = TRUE,
                                 levels=c("Fucus spiralis","Fucus vesiculosus","Ascophyllum nodosum","Fucus serratus"))
main.analysis$depth_treatment <- factor(main.analysis$depth_treatment,ordered = TRUE,
                                         levels=c("-5","-12","-28","-40"))


p=df_sensitivity %>% ggplot(aes(x=depth_treatment, y=emmean,group = runnr)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.01, size=0.05, position = position_dodge(.5)) +
  geom_line(position = position_dodge(.5),size=0.05) +
  geom_point(position = position_dodge(.5),size=0.05) + 
  facet_grid(~ Species) +
  theme_meta()+
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=0, linetype="solid")
  )

main.analysis %>% ggplot(aes(x=depth_treatment, y=emmean,group = runnr)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.01, size=1, position = position_dodge(.5)) +
  geom_line(position = position_dodge(.5),size=0.5) +
  geom_point(position = position_dodge(.5),size=1) + 
  facet_grid(~ Species) +
  theme_meta()+
  theme(
    strip.background = element_rect(
      color="black", fill="white", size=0, linetype="solid")
  )


### END
