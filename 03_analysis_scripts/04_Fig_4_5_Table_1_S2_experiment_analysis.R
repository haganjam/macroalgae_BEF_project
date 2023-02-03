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

# list of packages of load
pkgs <- c("here","readr","vegan","dplyr","lme4",
          "MuMIn","jtools","lmerTest","emmeans",
          "ggpubr", "ggfortify", "car", "ggdist", "ggbeeswarm","readr")

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

mortality %>% 
  group_by(binomial_code,depth_treatment) %>% 
  summarise(mean=mean(freq))

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

# create multiple models to predict dryweight
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
  model10 = lm(dry_weight_total_g ~ final_area_cm2 * Species + final_wet_weight_g * Species + final_area_cm2*final_wet_weight_g,data=analysis_data),
  
  model11 = lm(dry_weight_total_g ~ final_area_cm2 * Species * final_wet_weight_g,data=analysis_data))

# compare multiple models to predict dryweight
model.comparison = rbind(broom::glance(candidate_models_dw$model1),
      broom::glance(candidate_models_dw$model2),
      broom::glance(candidate_models_dw$model3),
      broom::glance(candidate_models_dw$model4),
      broom::glance(candidate_models_dw$model5),
      broom::glance(candidate_models_dw$model6),
      broom::glance(candidate_models_dw$model7),
      broom::glance(candidate_models_dw$model8),
      broom::glance(candidate_models_dw$model9),
      broom::glance(candidate_models_dw$model10),
      broom::glance(candidate_models_dw$model11))

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
    "dry weight ~ area * Species + wet weight * Species + area * wet weight",
    "dry weight ~ area * Species * wet weight"),
  model.comparison)

write.csv("model.comparison",here("figures/model.comparison.dw.prediction.csv"))

# best Model (no 10) is used to predict dryweight
mod_dw <- lm(dry_weight_total_g ~ 
               final_area_cm2 * Species +
               final_wet_weight_g * Species +
               final_area_cm2 * final_wet_weight_g,
             data=analysis_data)
summary(mod_dw)

# plot the model fit
plot(mod_dw$fitted.values, mod_dw$model$dry_weight_total_g)

dw_pred_data = cbind(predicted_dry_weight_g = mod_dw$fitted.values, dry_weight_g = mod_dw$model$dry_weight_total_g)

p_S_dry_weight_prediction <- 
  ggplot(data = dw_pred_data, 
         aes(x = dry_weight_g, y = predicted_dry_weight_g )) + 
  geom_point(size=1,alpha=0.5) +  
  geom_smooth(method="lm", se = TRUE, size=.6, alpha=0.8, col = "black") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "red") +
  theme_meta()+
  xlab(expression("Measured dry weight (g)")) + 
  ylab("Predicted dry weight (g)") +
  xlim(c(0,15)) +
  ylim(c(0,15)) +
  annotate("text", x=1.2, y=14.5, label = expression(~r^{2}~"= 0.98"))
plot(p_S_dry_weight_prediction)

ggsave(filename = here("figures/fig_S3.pdf"), plot = p_S_dry_weight_prediction, 
       units = "cm", width = 10, height = 10, dpi = 450)


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

# summarise statistics on growth by species and by treatments
analysis_data %>% 
  group_by(Species) %>% 
  summarise(mean=mean(dry_weight_total_g_increase,na.rm=T),
            sd=sd(dry_weight_total_g_increase,na.rm=T),
            percent = mean(dry_weight_total_g_relative_increase_total,na.rm=T)
            )

analysis_data %>% 
  group_by(Species,depth_treatment) %>% 
  summarise(mean=mean(dry_weight_total_g_increase,na.rm=T),
            sd=sd(dry_weight_total_g_increase,na.rm=T),
            percent = mean(dry_weight_total_g_relative_increase_total,na.rm=T)
            )

analysis_data %>% 
  group_by(Species) %>% 
  summarise(dry_mean=mean(dry_weight_total_g_before_predicted,na.rm=T),
            dry_sd=sd(dry_weight_total_g_before_predicted,na.rm=T),
            length_cm = mean(initial_length_cm,na.rm=T),
            length_sd = sd(initial_length_cm,na.rm=T)
            )


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

# check the data without missing values
summary.data = na.omit(summary.data)

# create summary table by species and depth
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

# create summary table by 
summary.table_species = 
  summary.data %>%
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
summary.table$depth_treatment[is.na(summary.table$depth_treatment)] = "All"

summary.table <- 
  summary.table %>%
  arrange(factor(species, levels = c("Fucus spiralis", "Fucus vesiculosus", "Ascophyllum nodosum", "Fucus serratus")),
          factor(depth_treatment, levels = c("-5","-12","-28","-40","All"))) %>%
  select(species,depth_treatment, everything()) %>%
  mutate_at(vars(contains("initial_")), round, 1) %>%
  mutate_at(vars(ends_with("_growth")), round, 3)

summary.table = 
  summary.table %>% 
  mutate(initial_dryweight = paste(as.character(format(mean_initial_dryweight,nsmall=1))," ± ", as.character(format(sd_initial_dryweight,nsmall=1))),
         dryweight_growth = paste(as.character(format(mean_dryweight_growth, nsmall = 3)), " ± ", as.character(format(sd_dryweight_growth, nsmall = 3))),
         initial_wetweight = paste(as.character(format(mean_initial_wetweight, nsmall = 1)), " ± ", as.character(format(sd_initial_wetweight, nsmall = 1))),
         wetweight_growth = paste(as.character(format(mean_wetweight_growth, nsmall = 3)), " ± ", as.character(format(sd_wetweight_growth, nsmall = 3))),
         initial_area = paste(as.character(format(mean_initial_area, nsmall = 1)), " ± ", as.character(format(sd_initial_area, nsmall = 1))),
         area_growth = paste(as.character(format(mean_area_growth, nsmall = 3)), " ± ", as.character(format(sd_area_growth, nsmall = 3))),
         initial_max_length = paste(as.character(format(mean_initial_max_length, nsmall = 1)), " ± ", as.character(format(sd_initial_max_length, nsmall = 1))),
         max_length_growth = paste(as.character(format(mean_max_length_growth, nsmall = 3)), " ± ", as.character(format(sd_max_length_growth, nsmall = 3))))

# clean up the summary table
summary.table <- 
  summary.table %>%
  select(-starts_with("mean"), -starts_with("sd"))

# rename the columns in the summary table
colnames(summary.table) <- c("Species","Depth treatment (cm)","Initial dryweight ± SD (g)","Dryweight growth ± SD (g day-1)", "Initial wetweight ± SD (g)",  
                             "Wetweight growth ± SD (g day-1)", "Initial area ± SD (cm²)", "Area growth ± SD (cm² day-1)",
                             "Initial maximum length ± SD (cm)", "Maximum length growth ± SD (cm day-1)")

# write the summary to a .csv file
write.csv(summary.table, here("figures/table_S5.csv"))


# analysis of growth (function) in different depth zones


# analysis for each species: fit an individual model to each species

# Fucus spiralis
model_fu_sp <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),
                    data = filter(analysis_data, binomial_code == "fu_sp"))
anova(model_fu_sp)
densityPlot(resid(model_fu_sp))
summ(model_fu_sp)

emm_fu_sp <- emmeans(model_fu_sp, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")


# pull the results into a data.frame
table_1_fu_sp = cbind(anova(model_fu_sp), r.squaredGLMM(model_fu_sp), 
                      N = length(resid(model_fu_sp)), model = "fu_sp")


# Fucus vesiculosus
model_fu_ve <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),
                    data = filter(analysis_data, binomial_code == "fu_ve"))
anova(model_fu_ve)
densityPlot(resid(model_fu_ve))
summ(model_fu_ve)

# pull the results into a data.frame
table_1_fu_ve = cbind(anova(model_fu_ve), r.squaredGLMM(model_fu_ve), 
                      N = length(resid(model_fu_ve)), model = "fu_ve")
emm_fu_ve <- emmeans(model_fu_ve, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")


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

# Fucus serratus
model_fu_se <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id), 
                    data = filter(analysis_data, binomial_code == "fu_se"))
anova(model_fu_se)
densityPlot(resid(model_fu_se))
summ(model_fu_se)

# pull the results into a data.frame
table_1_fu_se <- cbind(anova(model_fu_se), r.squaredGLMM(model_fu_se), 
                       N = length(resid(model_fu_se)), model = "fu_se")

emm_fu_se <- emmeans(model_fu_se, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")


emm = list(emm_fu_sp,emm_fu_ve,emm_as_no,emm_fu_se)


# Table 1: bind the combined model and all individual models into a table and export as a .csv
write_csv(rbind(table_1_fu_sp,table_1_fu_ve,table_1_as_no,table_1_fu_se),"figures/table_1.csv")


# Figure 4a-d

# set the species names
sp_names <- c("Fucus spiralis", "Fucus vesiculosus", "Ascophyllum nodosum", "Fucus serratus") 

# set the maximum and the minima of each graph
ymin <- -2.35
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
               c("A", "B", "B", "B"),
               c("A", "A", "B", "B"))

# set-up the xlabels
xlabs <- c("", "", "", "Depth treatment (cm)")

# set-up the colours
cols <- c("#D97E46", "#7A5414", "#AF994D", "#EAB20A")

plot_list <- vector("list", length = length(sp_names))
for(i in 1:length(sp_names)) {

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
  plot_df_ci <- data.frame(emm[[i]]$`emmeans of depth_treatment`)
  plot_df_ci$depth_treatment <- factor(plot_df_ci$depth_treatment, levels = c(-5, -12, -28, -40))
  
  p1 <- 
    ggplot() + 
    geom_tile(data = plot_df_sig,
              mapping = aes(x = depth_treatment, y = mid_point, 
                            width = 0.75, height = DW_height),
              alpha = 0.05) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black") + 
    gghalves::geom_half_point(data = plot_df, 
                              mapping = aes(factor(depth_treatment), dry_weight_g_daily_relative_increase), 
                              position = position_nudge(-0.3), colour = cols[i],
                              alpha = 0.6, shape = 1, size = 1,
                              range_scale = 0.4) +
    geom_errorbar(data = plot_df_ci,
                  mapping = aes(x = depth_treatment, 
                                ymin = lower.CL,
                                ymax = upper.CL), 
                  width = 0.05, colour = cols[i], size = 0.45,
                  position = position_nudge(0.1)) +
    geom_point(data = plot_df_ci,
               mapping = aes(x = depth_treatment, y = emmean), 
               shape = 18, colour = cols[i], size = 2.75,
               position = position_nudge(0.1)) +
    geom_text(data = plot_df_sig,
              mapping = aes(x = depth_treatment, y = ymin+0.25, label = significance), size = 2.5) +
    xlab(xlabs[i]) +
    ylab(expression("Dry weight change"~(g~g^{-1}~"%"~day^{-1}) )) +
    ggtitle("") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(ymin-0.001, ymax+0.001), breaks = c(-2, -1, 0, 1, 2, 3)) +
    theme_meta() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(colour = "black", size = 0.5),
          axis.line.y = element_line(colour = "black", size = 0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5),
          axis.title.y = element_text(size = 8.5),
          axis.title.x = element_text(size = 9),
          plot.title = element_text(size = 1))
  
  plot_list[[i]] <- p1
  
  }

# check one plot
plot_list[[1]]

# arrange these plots into a single figure
pg <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]],
                ncol = 1, nrow = 4,
                labels = c("a", "b", "c", "d"),
                font.label = list(size = 10, color = "black", face = "plain"),
                hjust = -4.5)
plot(pg)

# export Fig. 4
ggsave(filename = here("figures/fig_4.png"), plot = pg, 
       units = "cm", width = 6, height = 22, dpi = 450)


# Figure S5: Epiphyte analysis

# summary Stats
mean(analysis_data$epiphyte_wet_weight_g, na.rm=T)
sd(analysis_data$epiphyte_wet_weight_g, na.rm=T)

mean(analysis_data$epiphyte_wet_weight_g_per_area, na.rm=T)
sd(analysis_data$epiphyte_wet_weight_g_per_area, na.rm=T)

# epiphytes per area depending on depth and species
mod_epi <- lmer(epiphyte_wet_weight_g_per_area ~ Species*factor(depth_treatment)+(1|site_code/tile_id), 
                data = analysis_data)
summ(mod_epi)
anova(mod_epi)
densityPlot(resid(mod_epi))

# get the emmeans for multiple comparisons
emmeans(mod_epi, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")

# Figure S5:

# left panel

# set the species names
sp_names <- c("Fucus spiralis", "Fucus vesiculosus", "Ascophyllum nodosum", "Fucus serratus") 

# set-up the axis labels
xlabs <- c("", "", "Depth treatment (cm)", "Depth treatment (cm)")
ylabs <- c(expression("Epiphyte wet weight per thallus area (g cm"^-2*")"),
           "",
           expression("Epiphyte wet weight per thallus area (g cm"^-2*")"),
           "")

# set-up the colours
cols <- c("#D97E46", "#7A5414", "#AF994D", "#EAB20A")

# check the distribution of epiphyte wet-weights
summary(analysis_data$epiphyte_wet_weight_g_per_area)

plot_list <- vector("list", length = length(sp_names))
for(i in 1:length(sp_names)) {
  
  plot_df <- 
    analysis_data  %>% 
    filter(Species == sp_names[i], !is.na(epiphyte_wet_weight_g_per_area)) %>%
    mutate(depth_treatment = factor(depth_treatment, levels = c(-5, -12, -28, -40)))
  
  p1 <- 
    p_fusp_epi <- 
    ggplot(data = plot_df, 
           mapping = aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
    geom_boxplot(width = 0.15, outlier.shape = NA, color = cols[i],
                 position = position_nudge(0.15)) +  
    gghalves::geom_half_point(data = plot_df, 
                              mapping = aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area), 
                              position = position_nudge(-0.35), colour = cols[i],
                              alpha = 0.6, shape = 1, size = 1,
                              range_scale = 0.4) +
    ggtitle(sp_names[i]) +
    xlab(xlabs[i]) + 
    ylab(ylabs[i]) + 
    ylim(ylim = c(0, 0.73)) +
    theme_meta() +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 11, face = "italic"),
          panel.border = element_blank())
  
  plot_list[[i]] <- p1
  
}

# check one plot
plot_list[[1]]

p1234 <- ggarrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], 
                   ncol = 2,nrow = 2, labels = c("a", "b", "c", "d"),
                   hjust = -5, vjust = 2,
                   font.label = list(size = 11, color = "black", face = "plain"))

# right panel

# does epiphyte growth restrict fucoid growth?

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
  mutate(Species = factor(Species))

# change the levels
levels(analysis_data2$Species) <- c("F. serratus", "A. nodosum", "F. vesiculosus", "F. spiralis")

# right panel:
p5 <- 
  ggplot(data = analysis_data2, 
         mapping = aes(x = epiphyte_wet_weight_g_per_area, y = dry_weight_g_daily_relative_increase, color=Species,linetype=factor(depth_treatment))) + 
  geom_point(size = 1, alpha = 0.5) +  
  geom_smooth(method="lm",se = F,size=.6,alpha=0.8)+
  theme_meta()+
  scale_linetype_discrete(name="Depth (cm)") +
  scale_colour_manual(name = "", 
                      values = c("#EAB20A", "#AF994D", "#7A5414", "#D97E46") ) +
  xlab(expression("Epiphyte wet weight per thallus area (g cm"^-2*")")) + 
  ylab(expression("Dry weight change"~(g~g^{-1}~"%"~day^{-1}) )) +
  geom_abline(slope = -2.79, intercept = 0.95, size=1) + # which slope and intercept is this?
  theme(legend.key = element_rect(fill = NA, color = NA)) + 
  ggtitle("")

plot(p5)

# combine with the four panel epiphyte plot to plot Fig. 5
p12345 <- ggarrange(p1234, p5, ncol = 2, nrow = 1,
                    labels = c("", "e"),
                    widths = c(1, 1.25),
                    hjust = -3.5,
                    font.label = list(size = 11, color = "black", face = "plain")
                    )
plot(p12345)

# export supplementary Fig. epiphytes
ggsave(filename = here("figures/fig_S5.png"), plot = p12345, 
       units = "cm", width = 26, height = 22.5, dpi = 450)


# fig S4: sensitivity analysis to check for bias in lost individuals

sensitivity_runs = list()

for(i in 1:200) {
  set.seed(i)
temp_data = 
  analysis_data %>% 
  filter(!is.na(dry_weight_g_daily_relative_increase)) %>% 
  group_by(tile_id) %>% 
  sample_n(1,replace = T) # can also be done for more n

#A growth comparison is calculated for each species
model.growth.fu.sp.species <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code),
                                  data = filter(temp_data, binomial_code == "fu_sp"))
model.growth.fu.ve.species <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code),
                                   data = filter(temp_data, binomial_code == "fu_ve"))
model.growth.as.no.species <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code),
                                   data = filter(temp_data, binomial_code == "as_no"))
model.growth.fu.se.species <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code),
                                   data = filter(temp_data, binomial_code == "fu_se"))

#calculate emmeans for all models
emm_temp.fu.sp <- emmeans(model.growth.fu.sp.species, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")
emm_temp.fu.ve <- emmeans(model.growth.fu.ve.species, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")
emm_temp.as.no <- emmeans(model.growth.as.no.species, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")
emm_temp.fu.se <- emmeans(model.growth.fu.se.species, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")

#create emmeans for depth
emm_temp <- rbind(
  data.frame(emm_temp.fu.sp$`emmeans of depth_treatment`,Species="Fucus spiralis"),
  data.frame(emm_temp.fu.ve$`emmeans of depth_treatment`,Species="Fucus vesiculosus"),
  data.frame(emm_temp.as.no$`emmeans of depth_treatment`,Species="Ascophyllum nodosum"),
  data.frame(emm_temp.fu.se$`emmeans of depth_treatment`,Species="Fucus serratus")
)
#save to list
sensitivity_runs[[i]] = cbind(emm_temp, runnr = i)
}

df_sensitivity <- do.call("rbind",sensitivity_runs)

df_sensitivity$Species <- factor(df_sensitivity$Species,ordered = TRUE,
                                levels=c("Fucus spiralis","Fucus vesiculosus","Ascophyllum nodosum","Fucus serratus"))
df_sensitivity$depth_treatment <- factor(df_sensitivity$depth_treatment,ordered = TRUE,
                                 levels=c("-5","-12","-28","-40"))

main.analysis = 
  #add column with runnr
  cbind(runnr=0,
        #combine all columns from the emm
  rbind(
  data.frame(emm[[1]]$`emmeans of depth_treatment`,Species="Fucus spiralis"),
  data.frame(emm[[2]]$`emmeans of depth_treatment`,Species="Fucus vesiculosus"),
  data.frame(emm[[3]]$`emmeans of depth_treatment`,Species="Ascophyllum nodosum"),
  data.frame(emm[[4]]$`emmeans of depth_treatment`,Species="Fucus serratus")))

main.analysis$Species <- factor(main.analysis$Species,ordered = TRUE,
                                 levels=c("Fucus spiralis","Fucus vesiculosus","Ascophyllum nodosum","Fucus serratus"))
main.analysis$depth_treatment <- factor(main.analysis$depth_treatment,ordered = TRUE,
                                         levels=c("-5","-12","-28","-40"))


plot_sensitivity <-
  df_sensitivity %>% 
  ggplot(aes(x=depth_treatment, y=emmean,group = runnr,color=Species)) + 
  
  # sample sensitivity analysis
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.01, size=0.05, alpha=.9, position = position_dodge(.5)) +
  geom_line(position = position_dodge(.5),size=0.03) +
  geom_point(position = position_dodge(.5),size=0.03,shape = 1) + 
  facet_wrap(~ Species) +
  scale_color_manual(values=cols)+
  
  # observed analysis
  geom_line(data= main.analysis,size=0.5,color = "black") +
  geom_point(data= main.analysis,size=2, color = "black", alpha = 0.7) +
  geom_errorbar(data = main.analysis, 
                mapping = aes(ymin=lower.CL, ymax=upper.CL), 
                width= 0.05, size = 0.5, color = "black") +
  
  theme_meta()+
  theme(
    strip.background = element_rect(color="black", fill="white", size=0, linetype="solid"),
    legend.position = "none",
    strip.text = element_text(size = 11)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") + 
  ylim(c(-2.1,2.4))+
  ylab(expression("Dry weight change"~(g~g^{-1}~"%"~day^{-1}) ))+
  xlab("Depth treatment (cm)")

ggsave(filename = here("figures/fig_S4.png"), plot = plot_sensitivity, 
       units = "cm", width = 18, height = 17, dpi = 450)

### END
