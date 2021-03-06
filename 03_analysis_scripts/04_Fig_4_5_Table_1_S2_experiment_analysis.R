#'
#' @title: Generate ecologically relevant variables from the sea-level data
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
          "ggpubr", "ggfortify", "car", "ggdist")
groundhog.library(pkgs, groundhog.day)

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
mod_dw <- lm(dry_weight_total_g ~ final_area_cm2 * Species + final_wet_weight_g * Species,data=analysis_data)
summary(mod_dw)

# plot the model fit
plot(mod_dw$fitted.values, mod_dw$model$dry_weight_total_g)

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

# calculate a summary table
table(is.na(analysis_data$dry_weight_g_daily_relative_increase))

# epiphyte wet weight per area
analysis_data$epiphyte_wet_weight_g_per_area <- analysis_data$epiphyte_wet_weight_g / analysis_data$final_area_cm2


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
model1 <- lmer(analysis_data$dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = analysis_data)

# combine output into a table
table_1 <- cbind(anova(model1),
                 r.squaredGLMM(model1), 
                 N = length(resid(model1)), 
                 model="all species"
                )

# check the density plot
densityPlot(resid(model1))

# examine the model output
summ(model1)
anova(model1)

# calculate emmeans to compare groups directly
emm <- emmeans(model1, list(pairwise ~ factor(depth_treatment)/Species), adjust = "tukey")

# Table S2: write this output from emmeans into a .csv file
write_csv(as.data.frame(emm$`emmeans of depth_treatment, Species`), here("figures/table_S2.csv") )


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

# pull the results into a data.frame
table_1_fu_sp = cbind(anova(model_fu_sp), r.squaredGLMM(model_fu_sp), 
                      N = length(resid(model_fu_sp)), model = "fu_sp")

# Ascophyllum nodosum
model_as_no <- lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id), 
                    data = filter(analysis_data, binomial_code == "as_no"))
anova(model_as_no)
densityPlot(resid(model_as_no))
summ(model_as_no)

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

# Figure 4a: Fucus spiralis
analysis_data_fu_sp <- 
  analysis_data  %>% 
  filter(Species == "Fucus spiralis", !is.na(dry_weight_g_daily_relative_increase))

p_fusp <- 
  ggplot(analysis_data_fu_sp, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#fadb25") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#fadb25") +
  theme_meta() +
  ggtitle("F. spiralis") +
  xlab("Depth [cm]") +
  ylab("Dry weight increase in % per day") +
  geom_hline(yintercept =0) + 
  ylim(ylim=c(-2,3)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11)) 

plot(p_fusp)

# Figure 4b: Fucus vesiculosus
analysis_data_fu_ve <- 
  analysis_data  %>% 
  filter(Species == "Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))

p_fuve <- 
  ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#ec7853") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#ec7853") +
  theme_meta()+
  ggtitle("F. vesiculosus") +
  xlab("Depth [cm]")+ 
  ylab("    ")+ 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(-2,3)) +
  theme(plot.title = element_text(vjust = -  7, hjust = 0.2,
                                  size = 11))

plot(p_fuve)

# Figure 4c: Ascophyllum nodosum
analysis_data_as_no <- 
  analysis_data  %>% 
  filter(Species == "Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))

p_asno <- 
  ggplot(analysis_data_as_no, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#9c259f") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#9c259f") +
  theme_meta() +
  ggtitle("A. nodosum") +
  xlab("Depth [cm]") +
  ylab("Dry weight increase in % per day") + 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(-2,3)) +
  theme(plot.title = element_text(vjust = -  7, hjust = 0.2,
                                  size = 11))

plot(p_asno)

# Figure 4d: Fucus serratus
analysis_data_fu_se <- 
  analysis_data  %>% 
  filter(Species == "Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))

p_fuse <- 
  ggplot(analysis_data_fu_se, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA", fill="#0c1787") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#0c1787") +
  theme_meta() +
  ggtitle("F. serratus") +
  xlab("Depth [cm]")+ylab("   ") + 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(-2, 3)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11))

plot(p_fuse)

# arrange these plots into a single figure
p_growths <- ggarrange(p_fusp, p_fuve, p_asno, p_fuse ,ncol = 2,nrow = 2,
                       labels = c("a", "b", "c", "d"),
                       font.label = list(size = 11, color = "black", face = "plain"),
                       label.x = 0.85, label.y = 0.90)
plot(p_growths)

# combine with the pca plot to plot Fig. 4
pg_pca <- ggarrange(p_growths, pca_plot, ncol = 2, nrow = 1,
                    labels = c("", "e"),
                    widths = c(1.2, 1),
                    font.label = list(size = 11, color = "black", face = "plain"),
                    label.x = 0.9, label.y = 0.98
                    )

# export Fig. 4
ggsave(filename = here("figures/fig_4.pdf"), plot = pg_pca, 
       units = "cm", width = 21, height = 14, dpi = 300)


# Figure 5

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
  ggdist::stat_halfeye(adjust = .5, width = .4, .width = 0, justification = -.3, point_colour = NA,fill="#fadb25") + 
  geom_boxplot(width = .2, outlier.shape = NA,color="#fadb25") +  
  theme_meta() +
  ggtitle("F. spiralis") +
  xlab("Depth [cm]") + 
  ylab(expression("Epiphytes per thallus area [g * cm"^-2*"]")) + 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(0,.25)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11))

# Figure 5b: Fucus vesiculosus
analysis_data_fu_ve <- 
  analysis_data %>% 
  filter(Species=="Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))

p_fuve_epi <- 
  ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .4, .width = 0, justification = -.3, point_colour = NA,fill="#ec7853") + 
  geom_boxplot(width = .2, outlier.shape = NA,color="#ec7853") + 
  theme_meta() +
  ggtitle("F. vesiculosus") +
  xlab("Depth [cm]") + 
  ylab(" ") + 
  geom_hline(yintercept =0) + 
  ylim(ylim=c(0,.25)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11))

# Figure 5c: Ascophyllum nodosum
analysis_data_as_no <- 
  analysis_data  %>% 
  filter(Species == "Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))

p_asno_epi <- 
  ggplot(analysis_data_as_no, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .4, .width = 0, justification = -.3, point_colour = NA,fill="#9c259f") + 
  geom_boxplot(width = .2, outlier.shape = NA, color="#9c259f") + 
  theme_meta() +
  ggtitle("A. nodosum") +
  xlab("Depth [cm]") +
  ylab(expression("Epiphytes per thallus area [g * cm"^-2*"]")) + 
  geom_hline(yintercept =0)+
  ylim(ylim=c(0,.25)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11))

# Figure 5d: Fucus serratus
analysis_data_fu_se <- 
  analysis_data %>% 
  filter(Species == "Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))

p_fuse_epi <- 
  ggplot(analysis_data_fu_se, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .4, .width = 0, justification = -.3, point_colour = NA, fill="#0c1787") + 
  geom_boxplot(width = .2, outlier.shape = NA,color="#0c1787") +  
  theme_meta() +
  ggtitle("F. serratus") +
  xlab("Depth [cm]") + 
  ylab(" ")+
  geom_hline(yintercept =0) + 
  ylim(ylim=c(0,.25)) +
  theme(plot.title = element_text(vjust = - 7, hjust = 0.2,
                                  size = 11))

p_epi <- ggarrange(p_fusp_epi, p_fuve_epi, p_asno_epi, p_fuse_epi, ncol = 2,nrow = 2,
                   labels = c("a", "b", "c", "d"),
                   font.label = list(size = 11, color = "black", face = "plain"),
                   label.x = 0.85, label.y = 0.905)
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
  scale_linetype_discrete(name="Depth [cm]") +
  scale_colour_manual(name="",values=c("#0c1787","#9c259f", "#ec7853","#fadb25")) +
  xlab(expression("Epiphytes per thallus area [g * cm"^-2*"]")) + 
  ylab("Dry weight increase in % per day") +
  geom_abline(slope = -2.87, intercept = 0.94,size=1)+theme(legend.key = element_rect(fill = NA, color = NA))

plot(p_5_reg)

# combine with the four panel epiphyte plot to plot Fig. 5
p_epi_p5reg <- ggarrange(p_epi, p_5_reg, ncol = 2, nrow = 1,
                    labels = c("", "e"),
                    widths = c(1, 1),
                    font.label = list(size = 11, color = "black", face = "plain"),
                    label.x = 0.575, label.y = 0.98
)

plot(p_epi_p5reg)

# export Fig. 5
ggsave(filename = here("figures/fig_5.pdf"), plot = p_epi_p5reg, 
       units = "cm", width = 22, height = 15, dpi = 300)

### END
