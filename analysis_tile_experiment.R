# Script to analyse merged data from the tile experiment 2021
# Tiles were placed at 4 different depths at 5 sites. 4 Species
# 2021-11-07
# Benedikt Schrofner-Brunner
# benedikt.brunner@gu.se

# LOAD packages
library(car)
library(dplyr)
library(rstatix)
library(ggpubr)
library(reshape2)
# TODO groundhog


#import cleaned dataset
library(readr)
analysis_data <- read_csv("analysis_data/analysis_data.csv")[-1]

# Growth with weight, perimeter,area , length
analysis_data$growth_length_cm = analysis_data$final_length_cm - analysis_data$initial_length_cm
analysis_data$growth_area_cm2 = analysis_data$final_area_cm2 - analysis_data$initial_area_cm2
analysis_data$growth_wet_weight_g = analysis_data$final_wet_weight_g - analysis_data$initial_wet_weight_g
analysis_data$growth_perimeter_cm = analysis_data$final_perimeter_cm - analysis_data$initial_perimeter_cm

hist(analysis_data$growth_length_cm)
hist(analysis_data$growth_area_cm2)
hist(analysis_data$growth_wet_weight_g)
hist(analysis_data$growth_perimeter_cm)

cor(na.omit(analysis_data %>% select(contains("growth"))))
plot(na.omit(analysis_data %>% select(contains("growth"))))

test = analysis_data %>% filter(trait_tdmc<.2)

# Calculating traits after the experiment after https://seaweedtraits.github.io/traits-db.html

#TDMC - Thallus Dry Matter Content (no units): obtained by dividing dry mass (g) by fresh mass (g)
analysis_data$trait_tdmc = analysis_data$dry_weight_total_g/analysis_data$final_wet_weight_g
hist(analysis_data$trait_tdmc)



#Thickness
analysis_data$trait_thickness = analysis_data$blade_thickness_mean

# Ascophyllum thickness is represented by midrib thickness
analysis_data$trait_thickness[analysis_data$binomial_code=="as_no"] = analysis_data$midrib_mean[analysis_data$binomial_code=="as_no"]
hist(analysis_data$trait_thickness)


#STA Specific Thallus Area (mm2 g-1): obtained by dividing the area (mm2) of a sample by its dry mass (g)
analysis_data$trait_STA = ( analysis_data$final_area_cm2 * 100 ) / analysis_data$dry_weight_total_g
hist(analysis_data$trait_STA)


#SBA Specific Blade Area
analysis_data$trait_SBA = ( analysis_data$final_blade_area_cm2 * 100 ) / analysis_data$dry_weight_blade_g
hist(analysis_data$trait_SBA)


#SA:P - Surface Area to Perimeter ratio (no units): obtained by dividing the area (mm2) of a sample by its perimeter (mm)
analysis_data$trait_SAP = ( analysis_data$final_area_cm2 * 100 ) / analysis_data$final_perimeter_cm
hist(analysis_data$trait_SAP)

#Own traits:
#Lift per DW - bladder volume (obtained by calculating the volume of a sphere from diameter (=thickness)) times bladder count per dry weight
analysis_data$lift = (4/3)*pi* (analysis_data$bladder_thickness_mean / 2 ) *analysis_data$number_of_bladders
analysis_data$trait_float = analysis_data$lift / analysis_data$dry_weight_total_g
hist(analysis_data$trait_float)


##### PCA within species ####
library(ggfortify)

# selecting trait data
pca_data=analysis_data %>% select(binomial_code,depth_treatment,contains("trait"))

#Changing depth treatment to factor
pca_data$depth_treatment = as.character(pca_data$depth_treatment)


#PCA for fucus spiralis
pca_data_fu_sp=na.omit(pca_data %>% filter(binomial_code=="fu_sp") %>% select(-trait_float))
pca_res <- prcomp(pca_data_fu_sp[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_sp, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")


#PCA for fucus vesiculosus
pca_data_fu_ve=na.omit(pca_data %>% filter(binomial_code=="fu_ve"))
pca_res <- prcomp(pca_data_fu_ve[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_ve, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

#PCA for ascophyllum nodosum
pca_data_as_no=na.omit(pca_data %>% select(-trait_SBA) %>% filter(binomial_code=="as_no"))
pca_res <- prcomp(pca_data_as_no[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_as_no, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")


#PCA for fucus serratus
pca_data_fu_se=na.omit(pca_data %>% filter(binomial_code=="fu_se") %>% select(-trait_float))
pca_res <- prcomp(pca_data_fu_se[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_se, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

#max PCA
pca_data

pca_max=na.omit(pca_data %>% select(-trait_float,-trait_SBA))
pca_res <- prcomp(pca_max[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_max, shape = 'binomial_code',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.colour="black",colour="depth_treatment")


# Growth per depth
ggboxplot(analysis_data,y="growth_length_cm",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_perimeter_cm",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_wet_weight_g",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_area_cm2",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")

#Epiphytes
ggboxplot(analysis_data,y="epiphyte_wet_weight_g",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")

#univariate traits
ggboxplot(analysis_data,y="trait_tdmc",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="trait_STA",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="trait_float",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="trait_thickness",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="trait_SAP",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")

#receptacles
ggboxplot(analysis_data,y="number_receptacles",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")


#Fucus Vesiculosus Sex
ggboxplot(analysis_data,y="number_receptacles",x="depth_treatment",color = "sex_fu_ve",facet.by = "binomial_code")
analysis_data$sex_fu_ve


#PCA for fucus vesiculosus
pca_data=analysis_data %>% select(sex_fu_ve,binomial_code,depth_treatment,contains("trait"))

pca_data_fu_ve=na.omit(pca_data %>% filter(binomial_code=="fu_ve"))
pca_res <- prcomp(pca_data_fu_ve[-c(1,2,3)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_ve, colour = 'sex_fu_ve',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")






