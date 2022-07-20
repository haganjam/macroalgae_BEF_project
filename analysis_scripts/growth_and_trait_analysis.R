
# Project: Functional value of macroalgal biodiversity

# Title: Analyse growth and trait variations in different depths

# load libraries using groundhog
library(groundhog)
groundhog.day <- "2022-07-17"
pkgs <- c("here","readr","vegan","dplyr","lme4","MuMIn","jtools","lmerTest","emmeans")
groundhog.library(pkgs, groundhog.day)

#other libraries
library(ggpubr)
library(ggfortify)
library(car)
library(ggdist)


# load relevant functions
source(here("functions/function_plotting_theme.R"))

if(! dir.exists(here("figures"))){
  print("make a folder called experiment_data in the working directory and save the figures, see README for details")
}


#import cleaned dataset
analysis_data <- read_csv("analysis_data/analysis_data.csv")[-1]
analysis_data = analysis_data[-493,] #outlier, probably wrong measurement, see end of script

#Calculate duration of the growth experiment
analysis_data$duration = as.numeric(as.Date(analysis_data$date_end.x,"%d_%m_%Y")-as.Date(analysis_data$date_start,"%d-%m-%Y"))
table(analysis_data$duration)
summary(analysis_data$duration)


# Calculate growth with weight, perimeter,area , length
analysis_data$growth_length_cm = analysis_data$final_length_cm - analysis_data$initial_length_cm
analysis_data$growth_area_cm2 = analysis_data$final_area_cm2 - analysis_data$initial_area_cm2
analysis_data$growth_wet_weight_g = analysis_data$final_wet_weight_g - analysis_data$initial_wet_weight_g
analysis_data$growth_perimeter_cm = analysis_data$final_perimeter_cm - analysis_data$initial_perimeter_cm

hist(analysis_data$growth_length_cm)
hist(analysis_data$growth_area_cm2)
hist(analysis_data$growth_wet_weight_g)
hist(analysis_data$growth_perimeter_cm)

cor(na.omit(analysis_data %>% select(contains("growth")))) #Correlation between growths
plot(na.omit(analysis_data %>% select(contains("growth"))))

plot(na.omit(analysis_data %>% select(dry_weight_total_g,final_area_cm2)))

# Species names
analysis_data$Species = "Fucus spiralis"
analysis_data$Species[analysis_data$binomial_code == "fu_ve"] = "Fucus vesiculosus"
analysis_data$Species[analysis_data$binomial_code == "as_no"] = "Ascophyllum nodosum"
analysis_data$Species[analysis_data$binomial_code == "fu_se"] = "Fucus serratus"

analysis_data$Species = factor(analysis_data$Species,ordered = T,
                               levels=c("Fucus serratus",
                                        "Ascophyllum nodosum",
                                        "Fucus vesiculosus",
                                        "Fucus spiralis"
                               ))


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
analysis_data$trait_float[is.na(analysis_data$trait_float)] = 0

#Growth percentages
analysis_data$growth_area_cm2_percent = analysis_data$growth_area_cm2 / analysis_data$initial_area_cm2 *100
analysis_data$growth_length_cm_percent = analysis_data$growth_length_cm / analysis_data$initial_length_cm  *100
analysis_data$growth_perimeter_cm_percent = analysis_data$growth_perimeter_cm / analysis_data$initial_perimeter_cm *100
analysis_data$growth_wet_weight_g_percent = analysis_data$growth_wet_weight_g / analysis_data$initial_wet_weight_g *100


# Predict dryweight before

mod_dw = lm(dry_weight_total_g ~ final_area_cm2 * Species + final_wet_weight_g * Species,data=analysis_data)
summary(mod_dw)

plot(mod_dw$fitted.values,mod_dw$model$dry_weight_total_g)

#Predict dryweight for before measurements
pred_dw=analysis_data %>% select(initial_area_cm2,initial_wet_weight_g,Species)
colnames(pred_dw) = c("final_area_cm2","final_wet_weight_g","Species")
predict(mod_dw,newdata=pred_dw)
analysis_data$dry_weight_total_g_before_predicted = predict(mod_dw,newdata=pred_dw)

#dryweight increase
analysis_data$dry_weight_total_g_increase = analysis_data$dry_weight_total_g - analysis_data$dry_weight_total_g_before_predicted

#calculate relative increase over the whole experiment
analysis_data$dry_weight_total_g_relative_increase_total =100 * analysis_data$dry_weight_total_g_increase/analysis_data$dry_weight_total_g_before_predicted

#Growth in percent per day - there were slightly differing durations
analysis_data$dry_weight_g_daily_relative_increase = analysis_data$dry_weight_total_g_relative_increase_total/analysis_data$duration

table(is.na(analysis_data$dry_weight_g_daily_relative_increase))

#Epiphyte wet weight per area

analysis_data$epiphyte_wet_weight_g_per_area = analysis_data$epiphyte_wet_weight_g / analysis_data$final_area_cm2


##### PCA  traits ######

pca_data=analysis_data %>% select(Species,depth_treatment,site_code,contains("trait"))

#Changing depth treatment to factor
pca_data$depth_treatment = as.factor(pca_data$depth_treatment)
pca_data = pca_data %>% select(-trait_SBA) #deselect SBA, this highly correlates with STA


#Rename
colnames(pca_data) = c("Species", "depth","site", "TDMC", "thickness",
                      "STA",       "SA:P" ,      "Pneumatocysts")

pca_data = na.omit(pca_data)

gghistogram(pca_data,x="TDMC",facet.by = "Species")
gghistogram(pca_data,x="thickness",facet.by = "Species")
gghistogram(pca_data,x="STA",facet.by = "Species")
gghistogram(pca_data,x="SA:P",facet.by = "Species")
gghistogram(pca_data,x="Pneumatocysts",facet.by = "Species")

#PCA on traits
pca_res <- prcomp(pca_data[-c(1,2,3)], scale = TRUE)



pca_plot = autoplot(pca_res, data = pca_data, #shape = 'depth',
                    size=3,,
                    loadings = TRUE, loadings.colour = 'black',
                    loadings.label = TRUE, loadings.label.size = 5,loadings.label.vjust=c(1.2,-.8,-1,2,-.5),loadings.label.hjust=c(1,.6,0,1,.5),
                    loadings.label.colour="black",colour="Species",legend.position="none",alpha=0.5) +
  theme_meta() +   scale_colour_manual(values = c("#0c1787","#9c259f", "#ec7853","#fadb25"))+
  #scale_color_viridis(discrete=TRUE, option="plasma") +
  theme(legend.position = "none")
pca_plot

screeplot(pca_res)
summary(pca_res)


ggsave(filename = here("figures/fig_4e_PCA.png"), plot = pca_plot, 
       units = "cm", width = 12, height = 15, dpi = 300)

####PERMANOVA Traits########

#Permanova on traits, remove hashtags if you want to run
set.seed(1)
#adonis2(as.matrix(scale(pca_data[-c(1,2,3)])) ~ pca_data$Species*pca_data$depth, method = "euclidean",
#       permutations = 99999)



##univariate traits
ggboxplot(analysis_data,y="trait_tdmc",x = "binomial_code")
ggboxplot(analysis_data,y="trait_STA",x = "binomial_code")
ggboxplot(analysis_data,y="trait_float",,x = "binomial_code")
ggboxplot(analysis_data,y="trait_thickness",x = "binomial_code")
ggboxplot(analysis_data,y="trait_SAP",x = "binomial_code")



######Analysis of growth (function) in different depth zones #######


###Linear mixed effects model:
model1 = lmer(analysis_data$dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = analysis_data)

table_1 = cbind(anova(model1),r.squaredGLMM(model1),N=length(resid(model1)),model="all species")
densityPlot(resid(model1))

summ(model1)
anova(model1)


#Calculate emmeans
emm=emmeans(model1, list(pairwise ~ factor(depth_treatment)/Species), adjust = "tukey")

write.csv(emm$`emmeans of depth_treatment, Species`,"tables/tableS2_emmeans.csv")

#Post Hoc Analysis for each species  for each species
#Serratus
model_fu_se = lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = filter(analysis_data,binomial_code=="fu_se"))
anova(model_fu_se)
densityPlot(resid(model_fu_se))
summ(model_fu_se) #R^2 68%, n = 56

table_1_fu_se = cbind(anova(model_fu_se),r.squaredGLMM(model_fu_se),N=length(resid(model_fu_se)),model="fu_se")

#Spiralis
model_fu_sp = lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = filter(analysis_data,binomial_code=="fu_sp"))
anova(model_fu_sp)
densityPlot(resid(model_fu_sp))
summ(model_fu_sp)

table_1_fu_sp = cbind(anova(model_fu_sp),r.squaredGLMM(model_fu_sp),N=length(resid(model_fu_sp)),model="fu_sp")


#Ascophyllum
model_as_no = lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = filter(analysis_data,binomial_code=="as_no"))
anova(model_as_no)
densityPlot(resid(model_as_no))
summ(model_as_no)

table_1_as_no = cbind(anova(model_as_no),r.squaredGLMM(model_as_no),N=length(resid(model_as_no)),model="as_no")


#Vesiculosus
model_fu_ve = lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = filter(analysis_data,binomial_code=="fu_ve"))
anova(model_fu_ve)
densityPlot(resid(model_fu_ve))
summ(model_fu_ve)

table_1_fu_ve = cbind(anova(model_fu_ve),r.squaredGLMM(model_fu_ve),N=length(resid(model_fu_ve)),model="fu_ve")


#export table
write.csv(rbind(table_1,table_1_fu_sp,table_1_fu_ve,table_1_as_no,table_1_fu_se),"tables/table1_anovas.csv")


####Raincloud plot for growth (figure 4abcd)#####

analysis_data_fu_se = analysis_data  %>% filter(Species=="Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))
p_fuse=ggplot(analysis_data_fu_se, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#0c1787") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#0c1787") +
  theme_meta()+
  xlab("Depth [cm]")+ylab("   ")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))


analysis_data_fu_ve = analysis_data  %>% filter(Species=="Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))
p_fuve=ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#ec7853") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#ec7853") +
  theme_meta()+
  xlab("Depth [cm]")+ylab("    ")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))


analysis_data_as_no = analysis_data  %>% filter(Species=="Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))
p_asno=ggplot(analysis_data_as_no, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#9c259f") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#9c259f") +
  theme_meta()+
  xlab("Depth [cm]")+ylab("Dry weight increase in % per day")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))


analysis_data_fu_sp = analysis_data  %>% filter(Species=="Fucus spiralis", !is.na(dry_weight_g_daily_relative_increase))
p_fusp=ggplot(analysis_data_fu_sp, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#fadb25") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#fadb25") +
  theme_meta()+
  xlab("Depth [cm]")+ylab("Dry weight increase in % per day")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))



p_growths = ggarrange(p_fusp,p_fuve,p_asno,p_fuse,ncol = 2,nrow = 2,
                      labels = c(" F. spiralis","F. vesiculosus","A. nodosum"," F. serratus"),
                      font.label = list(size = 12, color = "black", face = "italic", family = NULL),
                      label.x = 0.1,label.y = .97)
p_growths

ggsave(filename = here("figures/fig_4_boxplots.png"), plot = p_growths, 
       units = "cm", width = 16, height = 16, dpi = 300)

#####Epiphyte wet weight: Figure 5#######

# Epihpytes per area depending on depth and species
mod_epi = lmer(epiphyte_wet_weight_g_per_area ~ Species*factor(depth_treatment)+(1|site_code/tile_id),data=analysis_data)
summ(mod_epi)
anova(mod_epi)

densityPlot(resid(mod_epi))

emmeans(mod_epi, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")


#Epiphyte wet weight panel
analysis_data_fu_se = analysis_data  %>% filter(Species=="Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))
p_fuse_epi=ggplot(analysis_data_fu_se, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#0c1787") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#0c1787") +  theme_meta()+
  xlab("Depth [cm]")+ylab(expression("   "))+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))

analysis_data_fu_ve = analysis_data  %>% filter(Species=="Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))
p_fuve_epi=ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#ec7853") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#ec7853") +  theme_meta()+
  xlab("Depth [cm]")+ylab(expression("   "))+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))


analysis_data_as_no = analysis_data  %>% filter(Species=="Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))
p_asno_epi=ggplot(analysis_data_as_no, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#9c259f") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#9c259f") +  theme_meta()+
  xlab("Depth [cm]")+ylab(expression("Epiphytes per thallus area [g * cm"^-2*"]"))+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))


analysis_data_fu_sp = analysis_data  %>% filter(Species=="Fucus spiralis", !is.na(dry_weight_g_daily_relative_increase))
p_fusp_epi=ggplot(analysis_data_fu_sp, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#fadb25") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#fadb25") +  theme_meta()+
  xlab("Depth [cm]")+ylab(expression("Epiphytes per thallus area [g * cm"^-2*"]"))+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))

p_epi = ggarrange(p_fusp_epi,p_fuve_epi,p_asno_epi,p_fuse_epi,ncol = 2,nrow = 2,
                  labels = c("    F. spiralis","  F. vesiculosus","  A. nodosum","   F. serratus"),
                  font.label = list(size = 12, color = "black", face = "italic", family = NULL),
                  label.x = 0.1,label.y = .97)
p_epi

ggsave(filename = here("figures/fig_5_epiphyte_boxplots.png"), plot = p_epi, 
       units = "cm", width = 16, height = 16, dpi = 300)


### Does epiphyte growth restrict fucoid growth?

# fucoid growth ~ total epiphyte ww + Species*depth
mod_epi_growth = lmer(dry_weight_g_daily_relative_increase ~ epiphyte_wet_weight_g+Species*factor(depth_treatment)+(1|site_code/tile_id),data=analysis_data)
summ(mod_epi_growth)
anova(mod_epi_growth)

densityPlot(resid(mod_epi_g))

# fucoid growth ~ epiphyte ww per area + Species*depth

mod_epi = lmer(dry_weight_g_daily_relative_increase ~ epiphyte_wet_weight_g_per_area+Species*factor(depth_treatment)+(1|site_code/tile_id),data=filter(analysis_data,!is.na(epiphyte_wet_weight_g_per_area)))
summ(mod_epi)
anova(mod_epi)
#no interactions

#Standardized Beta
mod_epi2 = lmer(scale(dry_weight_g_daily_relative_increase) ~ scale(epiphyte_wet_weight_g_per_area)+Species*factor(depth_treatment)+(1|site_code/tile_id),data=analysis_data)
summ(mod_epi2)
anova(mod_epi2)
plot(mod_epi2)


p_5_reg=ggplot(analysis_data, aes(x=epiphyte_wet_weight_g_per_area, y=dry_weight_g_daily_relative_increase, color=Species,linetype=factor(depth_treatment))) + 
  geom_point(size=1,alpha=0.5) +  geom_smooth(method="lm",se = F,size=.6,alpha=0.8)+ theme_meta()+
  scale_linetype_discrete(name="Depth [cm]")+
  scale_colour_manual(name="",values=c("#0c1787","#9c259f", "#ec7853","#fadb25"))+
  xlab(expression("Epiphytes per thallus area [g * cm"^-2*"]"))+ylab("Dry weight increase in % per day")+
  geom_abline(slope = -2.87, intercept = 0.94,size=1)+theme(legend.key = element_rect(fill = NA, color = NA))

ggsave(filename = here("figures/fig_5_regression.png"), plot = p_5_reg, 
       units = "cm", width = 16, height = 16, dpi = 300)


#####Additional analyses, not included in manuscript######

###Analysis of reproductive structures####
#receptacles
ggboxplot(analysis_data,y="number_receptacles",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")


#Fucus Vesiculosus Sex
ggboxplot(analysis_data,y="number_receptacles",x="depth_treatment",color = "sex_fu_ve",facet.by = "binomial_code")
analysis_data$sex_fu_ve


#PCA for fucus vesiculosus on sex
pca_data=analysis_data %>% select(sex_fu_ve,binomial_code,depth_treatment,contains("trait"))

pca_data_fu_ve=na.omit(pca_data %>% filter(binomial_code=="fu_ve"))
pca_res <- prcomp(pca_data_fu_ve[-c(1,2,3)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_ve, colour = 'sex_fu_ve',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

#adonis(as.matrix(pca_data_fu_ve[-c(1,2,3)]) ~ pca_data_fu_ve$sex_fu_ve, method = "euclidean",
#       permutations = 9999)

#no difference in sex

##### Linear trait growht relationship ######
pca_data=analysis_data %>% select(Species,dry_weight_g_daily_relative_increase,depth_treatment,site_code,tile_id,contains("trait"))

#Changing depth treatment to factor
pca_data$depth_treatment = as.factor(pca_data$depth_treatment)
pca_data = pca_data %>% select(-trait_SBA) #deselect SBA, this highly correlates with STA


#Rename
colnames(pca_data) = c("Species","growth", "depth","site_code","tile_id", "TDMC", "thickness",
                       "STA",       "SA:P" ,      "Pneumatocysts")

pca_data = na.omit(pca_data)
pca_res <- prcomp(pca_data[-c(1,2,3,4,5)], scale = TRUE) #create PCA with traits only

#extract principal components
trait_components = cbind(pca_data,trait_pc1=pca_res$x[,1],trait_pc2=pca_res$x[,2])

#test for linear depth x trait interaction on growth
model_trait = lmer(growth ~ as.numeric(depth)*trait_pc1+(1|site_code/tile_id),data = trait_components)

densityPlot(resid(model_trait))

summ(model_trait)
anova(model_trait)

#same for pc2
model_trait = lmer(growth ~ as.numeric(depth)*trait_pc2+(1|site_code/tile_id),data = trait_components)
densityPlot(resid(model_trait))
summ(model_trait)
anova(model_trait)

####outlier identification#####
mod_dw = lm(dry_weight_total_g ~ final_area_cm2 * Species + final_wet_weight_g * Species,data=analysis_data)
summary(mod_dw)

a=data.frame(mod_dw$fitted.values,mod_dw$model$dry_weight_total_g)
rownames(a)=names(mod_dw$fitted.values)
colnames(a)=c("fitted","vals")
sp <- ggplot(a, aes(fitted, vals, label = rownames(a)))+
  geom_point()
# Add texts
sp + geom_text()
#493 #removed in the beginning

