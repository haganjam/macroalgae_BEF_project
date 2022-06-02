
# Project: Tile experiment

# Title: Analyse growth in different zones.

# LOAD packages 
library(car)
library(dplyr)
library(rstatix)
library(ggpubr)
library(lme4)
library(lmerTest)
library(purrr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggfortify)
library(viridis)
library(vegan)
library(here)
library(MuMIn)

source("functions/function_plotting_theme.R")

if(! dir.exists(here("figures"))){
  print("make a folder called experiment_data in the working directory and save the figures, see README for details")
}

# TODO groundhog

#import cleaned dataset
analysis_data <- read_csv("analysis_data/analysis_data.csv")[-1]
analysis_data = analysis_data[-493,] #outlier, probably wrong measurement

#Calculate duration of the growth experiment
analysis_data$duration = as.numeric(as.Date(analysis_data$date_end.x,"%d_%m_%Y")-as.Date(analysis_data$date_start,"%d-%m-%Y"))
table(analysis_data$duration)

hist(analysis_data$duration)

median(analysis_data$duration,na.rm = T)
min(analysis_data$duration,na.rm = T)
max(analysis_data$duration,na.rm = T)


# Growth with weight, perimeter,area , length
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


##### PCA within species ######

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

#adonis(as.matrix(pca_data_fu_sp[-c(1,2)]) ~ pca_data_fu_sp$depth_treatment, method = "euclidean",
#       permutations = 9999)

#PCA for fucus vesiculosus
pca_data_fu_ve=na.omit(pca_data %>% filter(binomial_code=="fu_ve"))
pca_res <- prcomp(pca_data_fu_ve[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_ve, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

#adonis(as.matrix(pca_data_fu_ve[-c(1,2)]) ~ pca_data_fu_ve$depth_treatment, method = "euclidean",
#       permutations = 9999)

#PCA for ascophyllum nodosum
pca_data_as_no=na.omit(pca_data %>% select(-trait_SBA) %>% filter(binomial_code=="as_no"))
pca_res <- prcomp(pca_data_as_no[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_as_no, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

mod1=manova(as.matrix(pca_data_as_no[-c(1,2)]) ~ pca_data_as_no$depth_treatment)
summary(mod1)

#adonis(as.matrix(pca_data_as_no[-c(1,2)]) ~ pca_data_as_no$depth_treatment, method = "euclidean",
#       permutations = 9999)

#PCA for fucus serratus
pca_data_fu_se=na.omit(pca_data %>% filter(binomial_code=="fu_se") %>% select(-trait_float))
pca_res <- prcomp(pca_data_fu_se[-c(1,2)], scale = TRUE)

autoplot(pca_res, data = pca_data_fu_se, colour = 'depth_treatment',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

#adonis(as.matrix(pca_data_fu_se[-c(1,2)]) ~ pca_data_fu_se$depth_treatment, method = "euclidean",
#       permutations = 9999)

#max PCA
pca_data

pca_data=analysis_data %>% select(Species,depth_treatment,site_code,final_length_cm,epiphyte_wet_weight_g,contains("trait"))

#Changing depth treatment to factor
pca_data$depth_treatment = as.factor(pca_data$depth_treatment)


pca_max=na.omit(pca_data %>% select(-trait_SBA))



#Rename
colnames(pca_max) = c("Species", "depth","site","length","epiphytes", "TDMC", "thickness",
                      "STA",       "SA:P" ,      "Pneumatocysts")

pca_max$epiphytes=log(pca_max$epiphytes+1)#transform epiphytes

gghistogram(pca_max,x="length",facet.by = "Species")
gghistogram(pca_max,x="epiphytes",facet.by = "Species")
gghistogram(pca_max,x="TDMC",facet.by = "Species")
gghistogram(pca_max,x="thickness",facet.by = "Species")
gghistogram(pca_max,x="STA",facet.by = "Species")
gghistogram(pca_max,x="SA:P",facet.by = "Species")
gghistogram(pca_max,x="Pneumatocysts",facet.by = "Species")


pca_res <- prcomp(pca_max[-c(1,2,3,4,5)], scale = TRUE)


colnames(pca_max) = c("Species", "depth","site","length","epiphytes", "TDMC", "thickness",
                      "STA",       "SA:P" ,      "Pneumatocysts")

pca_plot = autoplot(pca_res, data = pca_max, #shape = 'depth',
                    size=3,,
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 5,loadings.label.vjust=c(1.2,-.8,-1,2,-.5),loadings.label.hjust=c(1,.6,0,1,.5),
         loadings.label.colour="black",colour="Species",legend.position="none",alpha=0.5) +
  theme_meta() + scale_color_viridis(discrete=TRUE, option="plasma") +
  theme(legend.position = "none")
pca_plot

screeplot(pca_res)
summary(pca_res)


ggsave(filename = here("figures/fig_4_PCA.png"), plot = pca_plot, 
       units = "cm", width = 12, height = 15, dpi = 300)

pca_plot_nodepth = autoplot(pca_res, data = pca_max, size=3,,
                    loadings = TRUE, loadings.colour = 'red',
                    loadings.label = TRUE, loadings.label.size = 5,
                    loadings.label.colour="black",colour="Species") + theme_meta() + scale_color_viridis(discrete=TRUE, option="plasma")

ggsave(filename = here("figures/fig_4_PCA_nodepth.png"), plot = pca_plot_nodepth, 
       units = "cm", width = 20, height = 16, dpi = 300)

adonis2(as.matrix(scale(pca_max[-c(1,2,3)])) ~ pca_max$Species*pca_max$depth, method = "euclidean",
       permutations = 99999)

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

###### Analysis of growth (function) in different depth zones #######

# Growth per depth
ggboxplot(analysis_data,y="growth_length_cm",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_perimeter_cm",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_wet_weight_g",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_area_cm2",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="dry_weight_g_daily_relative_increase",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")



analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_area_cm2)) %>% kruskal_test(growth_area_cm2~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_length_cm)) %>% kruskal_test(growth_length_cm~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_perimeter_cm)) %>% kruskal_test(growth_perimeter_cm~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_wet_weight_g)) %>% kruskal_test(growth_wet_weight_g~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_wet_weight_g)) %>% kruskal_test(growth_wet_weight_g~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(dry_weight_g_daily_relative_increase)) %>% kruskal_test(dry_weight_g_daily_relative_increase~depth_treatment)


#growth_percent
analysis_data$growth_area_cm2_percent = analysis_data$growth_area_cm2 / analysis_data$initial_area_cm2 *100
analysis_data$growth_length_cm_percent = analysis_data$growth_length_cm / analysis_data$initial_length_cm  *100
analysis_data$growth_perimeter_cm_percent = analysis_data$growth_perimeter_cm / analysis_data$initial_perimeter_cm *100
analysis_data$growth_wet_weight_g_percent = analysis_data$growth_wet_weight_g / analysis_data$initial_wet_weight_g *100

ggboxplot(analysis_data,y="growth_length_cm_percent",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")
ggboxplot(analysis_data,y="growth_perimeter_cm_percent",x="depth_treatment",color = "binomial_code",facet.by = "binomial_code")

ggboxplot(analysis_data,y="growth_wet_weight_g_percent",
          x="depth_treatment",color = "Species",xlab="depth treatment [cm]",ylab="wet weight growth in %", ylim = c(-100,200))+ 
  geom_hline(yintercept=0)+ scale_y_continuous(breaks=c(-100,-50, 0,50, 100,150,200))

ggboxplot(analysis_data,y="growth_area_cm2_percent",
          x="depth_treatment",color = "Species",xlab="depth treatment [cm]",ylab="areal growth in %",ylim = c(-100,200))+ 
  geom_hline(yintercept=0)+ scale_y_continuous(breaks=c(-100,-50, 0,50, 100,150,200))

ggboxplot(analysis_data,y="epiphyte_wet_weight_g",
          x="depth_treatment",xlab="depth treatment [cm]",ylab="Epiphyte wet weight [g]")+ 
  geom_hline(yintercept=0)+ scale_y_continuous(breaks=c(0,10,20,30,40,50))

####Losses####      
analysis_data$survived = 1
analysis_data$survived[is.na(analysis_data$growth_wet_weight_g_percent)] = 0


analysis_data %>% group_by(Species) %>% summarise(sum(survived))

analysis_data %>% group_by(depth_treatment) %>% summarise(sum(survived))

analysis_data %>% group_by(Species,depth_treatment) %>% summarise(survival = sum(survived))

# Survival by species and treatment
analysis_data %>% group_by(Species,depth_treatment) %>% summarise(survival = sum(survived)) %>% mutate (survival=survival/(720/16))

# Survival species
analysis_data %>% group_by(Species) %>% summarise(survival = sum(survived)) %>% mutate (survival=survival/(720/4))



#####Growth analysis#######

analysis_data$origin_site_code

#Species
model1=lm(dry_weight_g_daily_relative_increase ~ Species+origin_site_code,data=analysis_data)
summary(aov(model1))
eta_squared(model1)

#Does origin site matter
model1=lm(dry_weight_g_daily_relative_increase ~ Species+origin_site_code+site_code,data=analysis_data)
summary(aov(model1))
eta_squared(model1)

#Does transplant site matter?
model1=lm(dry_weight_g_daily_relative_increase ~ Species*depth_treatment+origin_site_code+site_code,data=analysis_data)
summary(aov(model1))
eta_squared(model1)

boxplot(analysis_data$growth_area_cm2 ~ analysis_data$site_code)


###Linear mixed effects model:
model1 = lmer(analysis_data$dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = analysis_data)

table_1 = cbind(anova(model1),r.squaredGLMM(model1),N=length(resid(model1)),model="all species")
densityPlot(resid(model1))

plot(model1)

library(jtools)
summ(model1)
anova(model1)




library(sjstats)
library(MuMIn)

library(emmeans)
emm=emmeans(model1, list(pairwise ~ factor(depth_treatment)/Species), adjust = "tukey")

write.csv(emm$`emmeans of depth_treatment, Species`,"tables/table2_a_emmeans.csv")
write.csv(emm$`pairwise differences of depth_treatment, Species`,"tables/table2_b_pairwise_comparisons.csv")

#ANOVA for each species
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


#Ves
model_fu_ve = lmer(dry_weight_g_daily_relative_increase ~ factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = filter(analysis_data,binomial_code=="fu_ve"))
anova(model_fu_ve)
densityPlot(resid(model_fu_ve))
summ(model_fu_ve)

table_1_fu_ve = cbind(anova(model_fu_ve),r.squaredGLMM(model_fu_ve),N=length(resid(model_fu_ve)),model="fu_ve")


#export table
write.csv(rbind(table_1,table_1_fu_sp,table_1_fu_ve,table_1_as_no,table_1_fu_se),"tables/table1_anovas.csv")


#emmeants plot
emm=as.data.frame(emm$`emmeans of depth_treatment, Species`)
ggbarplot(data = emm,x="depth_treatment",y="emmean",fill="Species",position = position_dodge(0.9))

# Default line plot
p<- ggplot(emm, aes(x=factor(depth_treatment), y=emmean, group=Species, color=Species)) + 
  geom_line(position=position_dodge(.2)) +
  geom_point(position=position_dodge(.2))+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.4,
                position=position_dodge(.2))+geom_hline(yintercept=0, linetype="dashed", 
                                                        color = "red", size=1)
print(p)
# Finished line plot
p+labs(title="Emmeans of areal growth in %", x="depth (cm)", y = "% areal growth")+theme_bw()+ scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) + theme_bw()
library(viridis)

#####GAM#####

#GAM does not work because there are only 4 depths....
gam.alg = mgcv::gam(dry_weight_g_daily_relative_increase ~ s(depth_treatment)+s(Species),data=analysis_data)
plot(gam.alg)

#Induce small error because GAM seems to need that
test=analysis_data$depth_treatment+rnorm(n=length(analysis_data$depth_treatment),mean = 0,sd = 0.001)
analysis_data$test=test

gam.alg = mgcv::gam(dry_weight_g_daily_relative_increase ~ s(test,by=Species)+s(test)+Species,data=analysis_data)
library(tidymv)
gam_plot=plot_smooths(
  model = gam.alg,
  series = test,
  comparison = Species)+xlab("depth [cm]")+ylab("dry weight increase in % per day")+theme_meta()+ scale_color_viridis(discrete = TRUE, option = "plasma")+
  scale_fill_viridis(discrete = TRUE,option="plasma")+ scale_x_continuous(breaks=c(-40,-28, -12,-5)) +
  theme(legend.position = "none")


ggsave(filename = here("figures/fig_5_GAM.png"), plot = gam_plot, 
       units = "cm", width = 15, height = 10, dpi = 300)


####Raincloud plot for growth (figure 5)#####
library(ggdist)


analysis_data_fu_se = analysis_data  %>% filter(Species=="Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))
p_fuse=ggplot(analysis_data_fu_se, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#0c1787") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#0c1787") +
  #ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.1, binwidth = .1,color=NA)+
  theme_meta()+
  xlab("depth [cm]")+ylab("   ")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))


analysis_data_fu_ve = analysis_data  %>% filter(Species=="Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))
p_fuve=ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#ec7853") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#ec7853") +
  #ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.1, binwidth = .1,color=NA)+
  theme_meta()+
  xlab("depth [cm]")+ylab("    ")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))


analysis_data_as_no = analysis_data  %>% filter(Species=="Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))
p_asno=ggplot(analysis_data_as_no, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = "NA",fill="#9c259f") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#9c259f") +
  #ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.1, binwidth = .1,color=NA)+
  theme_meta()+
  xlab("depth [cm]")+ylab("dry weight increase in % per day")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))


analysis_data_fu_sp = analysis_data  %>% filter(Species=="Fucus spiralis", !is.na(dry_weight_g_daily_relative_increase))
p_fusp=ggplot(analysis_data_fu_sp, aes(factor(depth_treatment), dry_weight_g_daily_relative_increase)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA,fill="#f1f820") + 
  geom_boxplot(width = .1, outlier.shape = NA,color="#f1f820") +
  #ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.1, binwidth = .1,color=NA)+
  theme_meta()+
  xlab("depth [cm]")+ylab("dry weight increase in % per day")+geom_hline(yintercept =0)+ylim(ylim=c(-2,3))



p_growths = ggarrange(p_fusp,p_fuve,p_asno,p_fuse,ncol = 2,nrow = 2,
                      labels = c(" F. spiralis","F. vesiculosus","A. nodosum"," F. serratus"),
                      font.label = list(size = 12, color = "black", face = "italic", family = NULL),
                      label.x = 0.1,label.y = .97)
p_growths

ggsave(filename = here("figures/fig_5_boxplots.png"), plot = p_growths, 
       units = "cm", width = 16, height = 16, dpi = 300)

###Epihyte wet weight#####
ggboxplot(analysis_data,y="epiphyte_wet_weight_g",x="depth_treatment",color = "Species")
ggboxplot(analysis_data,y="epiphyte_wet_weight_g_per_area",
          x="depth_treatment",color = "Species",xlab="depth", ylab="epiphytes per thallus area [g*cm-2]")+
  color_palette()

ggboxplot(analysis_data,y="epiphyte_wet_weight_g_per_area",
          x="depth_treatment",color = "Species",xlab="depth", ylab="epiphytes per thallus area [g*cm-2]")



#Epiphyte wet weight panel
analysis_data_fu_se = analysis_data  %>% filter(Species=="Fucus serratus", !is.na(dry_weight_g_daily_relative_increase))
p_fuse_epi=ggplot(analysis_data_fu_se, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(color="#0c1787") +
  theme_meta()+
  xlab("depth [cm]")+ylab("  ")+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))
p_fuse_epi

analysis_data_fu_ve = analysis_data  %>% filter(Species=="Fucus vesiculosus", !is.na(dry_weight_g_daily_relative_increase))
p_fuve_epi=ggplot(analysis_data_fu_ve, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(color="#ec7853") +
  theme_meta()+
  xlab("depth [cm]")+ylab("  ")+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))
p_fuve_epi

analysis_data_as_no = analysis_data  %>% filter(Species=="Ascophyllum nodosum", !is.na(dry_weight_g_daily_relative_increase))
p_asno_epi=ggplot(analysis_data_as_no, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(color="#9c259f") +
  theme_meta()+
  xlab("depth [cm]")+ylab(expression("epiphytes per thallus area [g * cm"^-2*"]"))+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))
p_asno_epi

analysis_data_fu_sp = analysis_data  %>% filter(Species=="Fucus spiralis", !is.na(dry_weight_g_daily_relative_increase))
p_fusp_epi=ggplot(analysis_data_fu_sp, aes(factor(depth_treatment), epiphyte_wet_weight_g_per_area)) + 
  geom_boxplot(color="#f1f820") +
  theme_meta()+
  xlab("depth [cm]")+ylab(expression("epiphytes per thallus area [g * cm"^-2*"]"))+geom_hline(yintercept =0)+ylim(ylim=c(0,.25))
p_fusp_epi

p_epi = ggarrange(p_fusp_epi,p_fuve_epi,p_asno_epi,p_fuse_epi,ncol = 2,nrow = 2,
                      labels = c("    F. spiralis","  F. vesiculosus","  A. nodosum","   F. serratus"),
                      font.label = list(size = 12, color = "black", face = "italic", family = NULL),
                      label.x = 0.1,label.y = .97)
p_epi

ggsave(filename = here("figures/fig_6_epi_boxplots.png"), plot = p_epi, 
       units = "cm", width = 16, height = 16, dpi = 300)


mod_epi = lmer(epiphyte_wet_weight_g_per_area ~ Species*factor(depth_treatment)+(1|site_code/tile_id),data=analysis_data)
summ(mod_epi)
anova(mod_epi)

library(emmeans)
emmeans(mod_epi, list(pairwise ~ factor(depth_treatment)), adjust = "tukey")

# Does epiphyte growth restrict fucoid growth?
mod_epi = lmer(dry_weight_g_daily_relative_increase ~ epiphyte_wet_weight_g+Species*factor(depth_treatment)+(1|site_code/tile_id),data=analysis_data)
summ(mod_epi)
anova(mod_epi)

mod_epi = lmer(dry_weight_g_daily_relative_increase ~ epiphyte_wet_weight_g_per_area+Species*factor(depth_treatment)+epiphyte_wet_weight_g_per_area:Species+epiphyte_wet_weight_g_per_area:factor(depth_treatment)+(1|site_code/tile_id),data=filter(analysis_data,!is.na(epiphyte_wet_weight_g_per_area)))
summ(mod_epi)
anova(mod_epi)
#no interactions

#model comparison
mod_epi0 = lmer(dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment)+(1|site_code/tile_id),data=filter(analysis_data,!is.na(epiphyte_wet_weight_g_per_area)))
summ(mod_epi0)
anova(mod_epi0)

mod_epi1 = lmer((dry_weight_g_daily_relative_increase) ~ epiphyte_wet_weight_g_per_area+Species*factor(depth_treatment)+(1|site_code/tile_id),data=filter(analysis_data,!is.na(epiphyte_wet_weight_g_per_area)))
summ(mod_epi1)
anova(mod_epi1)
plot(mod_epi2)

anova(mod_epi1,mod_epi) # no interactions found

anova(mod_epi0,mod_epi1)

#Standardized Beta
mod_epi2 = lmer(scale(dry_weight_g_daily_relative_increase) ~ scale(epiphyte_wet_weight_g_per_area)+Species*factor(depth_treatment)+(1|site_code/tile_id),data=analysis_data)
summ(mod_epi2)
anova(mod_epi2)
plot(mod_epi2)

ggplot(analysis_data,aes(y=epiphyte_wet_weight_g_per_area, x=dry_weight_g_daily_relative_increase))

p_6_reg=ggplot(analysis_data, aes(x=epiphyte_wet_weight_g_per_area, y=dry_weight_g_daily_relative_increase, color=Species,linetype=factor(depth_treatment))) + 
  geom_point(size=1,alpha=0.5) +  geom_smooth(method="lm",se = F,size=.6,alpha=0.8)+ theme_meta()+
  scale_linetype_discrete(name="depth [cm]")+
  scale_colour_manual(name="",values=c("#0c1787","#9c259f", "#ec7853","#f1f820"))+
  xlab(expression("epiphytes per thallus area [g * cm"^-2*"]"))+ylab("dry weight increase in % per day")+
  geom_abline(slope = -2.87, intercept = 0.94,size=1)

ggsave(filename = here("figures/fig_6_regression.png"), plot = p_6_reg, 
       units = "cm", width = 16, height = 16, dpi = 300)

  ###ADD THE REG LINE AND REDO THE ONE GRAPH FROM BEFR

analysis_data$Species

ggscatter(analysis_data,y = "dry_weight_g_daily_relative_increase",x="epiphyte_wet_weight_g_per_area",facet.by = "depth_treatment",color = "Species",add = "reg.line")
ggscatter(analysis_data,y = "dry_weight_g_daily_relative_increase",x="epiphyte_wet_weight_g_per_area",add = "reg.line")

####Trait environment intreraction####

pca_data
pca_data=analysis_data %>% select(Species,depth_treatment,site_code,tile_id,dry_weight_g_daily_relative_increase,contains("trait"))

#Changing depth treatment to factor
pca_data$depth_treatment = as.factor(pca_data$depth_treatment)

pca_max=na.omit(pca_data %>% select(-trait_SBA))

colnames(pca_max) = c("Species", "depth","site","tile_id","growth", "TDMC", "thickness",
                      "STA",       "SA:P" ,      "Pneumatocysts")

pca_res <- prcomp(pca_max[-c(1,2,3,4,5)], scale = TRUE)

autoplot(pca_res, data = pca_max, shape = 'depth', size=3,,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour="black",colour="Species")  + theme_bw()


pc1=data.frame(pca_res$x)$PC1
pc2=data.frame(pca_res$x)$PC2

pc1

model1 = lmer(dry_weight_g_daily_relative_increase ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = analysis_data)
anova(model1)

summary(model1)

library(jtools)
summ(model1)

mod0=lm(growth ~ depth*Species,pca_max)
anova(mod0)

mod10=lm(growth ~ depth*pc1,pca_max)
anova(mod10)

mod1=lm(growth ~ factor(depth)*Species+factor(depth)*pc1,pca_max)
anova(mod1)
plot(mod1)

mod2=lm(growth ~ depth*Species+depth*pc1+depth*pc2,pca_max)
anova(mod2)

anova(mod0,mod1)
anova(mod1,mod2)
anova(mod0,mod2)

####Mixed model approach
model0 = lmer(growth ~ Species*depth +(1|site/tile_id),data = pca_max)
anova(model0)
summ(model0)

model1 = lmer(growth ~ Species*depth+depth*pc1+(1|site/tile_id),data = pca_max)
anova(model1)
summ(model1)

model1 = lmer(growth ~ Species*depth*depth*pc1+(1|site/tile_id),data = pca_max)
anova(model1)
summ(model1)


model1 = lmer(growth ~ Species+depth*pc1+(1|site/tile_id),data = pca_max)
anova(model1)
summ(model1)

anova(model0,model1)

model2 = lmer(growth ~ Species*depth*pc1+(1|site/tile_id),data = pca_max)
anova(model2)
summ(model2)

####outlier identification#####
#####one value is completely off####

          
    
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


####Correlation with survival rate####

#traits
#species identity * depth
#growth


data_survival=analysis_data %>% group_by(Species,depth_treatment,site_code) %>% summarise(growth =mean(dry_weight_g_daily_relative_increase,na.rm=T),growth_sd =sd(dry_weight_g_daily_relative_increase,na.rm=T),
                                                                  trait_STA=mean(trait_STA,na.rm=T),
                                                                  trait_thickness=mean(trait_thickness,na.rm=T),
                                                                  trait_tdmc=mean(trait_tdmc,na.rm=T),
                                                                  survival_rate=mean(survived,na.rm=T)*100)
lm1=lm(survival_rate ~ Species*factor(depth_treatment),data=data_survival)
summary(aov(lm1))
lm1=lm(survival_rate ~ Species*factor(depth_treatment)+site_code,data=data_survival)
summary(aov(lm1))

ggboxplot(y="survival_rate" ,x="depth_treatment",color = "Species",data = data_survival)
ggboxplot(y="survival_rate" ,x="depth_treatment",color = "Species",facet.by = "site_code",data = data_survival)

summary(aov(lm1))
cor(data)

######Export for simulation
write.csv(analysis_data,"analysis_data/analysis_data_after_functioning_analysis.csv")

