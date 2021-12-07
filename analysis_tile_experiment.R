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
library(lme4)
library(lmerTest)
library(purrr)
library(tidyr)
library(ggplot2)
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


pca_res <- prcomp(pca_max[-c(1,2,3)], scale = TRUE)

autoplot(pca_res, data = pca_max, shape = 'depth', size=3,,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour="black",colour="Species")  + theme_bw()


autoplot(pca_res, data = pca_max, shape = 'site', size=3,,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 4,
         loadings.label.colour="black",colour="Species")  + theme_bw()




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

### preliminary data analysis ###

analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_area_cm2)) %>% kruskal_test(growth_area_cm2~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_length_cm)) %>% kruskal_test(growth_length_cm~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_perimeter_cm)) %>% kruskal_test(growth_perimeter_cm~depth_treatment)
analysis_data %>% group_by(binomial_code) %>% filter(!is.na(growth_wet_weight_g)) %>% kruskal_test(growth_wet_weight_g~depth_treatment)


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

pca_growth = na.omit(analysis_data %>% select(Species,growth_area_cm2_percent:growth_wet_weight_g_percent))

pca_res <- prcomp(pca_growth[-c(1)], scale = TRUE)

#How much do the growths correlate to each other
autoplot(pca_res, data = pca_growth, colour = 'Species',
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,loadings.label.colour="black")

#Species
model1=lm(growth_area_cm2_percent ~ Species+origin_,data=analysis_data)
summary(aov(model1))
eta_squared(model1)

#Does origin site matter
model1=lm(growth_area_cm2_percent ~ Species+origin_site_code,data=analysis_data)
summary(aov(model1))
eta_squared(model1)

#Does transplant site matter?
model1=lm(growth_area_cm2_percent ~ Species*depth_treatment+origin_site_code+site_code+tile_id,data=analysis_data)
summary(aov(model1))
eta_squared(model1)

###Linear mixed effects model:
model1 = lmer(growth_area_cm2_percent ~ Species*factor(depth_treatment) + (1|origin_site_code)+(1|site_code/tile_id),data = analysis_data)
anova(model1)

summary(model1)

library(sjstats)
library(MuMin)

library(emmeans)
emm=emmeans(model1, list(pairwise ~ factor(depth_treatment)/Species), adjust = "tukey")

#emmeants plot
emm=as.data.frame(emm$`emmeans of depth_treatment, Species`)
ggbarplot(data = emm,x="depth_treatment",y="emmean",fill="Species",position = position_dodge(0.9))

# Convert dose to a factor variable
df2$dose=as.factor(df2$dose)
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

#GAM does not work because there are only 4 depths....
gam.alg = mgcv::gam(growth_area_cm2_percent ~ s(depth_treatment),data=analysis_data)
plot(gam.alg)
library(voxel)
plotGAM(gamFit = gam.alg)

plot(gam.alg, se=TRUE,col="red")


data <- data.frame(x = rep(1:20, 2), group = rep(1:2, each = 20))
set.seed(1)
data$y <- (data$x^2)*data$group*3 + rnorm(40, sd = 200)
data$group <- ordered(data$group)

gam <- mgcv::gam(y ~ s(x) + group, data=data)

plot1 <- plotGAM(gamFit = gam, smooth.cov = "x", groupCovs = NULL,
                 rawOrFitted = "raw", plotCI=TRUE, orderedAsFactor = FALSE)
gam <- mgcv::gam(y ~ s(x) + group + s(x, by=group), data=data)
plot2 <- plotGAM(gamFit = gam, smooth.cov = "x", groupCovs = "group",
                 rawOrFitted = "raw", orderedAsFactor = FALSE)



#rainbow plot

####Trait environment intreraction####


library(lme4)
library(lmerTest)

mod1=lm(growth_wet_weight_g ~ depth_treatment*Species,data = analysis_data)


summary(aov(mod1))
mod1=lmer(growth_wet_weight_g_percent ~ depth_treatment*Species+(1|tile_id),data = analysis_data)
anova((mod1))
summary(aov(mod1))


mod1=lmer(growth_wet_weight_g_percent ~ depth_treatment*Species+depth_treatment:(trait_float+trait_SAP+trait_STA+trait_thickness+trait_tdmc)+(1|tile_id),data = analysis_data)
anova((mod1))

vif(mod1)


ggscatter(analysis_data,y="growth_wet_weight_g_percent",x="trait_SAP",facet.by = "Species",color = "depth_treatment")
ggscatter(analysis_data,y="growth_wet_weight_g_percent",x="trait_STA",facet.by = "Species",color = "depth_treatment")
ggscatter(analysis_data,y="growth_wet_weight_g_percent",x="trait_tdmc",facet.by = "Species",color = "depth_treatment")



depth_treatment







######Random combination vs natural combination#####
#here not nested in sites

#Boot size
samplesize = 100
comm_sim = data.frame(type = character(),growth = numeric())

species = as.character(unique(analysis_data$Species))

for(i in 1:samplesize){
message(paste("Step", i, "of", samplesize))

##Pick one plant on each depth

#Random 4
temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g)) %>% sample_n(1),
  analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g)) %>% sample_n(1),
  analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g)) %>% sample_n(1),
  analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g)) %>% sample_n(1))

comm_sim = rbind(comm_sim, data.frame(type = "random_4",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community)

#Random 3
randsubset_species = sample_n(data.frame(species),3)
#sample_n(randsubset_species,1)[,1] #pick one of the 3

temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1))

comm_sim = rbind(comm_sim, data.frame(type = "random_3",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community,randsubset_species)


#Random 2
randsubset_species = sample_n(data.frame(species),2)
#sample_n(randsubset_species,1)[,1] #pick one of the 2

temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1))

comm_sim = rbind(comm_sim, data.frame(type = "random_2",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community,randsubset_species)

#Random 1
randsubset_species = sample_n(data.frame(species),1)
#sample_n(randsubset_species,1)[,1] #pick 1 of 1

temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species==sample_n(randsubset_species,1)[,1]) %>% sample_n(1))

comm_sim = rbind(comm_sim, data.frame(type = "random_1",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community,randsubset_species)


##Calculate sorted version
temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species=="Fucus spiralis") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species=="Fucus vesiculosus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species=="Ascophyllum nodosum") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species=="Fucus serratus") %>% sample_n(1))
comm_sim = rbind(comm_sim, data.frame(type = "sorted",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community)


##Calculate monoculture version
temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species=="Fucus spiralis") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species=="Fucus spiralis") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species=="Fucus spiralis") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species=="Fucus spiralis") %>% sample_n(1))
comm_sim = rbind(comm_sim, data.frame(type = "mono_fu_sp",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community)


temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species=="Fucus vesiculosus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species=="Fucus vesiculosus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species=="Fucus vesiculosus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species=="Fucus vesiculosus") %>% sample_n(1))
comm_sim = rbind(comm_sim, data.frame(type = "mono_fu_ve",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community)

temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species=="Ascophyllum nodosum") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species=="Ascophyllum nodosum") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species=="Ascophyllum nodosum") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species=="Ascophyllum nodosum") %>% sample_n(1))
comm_sim = rbind(comm_sim, data.frame(type = "mono_as_no",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community)

temp_community = rbind(analysis_data %>% filter(depth_treatment == -5,!is.na(growth_wet_weight_g),Species=="Fucus serratus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -12,!is.na(growth_wet_weight_g),Species=="Fucus serratus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -28,!is.na(growth_wet_weight_g),Species=="Fucus serratus") %>% sample_n(1),
                       analysis_data %>% filter(depth_treatment == -40,!is.na(growth_wet_weight_g),Species=="Fucus serratus") %>% sample_n(1))
comm_sim = rbind(comm_sim, data.frame(type = "mono_fu_se",growth=sum(temp_community$growth_wet_weight_g)))
rm(temp_community)

}

boxplot(comm_sim$growth ~ comm_sim$type)

#########----###########


comm_sim_2 = data.frame(heterogeniety = numeric(),species = numeric(),growth = numeric())

species = as.character(unique(analysis_data$Species))
species = data.frame(species)

depths = data.frame(depth=c(-5,-12,-28,-40))
samplesize = 100


for(i in 1:samplesize){
  message(paste("Step", i, "of", samplesize))
  

### Pick one plant ### grow at 1 place ###

randsubset_species = sample_n(species,4)
randsubset_depths = sample_n(depths,1)

#one species at 1 place
temp_comm=analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4)

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 1,species = 1,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)

#Several species at 1 place
temp_comm=analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4)

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 1,species = 4,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)

#one species at 2 places average
randsubset_species = sample_n(species,4)
randsubset_depths = sample_n(depths,2)

temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4))

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 2,species = 1,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)

#several species at 2 places
temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4))

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 2,species = 4,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)


#one species at 3 places average
randsubset_species = sample_n(species,4)
randsubset_depths = sample_n(depths,3)

temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4))

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 3,species = 1,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)

# several species at 4 places

temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4))

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 3,species = 4,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)

#one species at 4 places average
randsubset_species = sample_n(species,4)
randsubset_depths = sample_n(depths,4)

temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[4,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4))

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 4,species = 1,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)

#several species at 4 places
temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                  analysis_data %>% filter(depth_treatment == randsubset_depths[4,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4))

comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 4,species = 4,growth = median(temp_comm$growth_wet_weight_g_percent)))
rm(temp_comm)



}


comm_sim_2_median = comm_sim_2

ggboxplot(comm_sim_2_median, y="growth", x="heterogeniety",facet.by = "species")


# Maximum function
comm_sim_2 = data.frame(heterogeniety = numeric(),species = numeric(),growth = numeric())

species = as.character(unique(analysis_data$Species))
species = data.frame(species)

depths = data.frame(depth=c(-5,-12,-28,-40))
samplesize = 1000


for(i in 1:samplesize){
  message(paste("Step", i, "of", samplesize))
  
  
  ### Pick one plant ### grow at 1 place ###
  
  randsubset_species = sample_n(species,4)
  randsubset_depths = sample_n(depths,1)
  
  #one species at 1 place
  temp_comm=analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4)
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 1,species = 1,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  #Several species at 1 place
  temp_comm=analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4)
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 1,species = 4,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  #one species at 2 places average
  randsubset_species = sample_n(species,4)
  randsubset_depths = sample_n(depths,2)
  
  temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4))
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 2,species = 1,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  #several species at 2 places
  temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4))
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 2,species = 4,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  
  #one species at 3 places average
  randsubset_species = sample_n(species,4)
  randsubset_depths = sample_n(depths,3)
  
  temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4))
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 3,species = 1,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  # several species at 4 places
  
  temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4))
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 3,species = 4,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  #one species at 4 places average
  randsubset_species = sample_n(species,4)
  randsubset_depths = sample_n(depths,4)
  
  temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[4,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,]) %>% sample_n(4))
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 4,species = 1,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  #several species at 4 places
  temp_comm = rbind(analysis_data %>% filter(depth_treatment == randsubset_depths[1,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[2,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[3,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4),
                    analysis_data %>% filter(depth_treatment == randsubset_depths[4,],!is.na(growth_wet_weight_g),Species==randsubset_species[1,] | Species==randsubset_species[2,] | Species==randsubset_species[3,] | Species==randsubset_species[4,]) %>% sample_n(4))
  
  comm_sim_2 = rbind(comm_sim_2,data.frame(heterogeniety = 4,species = 4,growth = max(temp_comm$growth_wet_weight_g_percent)))
  rm(temp_comm)
  
  
  
}

comm_sim_2_max = comm_sim_2

ggboxplot(comm_sim_2_max, y="growth", x="heterogeniety",facet.by = "species")




#loop approach


# create a 16 fields plot with heterogeniety 1 - 4
# plant out monoculture on all 16
# plant out multiculture on all 16

species = as.character(unique(analysis_data$Species))
species = data.frame(species)

depths = data.frame(depth=c(-5,-12,-28,-40))



#set the depths that will be used
depths_1 = c(1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1) #set depths
depths_2 = c(1,1,1,1, 1,1,1,1, 2,2,2,2, 2,2,2,2) #set depths
depths_3 = c(1,1,1,1, 2,2,2,2, 3,3,3,3, 1,2,3,1) #set depths # there is one more 1, but this will even out 
depths_4 = c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4) #set depths
depth_configurations= list(depths_1,depths_2,depths_3,depths_4)

#set the species that will be used
species_1 = c(1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1) #set species
species_2 = c(1,1,1,1, 1,1,1,1, 2,2,2,2, 2,2,2,2) #set species
species_3 = c(1,2,3,1, 2,3,1,2, 3,1,2,3, 1,2,3,1) #set species # there is one more 1, but this will even out 
species_4 = c(1,1,1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4) #set species
species_configurations= list(species_1,species_2,species_3,species_4)



# create basic dataframe for simulation
comm_sim_3 = data.frame(heterogeniety = numeric(),nspecies = numeric(),growth = numeric())

#number of iterations
nreps= 200

#big loop
for(repeats in 1:nreps){
message(paste(100*repeats/nreps, "% completed"))

#shuffle species configuration
species_configurations[[1]]=species_configurations[[1]][sample(16)]
species_configurations[[2]]=species_configurations[[2]][sample(16)]
species_configurations[[3]]=species_configurations[[3]][sample(16)]
species_configurations[[4]]=species_configurations[[4]][sample(16)]

#shuffle species and depths, so different species and depths are combined
species = data.frame(species=c("Fucus spiralis","Fucus vesiculosus","Ascophyllum nodosum", "Fucus serratus")[sample(4)])
#message(species)
depths = data.frame(depth=c(-5,-12,-28,-40)[sample(4)])
#message(depths)


###Create 16 fields with a number of species and 
# here 16 plants from the same or n different species (configuration), will be assigned to m different depths

for (spec in 1:4){
  for (het in 1:4){
    for(i in 1:16){
      #message(paste(het,spec,i))
      temp_comm = analysis_data %>% sample_n(0) # get empty table
      
      pick = analysis_data %>% filter(!is.na(growth_wet_weight_g),
                                      Species == species[species_configurations[[spec]][i],], #get the species
                                      depth_treatment == depths[depth_configurations[[het]][i],]) %>%  sample_n(1) #get the depth
       
      temp_comm = rbind(temp_comm,pick)
      
      rm(pick)
      
    }
    comm_sim_3 = rbind(comm_sim_3, data.frame(heterogeniety = het,nspecies = spec,growth = sum(temp_comm$growth_wet_weight_g)))
    rm(temp_comm)
  }
  }
}

comm_sim_3$heterogeniety=as.factor(comm_sim_3$heterogeniety)
ggboxplot(comm_sim_3,y="growth",color="heterogeniety",x = "nspecies")
ggscatter(comm_sim_3,y="growth",color="heterogeniety",x = "nspecies",add = "reg.line")

#16 in monoculture
table(comm_sim_3$growth)




















                                                                                                                                                                    