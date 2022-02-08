###Simulation of sortings###

#library
library(dplyr)
library(ggplot2)

source("functions/function_plotting_theme.R")

#import dataset
#this will be created at the end of "functioning_analysis.R"
data=read.csv("analysis_data/analysis_data_after_functioning_analysis.csv")

#
data=data %>% filter(!is.na(dry_weight_g_daily_relative_increase))

data = data %>% select(binomial_code,depth_treatment,dry_weight_g_daily_relative_increase)

colnames(data) = c("species","depth","functioning")

mean_func = data %>% group_by(species,depth) %>% summarise(m.functioning=mean(functioning))

#using the mean of all
species_function_summary = function(species.config,mean_func){
depths = c(-40,-28,-12,-5)

funct=rbind(
mean_func %>% filter(species == species.config[1],depth==depths[1]),
mean_func %>% filter(species == species.config[2],depth==depths[2]),
mean_func %>% filter(species == species.config[3],depth==depths[3]),
mean_func %>% filter(species == species.config[4],depth==depths[4]))
data.frame(richness=length(unique(species.config)),mean_function=mean(funct$m.functioning),config=paste(species.config[1],
                                                        species.config[2],
                                                        species.config[3],
                                                        species.config[4]))

}

#Scenarios

scenarios = rbind(
  species_function_summary(c("fu_se","as_no","fu_ve","fu_sp"),mean_func = mean_func),
  #mono
  species_function_summary(c("fu_se","fu_se","fu_se","fu_se"),mean_func = mean_func),
  species_function_summary(c("as_no","as_no","as_no","as_no"),mean_func = mean_func),
  species_function_summary(c("fu_ve","fu_ve","fu_ve","fu_ve"),mean_func = mean_func),
  species_function_summary(c("fu_sp","fu_sp","fu_sp","fu_sp"),mean_func = mean_func),
  #"realistic" two species
  species_function_summary(c("fu_se","fu_se","fu_sp","fu_sp"),mean_func = mean_func),
  species_function_summary(c("fu_se","fu_se","fu_ve","fu_ve"),mean_func = mean_func),
  species_function_summary(c("fu_se","fu_se","as_no","as_no"),mean_func = mean_func),
  species_function_summary(c("as_no","as_no","fu_sp","fu_sp"),mean_func = mean_func),
  species_function_summary(c("as_no","as_no","fu_ve","fu_ve"),mean_func = mean_func),
  species_function_summary(c("fu_ve","fu_ve","fu_sp","fu_sp"),mean_func = mean_func),
  
  #realistic three species
  species_function_summary(c("fu_se","fu_ve","fu_ve","fu_sp"),mean_func = mean_func),
  species_function_summary(c("fu_se","as_no","as_no","fu_sp"),mean_func = mean_func),
  species_function_summary(c("fu_se","fu_se","fu_ve","fu_sp"),mean_func = mean_func),
  species_function_summary(c("fu_se","as_no","fu_ve","fu_ve"),mean_func = mean_func),
  species_function_summary(c("fu_se","as_no","as_no","fu_ve"),mean_func = mean_func)
)
plot(scenarios$richness,scenarios$mean_function)


#uing supsamples of 3 and repeat 10 times
species_function_summary_sub_samples = function(data,species.config){
  depths = c(-40,-28,-12,-5)
  sub.sample.size= 3
  repeats = 20
  
  res=data.frame(richness=numeric(),mean_function=numeric(),config=character())
  
  for( i in 1:repeats) {
    
    #picking always a few of each plant
    funct=rbind(
      data %>% filter(species == species.config[1],depth==depths[1]) %>% sample_n(sub.sample.size) %>% group_by(species,depth) %>% summarise(m.functioning=mean(functioning)),
      data %>% filter(species == species.config[2],depth==depths[2]) %>% sample_n(sub.sample.size) %>% group_by(species,depth) %>% summarise(m.functioning=mean(functioning)),
      data %>% filter(species == species.config[3],depth==depths[3]) %>% sample_n(sub.sample.size) %>% group_by(species,depth) %>% summarise(m.functioning=mean(functioning)),
      data %>% filter(species == species.config[4],depth==depths[4]) %>% sample_n(sub.sample.size) %>% group_by(species,depth) %>% summarise(m.functioning=mean(functioning)))
    
    #summerize functioning across gradient
    res=rbind(res,data.frame(richness=length(unique(species.config)),mean_function=mean(funct$m.functioning),config=paste(species.config[1],species.config[2],species.config[3],species.config[4])))
  }
  res
}




scenarios = rbind(
  species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_sp"),data = data),
  species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_sp"),data = data),
  
  #mono
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_se"),data = data),
  species_function_summary_sub_samples(c("as_no","as_no","as_no","as_no"),data = data),
  species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_ve"),data = data),
  species_function_summary_sub_samples(c("fu_sp","fu_sp","fu_sp","fu_sp"),data = data),
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_se"),data = data),
  species_function_summary_sub_samples(c("as_no","as_no","as_no","as_no"),data = data),
  species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_ve"),data = data),
  species_function_summary_sub_samples(c("fu_sp","fu_sp","fu_sp","fu_sp"),data = data),
  #"realistic" two species
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_sp","fu_sp"),data = data),
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_ve","fu_ve"),data = data),
  species_function_summary_sub_samples(c("fu_se","fu_se","as_no","as_no"),data = data),
  species_function_summary_sub_samples(c("as_no","as_no","fu_sp","fu_sp"),data = data),
  species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_ve"),data = data),
  species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_sp","fu_sp"),data = data),
  
  #realistic three species
  species_function_summary_sub_samples(c("fu_se","fu_ve","fu_ve","fu_sp"),data = data),
  species_function_summary_sub_samples(c("fu_se","as_no","as_no","fu_sp"),data = data),
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_ve","fu_sp"),data = data),
  species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_ve"),data = data),
  species_function_summary_sub_samples(c("fu_se","as_no","as_no","fu_ve"),data = data)
)

#plot(scenarios$richness,scenarios$mean_function)
ggplot(scenarios, aes(richness, mean_function))+
  geom_point(shape = 1) + stat_smooth(method = "lm", formula = y ~ log(x))+theme_classic()
