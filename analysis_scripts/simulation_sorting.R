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

boxplot(data$functioning~data$species) # we could standardize value within species...

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
species_function_summary_sub_samples = function(data,species.config,repeats){
  depths = c(-40,-28,-12,-5)
  sub.sample.size= 3

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
  #4 species
  species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_sp"),data = data,repeats = 120),

  #monoculture
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_se"),data = data,repeats = 30),
  species_function_summary_sub_samples(c("as_no","as_no","as_no","as_no"),data = data,repeats = 30),
  species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_ve"),data = data,repeats = 30),
  species_function_summary_sub_samples(c("fu_sp","fu_sp","fu_sp","fu_sp"),data = data,repeats = 30),


  #"realistic" two species, sorting is maintained
#fu_se and fu_sp
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_sp","fu_sp"),data = data, repeats = 10),
  species_function_summary_sub_samples(c("fu_se","fu_sp","fu_sp","fu_sp"),data = data, repeats = 10),
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_sp"),data = data, repeats = 10),
  
#fu_se and fu_ve
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_ve","fu_ve"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("fu_se","fu_ve","fu_ve","fu_ve"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_ve"),data = data,repeats = 10),
  
#fu_se and as_no
  species_function_summary_sub_samples(c("fu_se","fu_se","as_no","as_no"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("fu_se","as_no","as_no","as_no"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","as_no"),data = data,repeats = 10),
  
#as_no and fu_sp
  species_function_summary_sub_samples(c("as_no","as_no","fu_sp","fu_sp"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("as_no","fu_sp","fu_sp","fu_sp"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_sp"),data = data,repeats = 10),
  
#as_no and fu_ve
  species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_ve"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_ve"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_ve"),data = data,repeats = 10),
  
#fu_ve and fu_sp
  species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_sp","fu_sp"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_sp"),data = data,repeats = 10),
  species_function_summary_sub_samples(c("fu_ve","fu_sp","fu_sp","fu_sp"),data = data,repeats = 10),

  #"realistic" three species
#fu_sp out
species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_ve"),data = data,repeats = 10),
species_function_summary_sub_samples(c("fu_se","as_no","as_no","fu_ve"),data = data,repeats = 10),
species_function_summary_sub_samples(c("fu_se","fu_se","as_no","fu_ve"),data = data,repeats = 10),

#fu_ve out
species_function_summary_sub_samples(c("fu_se","fu_se","as_no","fu_sp"),data = data,repeats = 10),
species_function_summary_sub_samples(c("fu_se","as_no","as_no","fu_sp"),data = data,repeats = 10),
species_function_summary_sub_samples(c("fu_se","as_no","fu_sp","fu_sp"),data = data,repeats = 10),

#as_no out
species_function_summary_sub_samples(c("fu_se","fu_se","fu_ve","fu_sp"),data = data,repeats = 10),
species_function_summary_sub_samples(c("fu_se","fu_ve","fu_ve","fu_sp"),data = data,repeats = 10),
species_function_summary_sub_samples(c("fu_se","fu_ve","fu_sp","fu_sp"),data = data,repeats = 10),

#fu_se out
species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_sp"),data = data,repeats = 10),
species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_sp"),data = data,repeats = 10),
species_function_summary_sub_samples(c("as_no","fu_ve","fu_sp","fu_sp"),data = data,repeats = 10)
)

#plot(scenarios$richness,scenarios$mean_function)
ggplot(scenarios, aes(richness, mean_function))+
  geom_point(shape = 1) + stat_smooth(method = "lm", formula = y ~ log(x))+theme_classic()
table(scenarios$richness)


#Scaled approach
boxplot(data$functioning~data$species) # we could standardize value within species...to see if this slope comes just from the different functioning from species

data2=data %>%
  group_by(species) %>%
  mutate(functioning = scale(functioning))

  boxplot(data2$functioning~data2$species) # we could standardize value within species...to see if this slope comes just from the different functioning from species

  scenarios = rbind(
    #4 species
    species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_sp"),data = data2,repeats = 120),
    
    #monoculture
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_se"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("as_no","as_no","as_no","as_no"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_ve"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("fu_sp","fu_sp","fu_sp","fu_sp"),data = data2,repeats = 30),
    
    
    #"realistic" two species, sorting is maintained
    #fu_se and fu_sp
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_sp","fu_sp"),data = data2, repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_sp","fu_sp","fu_sp"),data = data2, repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_sp"),data = data2, repeats = 10),
    
    #fu_se and fu_ve
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_ve","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_ve","fu_ve","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","fu_ve"),data = data2,repeats = 10),
    
    #fu_se and as_no
    species_function_summary_sub_samples(c("fu_se","fu_se","as_no","as_no"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","as_no","as_no","as_no"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_se","as_no"),data = data2,repeats = 10),
    
    #as_no and fu_sp
    species_function_summary_sub_samples(c("as_no","as_no","fu_sp","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_sp","fu_sp","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_sp"),data = data2,repeats = 10),
    
    #as_no and fu_ve
    species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_ve"),data = data2,repeats = 10),
    
    #fu_ve and fu_sp
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_sp","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_ve","fu_sp","fu_sp","fu_sp"),data = data2,repeats = 10),
    
    #"realistic" three species
    #fu_sp out
    species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","as_no","as_no","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_se","as_no","fu_ve"),data = data2,repeats = 10),
    
    #fu_ve out
    species_function_summary_sub_samples(c("fu_se","fu_se","as_no","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","as_no","as_no","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","as_no","fu_sp","fu_sp"),data = data2,repeats = 10),
    
    #as_no out
    species_function_summary_sub_samples(c("fu_se","fu_se","fu_ve","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_ve","fu_ve","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_se","fu_ve","fu_sp","fu_sp"),data = data2,repeats = 10),
    
    #fu_se out
    species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_sp","fu_sp"),data = data2,repeats = 10)
  )
  
  ggplot(scenarios, aes(richness, mean_function))+
    geom_point(shape = 1) + stat_smooth(method = "lm", formula = y ~ log(x))+theme_classic()
  table(scenarios$richness)
  
  
#How does it look without fucus serratus?
  #uing supsamples of 3 and repeat 10 times
  species_function_summary_sub_samples = function(data,species.config,repeats){
    depths = c(-40,-28,-12,-5)
    sub.sample.size= 3
    
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
    #4 species
    species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_sp"),data = data,repeats = 120),
    
    #monoculture
    species_function_summary_sub_samples(c("as_no","as_no","as_no","as_no"),data = data,repeats = 30),
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_ve"),data = data,repeats = 30),
    species_function_summary_sub_samples(c("fu_sp","fu_sp","fu_sp","fu_sp"),data = data,repeats = 30),
    
    
    #"realistic" two species, sorting is maintained
  
    #as_no and fu_sp
    species_function_summary_sub_samples(c("as_no","as_no","fu_sp","fu_sp"),data = data,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_sp","fu_sp","fu_sp"),data = data,repeats = 10),
    species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_sp"),data = data,repeats = 10),
    
    #as_no and fu_ve
    species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_ve"),data = data,repeats = 10),
    species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_ve"),data = data,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_ve"),data = data,repeats = 10),
    
    #fu_ve and fu_sp
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_sp","fu_sp"),data = data,repeats = 10),
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_sp"),data = data,repeats = 10),
    species_function_summary_sub_samples(c("fu_ve","fu_sp","fu_sp","fu_sp"),data = data,repeats = 10),
    
    #"realistic" three species
    #fu_se out
    species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_sp"),data = data,repeats = 20),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_sp"),data = data,repeats = 20),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_sp","fu_sp"),data = data,repeats = 20)
  )
  
  #plot(scenarios$richness,scenarios$mean_function)
  ggplot(scenarios, aes(richness, mean_function))+
    geom_point(shape = 1) + stat_smooth(method = "lm", formula = y ~ log(x))+theme_classic()
  table(scenarios$richness)
  
  
#scaled without fucus serratus
  data2=data %>%
    group_by(species) %>%
    mutate(functioning = scale(functioning))
  
  boxplot(data2$functioning~data2$species) # we could standardize value within species...to see if this slope comes just from the different functioning from species
  
  scenarios = rbind(
    #4 species
    species_function_summary_sub_samples(c("fu_se","as_no","fu_ve","fu_sp"),data = data2,repeats = 90),
    
    #monoculture
    species_function_summary_sub_samples(c("as_no","as_no","as_no","as_no"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_ve"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("fu_sp","fu_sp","fu_sp","fu_sp"),data = data2,repeats = 30),
    
    
    #"realistic" two species, sorting is maintained

    #as_no and fu_sp
    species_function_summary_sub_samples(c("as_no","as_no","fu_sp","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_sp","fu_sp","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_sp"),data = data2,repeats = 10),
    
    #as_no and fu_ve
    species_function_summary_sub_samples(c("as_no","as_no","as_no","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_ve"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_ve"),data = data2,repeats = 10),
    
    #fu_ve and fu_sp
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_sp","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_ve","fu_ve","fu_ve","fu_sp"),data = data2,repeats = 10),
    species_function_summary_sub_samples(c("fu_ve","fu_sp","fu_sp","fu_sp"),data = data2,repeats = 10),
    
    #"realistic" three species
 
    #fu_se out
    species_function_summary_sub_samples(c("as_no","as_no","fu_ve","fu_sp"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_ve","fu_sp"),data = data2,repeats = 30),
    species_function_summary_sub_samples(c("as_no","fu_ve","fu_sp","fu_sp"),data = data2,repeats = 30)
  )
  
  ggplot(scenarios, aes(richness, mean_function))+
    geom_point(shape = 1) + stat_smooth(method = "lm", formula = y ~ log(x))+theme_classic()
  table(scenarios$richness)
  
  
  
