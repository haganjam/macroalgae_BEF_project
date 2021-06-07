####Libraries####
library(googlesheets4)
library(ggpubr)
library(dplyr)


######Data Import#####
allo_dat <- read_sheet("https://docs.google.com/spreadsheets/d/167zCNjbmZ1PV5V1vZeGe9QcIrvhJ4diZ8q2rF9Ggry0/edit#gid=0", sheet = "Sheet1")

allo_dat.fu_se = allo_dat %>% filter(binomial == "fucus_serratus")
allo_dat.as_no = allo_dat %>% filter(binomial == "ascophyllum_nodosum")

#####Fucus Serratus####

#dryweight ~ wetweight
mod1=lm(allo_dat.fu_se$dry_weight_g ~ allo_dat.fu_se$wet_weight_g)
summary(mod1)

#Predicting Dryweight
allo_dat.fu_se$pred.dryweight = predict(mod1,newdata = data.frame(allo_dat.fu_se$wet_weight_g))

#Cylindrical volume
allo_dat.fu_se$radius = allo_dat.fu_se$circum_cm/(2*pi)
allo_dat.fu_se$cyl.vol = allo_dat.fu_se$radius^2*pi*allo_dat.fu_se$length_cm



#Creating formula log(ww) ~ log(cyl.vol)
mod2=lm(log(allo_dat.fu_se$wet_weight_g) ~ log(allo_dat.fu_se$cyl.vol))
summary(mod2)
#plot(mod2) #ok

coef(mod2)


#plotting
res= data.frame(log.ww=(log(allo_dat.fu_se$wet_weight_g)),log.cyl.vol=log(allo_dat.fu_se$cyl.vol))
ggscatter(data=res, y="log.ww",x="log.cyl.vol",add = "reg.line")

#How accurate?
fitted(mod2)
pred_fu_se=as.data.frame(predict(mod2, newdata = data.frame(log(allo_dat.fu_se$cyl.vol)), interval = "confidence"))
pred_fu_se=pred_fu_se[order(pred_fu_se$fit),]
pred_fu_se=cbind(pred_fu_se,name=letters[1:length(pred_fu_se$fit)])

pred_fu_se$fit=exp(pred_fu_se$fit)
pred_fu_se$lwr=exp(pred_fu_se$lwr)
pred_fu_se$upr=exp(pred_fu_se$upr)

ggplot(pred_fu_se) +
  geom_bar( aes(x=name, y=fit), stat="identity", fill="black", alpha=0.5) +
  geom_errorbar( aes(x=name, ymin=lwr, ymax=upr), width=0.3, colour="orange", alpha=0.9, size=0.5)

#######Formula of Buchanan for Serratus
#loggedDW ~ loggedVol(cylinder)
buchanan_dw = exp(0.97 * log(allo_dat.fu_se$cyl.vol)-3.2)

cor(buchanan_dw,allo_dat.fu_se$pred.dryweight)^2



####Ascophyllum Nodosum#######
mod1=lm(allo_dat.as_no$dry_weight_g ~ allo_dat.as_no$wet_weight_g)
summary(mod1)

plot(allo_dat.as_no$wet_weight_g,allo_dat.as_no$dry_weight_g)


#Predicting Dryweight
allo_dat.as_no$pred.dryweight = predict(mod1,newdata = data.frame(allo_dat.as_no$wet_weight_g))

#Cylindrical volume
allo_dat.as_no$radius = allo_dat.as_no$circum_cm/(2*pi)
allo_dat.as_no$cyl.vol = allo_dat.as_no$radius^2*pi*allo_dat.as_no$length_cm



#Creating formula log(ww) ~ log(cyl.vol)
mod2=lm(log(allo_dat.as_no$dry_weight_g) ~ log(allo_dat.as_no$cyl.vol))
summary(mod2)
#plot(mod2) #ok


res= data.frame(log.dw=(log(allo_dat.as_no$dry_weight_g)),log.cyl.vol=log(allo_dat.as_no$cyl.vol))
ggscatter(data=res, y="log.dw",x="log.cyl.vol",add = "reg.line")

#How accurate?
fitted(mod2)
pred_as_no=as.data.frame(predict(mod2, newdata = data.frame(log(allo_dat.as_no$cyl.vol)), interval = "confidence"))
pred_as_no=pred_as_no[order(pred_as_no$fit),]
pred_as_no=cbind(pred_as_no,name=letters[1:length(pred_as_no$fit)])

pred_as_no$fit=exp(pred_as_no$fit)
pred_as_no$lwr=exp(pred_as_no$lwr)
pred_as_no$upr=exp(pred_as_no$upr)

ggplot(pred_as_no) +
  geom_bar( aes(x=name, y=fit), stat="identity", fill="black", alpha=0.5) +
  geom_errorbar( aes(x=name, ymin=lwr, ymax=upr), width=0.3, colour="orange", alpha=0.9, size=0.5)


