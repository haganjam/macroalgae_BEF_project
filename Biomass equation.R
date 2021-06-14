####Libraries####
library(googlesheets4)
library(ggpubr)
library(dplyr)

######Data Import#####
allo_dat <- read_sheet("https://docs.google.com/spreadsheets/d/167zCNjbmZ1PV5V1vZeGe9QcIrvhJ4diZ8q2rF9Ggry0/edit#gid=0", sheet = "Sheet1")

allo_dat.fu_se = allo_dat %>% filter(binomial == "fucus_serratus")
allo_dat.as_no = allo_dat %>% filter(binomial == "ascophyllum_nodosum")
allo_dat.fu_sp = allo_dat %>% filter(binomial_code == "fu_sp")
allo_dat.fu_ve = allo_dat %>% filter(binomial_code == "fu_ve")

##### Fucus Serratus####


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


#Creating formula log(dw) ~ log(length^3)
mod2=lm(log(allo_dat.fu_se$dry_weight_g) ~ log(allo_dat.fu_se$length_cm))
summary(mod2)
plot(mod2)
plot(log(allo_dat.fu_se$dry_weight_g) ,log(allo_dat.fu_se$length_cm))


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



##### Ascophyllum Nodosum#######
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

#A model with length
mod2=lm(log(allo_dat.as_no$dry_weight_g) ~ log(allo_dat.as_no$length_cm*(allo_dat.as_no$circum_cm^2)))
summary(mod2)



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



##### Fucus vesiculosus-----

mod1=lm(allo_dat.fu_ve$dry_weight_g ~ allo_dat.fu_ve$wet_weight_g)
summary(mod1)

plot(allo_dat.fu_ve$wet_weight_g,allo_dat.fu_ve$dry_weight_g)

#Cylindrical volume
allo_dat.fu_ve$radius = allo_dat.fu_ve$circum_cm/(2*pi)
allo_dat.fu_ve$cyl.vol = allo_dat.fu_ve$radius^2*pi*allo_dat.fu_ve$length_cm


#Creating formula log(ww) ~ log(cyl.vol)
mod2=lm(log(allo_dat.fu_ve$dry_weight_g) ~ log(allo_dat.fu_ve$cyl.vol))
summary(mod2)
#plot(mod2) #ok


res= data.frame(log.dw=(log(allo_dat.fu_ve$dry_weight_g)),log.cyl.vol=log(allo_dat.fu_ve$cyl.vol))
ggscatter(data=res, y="log.dw",x="log.cyl.vol",add = "reg.line")

#How accurate?
fitted(mod2)
pred_fu_ve=as.data.frame(predict(mod2, newdata = data.frame(log(allo_dat.fu_ve$cyl.vol)), interval = "confidence"))
pred_fu_ve=pred_fu_ve[order(pred_fu_ve$fit),]
pred_fu_ve=cbind(pred_fu_ve,name=letters[1:length(pred_fu_ve$fit)])

pred_fu_ve$fit=exp(pred_fu_ve$fit)
pred_fu_ve$lwr=exp(pred_fu_ve$lwr)
pred_fu_ve$upr=exp(pred_fu_ve$upr)

ggplot(pred_fu_ve) +
  geom_bar( aes(x=name, y=fit), stat="identity", fill="black", alpha=0.5) +
  geom_errorbar( aes(x=name, ymin=lwr, ymax=upr), width=0.3, colour="orange", alpha=0.9, size=0.5)



##### Fucus spiralis -------

mod1=lm(allo_dat.fu_sp$dry_weight_g ~ allo_dat.fu_sp$wet_weight_g)
summary(mod1)

plot(allo_dat.fu_sp$wet_weight_g,allo_dat.fu_sp$dry_weight_g)

#Cylindrical volume
allo_dat.fu_sp$radius = allo_dat.fu_sp$circum_cm/(2*pi)
allo_dat.fu_sp$cyl.vol = allo_dat.fu_sp$radius^2*pi*allo_dat.fu_sp$length_cm


#Creating formula log(ww) ~ log(cyl.vol)
mod2=lm(log(allo_dat.fu_sp$dry_weight_g) ~ log(allo_dat.fu_sp$cyl.vol))
summary(mod2)
#plot(mod2) #ok
res= data.frame(log.dw=(log(allo_dat.fu_sp$dry_weight_g)),log.cyl.vol=log(allo_dat.fu_sp$cyl.vol))
ggscatter(data=res, y="log.dw",x="log.cyl.vol",add = "reg.line")


#just length
mod3=lm(log(allo_dat.fu_sp$dry_weight_g) ~ log(allo_dat.fu_sp$length_cm))
summary(mod3)
res= data.frame(log.dw=(log(allo_dat.fu_sp$dry_weight_g)),log.length_cm=log(allo_dat.fu_sp$length_cm))
ggscatter(data=res, y="log.dw",x="log.length_cm",add = "reg.line")


#How accurate?
fitted(mod2)
pred_fu_sp=as.data.frame(predict(mod2, newdata = data.frame(log(allo_dat.fu_sp$cyl.vol)), interval = "confidence"))
pred_fu_sp=pred_fu_sp[order(pred_fu_sp$fit),]
pred_fu_sp=cbind(pred_fu_sp,name=letters[1:length(pred_fu_sp$fit)])

pred_fu_sp$fit=exp(pred_fu_sp$fit)
pred_fu_sp$lwr=exp(pred_fu_sp$lwr)
pred_fu_sp$upr=exp(pred_fu_sp$upr)

ggplot(pred_fu_sp) +
  geom_bar( aes(x=name, y=fit), stat="identity", fill="black", alpha=0.5) +
  geom_errorbar( aes(x=name, ymin=lwr, ymax=upr), width=0.3, colour="orange", alpha=0.9, size=0.5)







##Comparison##
allo_dat$dw_ww = allo_dat$dry_weight_g / allo_dat$wet_weight_g

ggboxplot(allo_dat, x="binomial",y="dw_ww") +rotate_x_text(70)


ggscatter(allo_dat,x="length_cm",y="circum_cm",facet.by = "binomial_code",add="reg.line")

ggscatter(allo_dat,x="length_cm",y="dw_ww",facet.by = "binomial_code",add="reg.line")

allo_dat$cyl.vol = allo_dat$length_cm*pi* (allo_dat$circum_cm/(2*pi))^2 

ggscatter(allo_dat,x="length_cm",y="dw_ww",facet.by = "binomial_code",add="reg.line")

ggscatter(allo_dat,x="dry_weight_g",y="cyl.vol",facet.by = "binomial_code")
ggscatter(allo_dat,x="wet_weight_g",y="cyl.vol",facet.by = "binomial_code")



# Random trials ----




plot(allo_dat$dry_weight_g[allo_dat$binomial=="fucus_spiralis"],allo_dat$wet_weight_g[allo_dat$binomial=="fucus_spiralis"])
plot(allo_dat$dry_weight_g[allo_dat$binomial=="fucus_vesiculosus"],allo_dat$wet_weight_g[allo_dat$binomial=="fucus_vesiculosus"])


plot(allo_dat$dry_weight_g[allo_dat$binomial_code=="fu_ve"],allo_dat$wet_weight_g[allo_dat$binomial_code=="fu_ve"],ylim=c(0,150))

a=allo_dat %>% filter(binomial_code=="fu_ve",dry_weight_g<5, wet_weight_g>25 )


plot(allo_dat$dry_weight_g[allo_dat$binomial_code=="fu_sp"],allo_dat$wet_weight_g[allo_dat$binomial_code=="fu_sp"])



