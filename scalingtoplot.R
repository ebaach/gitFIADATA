#Elizabeth Baach


# loading -----------------------------------------------------------------
#first lets load our packages
library(rFIA)
library(dplyr)
library(tidyverse)
library(sjstats)

compdata<- read.csv("compdata.csv", header = T)
compdata<- compdata %>% select(-c("X"))

propdata<- compdata

# converting to factors ---------------------------------------------------
propdata$phenology<-  as.factor(propdata$phenology)

propdata$n_fixer= factor(propdata$n_fixer)
levels(propdata$n_fixer) <- list("No"="no","Yes"=c("low", "medium"))
propdata$n_fixer <- gsub("low", "yes", propdata$n_fixer)
propdata$n_fixer <- gsub("medium", "yes", propdata$n_fixer)

propdata$life_span= factor(propdata$life_span, labels=c("short","moderate","long"))

propdata$growth_rate= factor(propdata$growth_rate, labels= c("slow","moderate","rapid"))

propdata$drought_tol<- as.factor(propdata$drought_tol)
levels(propdata$drought_tol) <- list("none"="none","low"="low", "moderate"=c("medium","moderate"), "high"="high")
propdata$drought_tol <- gsub("moderate", "medium", propdata$drought_tol)

propdata$fire_tol<- as.factor(propdata$fire_tol)
levels(propdata$fire_tol) <- list("none"="none","low"="low", "moderate"=c("medium","moderate"), "high"="high")
propdata$fire_tol <- gsub("moderate", "medium", propdata$fire_tol)

propdata$anaerobic_tol<- as.factor(propdata$anaerobic_tol)
levels(propdata$anaerobic_tol) <- list("none"="none","low"="low", "moderate"=c("medium","moderate"), "high"="high")
propdata$anaerobic_tol <- gsub("moderate", "medium", propdata$anaerobic_tol)

propdata$shade_tol<- as.factor(propdata$shade_tol)
levels(propdata$shade_tol) <- list("intolerant"="intolerant","intermediate"=c("low","moderate","intermediate"), "tolerant"=c("high","tolerant"))
propdata$shade_tol <- gsub("intermediate", "low", propdata$drought_tol)
propdata$shade_tol <- gsub("intermediate", "moderate", propdata$drought_tol)
propdata$shade_tol <- gsub("tolerant", "high", propdata$drought_tol)

propdata$fruit_type<- as.factor(propdata$fruit_type)
levels(propdata$fruit_type)<- list("cone"="cone", "soft"=c("aggregate of follicles","berry","drupe","legume","pome"), "hard"=c("ball(achene)","nut","capsule","samara","spiny ball"))
propdata$fruit_type <- gsub("soft","aggregate of follicles", propdata$fruit_type)
propdata$fruit_type <- gsub("soft","berry", propdata$fruit_type)
propdata$fruit_type <- gsub("soft","drupe", propdata$fruit_type)
propdata$fruit_type <- gsub("soft","legume", propdata$fruit_type)
propdata$fruit_type <- gsub("soft","pome", propdata$fruit_type)
propdata$fruit_type <- gsub("hard","ball(achene)", propdata$fruit_type)
propdata$fruit_type <- gsub("hard","nut", propdata$fruit_type)
propdata$fruit_type <- gsub("hard","capsule", propdata$fruit_type)
propdata$fruit_type <- gsub("hard","samara", propdata$fruit_type)
propdata$fruit_type <- gsub("hard","spiny ball", propdata$fruit_type)


# scaling to plot level using proportions ---------------------------------------------------
propdata$CNTY_PLT_phen<-paste(propdata$COUNTY_PLOT,propdata$phenology,sep="#")
propdata$CNTY_PLT_nfix<-paste(propdata$COUNTY_PLOT,propdata$n_fixer,sep="#")
propdata$CNTY_PLT_life <-paste(propdata$COUNTY_PLOT,propdata$life_span,sep="#")
propdata$CNTY_PLT_growth<-paste(propdata$COUNTY_PLOT,propdata$growth_rate,sep="#")
propdata$CNTY_PLT_drought<-paste(propdata$COUNTY_PLOT,propdata$drought_tol,sep="#")
propdata$CNTY_PLT_fire<-paste(propdata$COUNTY_PLOT,propdata$fire_tol,sep="#")
propdata$CNTY_PLT_anaerobic<-paste(propdata$COUNTY_PLOT,propdata$anaerobic_tol,sep="#")
propdata$CNTY_PLT_shade<-paste(propdata$COUNTY_PLOT,propdata$shade_tol,sep="#")
propdata$CNTY_PLT_fruit<-paste(propdata$COUNTY_PLOT,propdata$fruit_type,sep="#")

#phenology--- as a proportion (0= hardwood, 1=softwood)
nphen<- propdata %>% group_by(COUNTY_PLOT) %>% count(phenology)
nphen<- nphen %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
nphen<- nphen %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
nphen$CNTY_PLT_phen<-paste(nphen$COUNTY_PLOT,nphen$phenology,sep="#")

propdata$prop_phen <- nphen$prop[match(propdata$CNTY_PLT_phen,nphen$CNTY_PLT_phen)]

#nfixer---- as a proportion (0=no, 1=yes)

nfix<- propdata %>% group_by(COUNTY_PLOT) %>% count(n_fixer)
nfix<- nfix %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
nfix<- nfix %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
nfix$CNTY_PLT_nfix<-paste(nfix$COUNTY_PLOT,nfix$n_fixer,sep="#")

propdata$prop_nfix <- nfix$prop[match(propdata$CNTY_PLT_nfix,nfix$CNTY_PLT_nfix)]

#lifespan---- (0=short, 1=moderate,2=long)
lifesp<- propdata %>% group_by(COUNTY_PLOT) %>% count(life_span)
lifesp<- lifesp %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
lifesp<- lifesp %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
lifesp$CNTY_PLT_life<-paste(lifesp$COUNTY_PLOT,lifesp$life_span,sep="#")

propdata$prop_lifesp <- lifesp$prop[match(propdata$CNTY_PLT_life,lifesp$CNTY_PLT_life)]

#growth rate---- (0=slow, 1=moderate, 2= rapid)
growth<- propdata %>% group_by(COUNTY_PLOT) %>% count(growth_rate)
growth<- growth %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
growth<- growth %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
growth$CNTY_PLT_growth<-paste(growth$COUNTY_PLOT,growth$growth_rate,sep="#")

propdata$prop_growth <- growth$prop[match(propdata$CNTY_PLT_growth,growth$CNTY_PLT_growth)]

#drought tol ---- (none=0, low=1, moderate=2, high=3)
drought<- propdata %>% group_by(COUNTY_PLOT) %>% count(drought_tol)
drought<- drought %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
drought<- drought %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
drought$CNTY_PLT_drought<-paste(drought$COUNTY_PLOT,drought$drought_tol,sep="#")

propdata$prop_drought <- drought$prop[match(propdata$CNTY_PLT_drought,drought$CNTY_PLT_drought)]

#fire tol--- (none=0, low=1, moderate=2, high=3)
fire<- propdata %>% group_by(COUNTY_PLOT) %>% count(fire_tol)
fire<- fire %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
fire<- fire %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
fire$CNTY_PLT_fire<-paste(fire$COUNTY_PLOT,fire$fire_tol,sep="#")

propdata$prop_fire <- fire$prop[match(propdata$CNTY_PLT_fire,fire$CNTY_PLT_fire)]

#anaerobic tol --- - (none=0, low=1, moderate=2, high=3)
anaerobic<- propdata %>% group_by(COUNTY_PLOT) %>% count(anaerobic_tol)
anaerobic<- anaerobic %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
anaerobic<- anaerobic %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
anaerobic$CNTY_PLT_anaerobic<-paste(anaerobic$COUNTY_PLOT,anaerobic$anaerobic_tol,sep="#")

propdata$prop_anaerobic <- anaerobic$prop[match(propdata$CNTY_PLT_anaerobic,anaerobic$CNTY_PLT_anaerobic)]

#shade tol--- (intolerant=0, intermediate=1, tolerant =2)
shade<- propdata %>% group_by(COUNTY_PLOT) %>% count(shade_tol)
shade<- shade %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
shade<- shade %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
shade$CNTY_PLT_shade<-paste(shade$COUNTY_PLOT,shade$shade_tol,sep="#")

propdata$prop_shade <- shade$prop[match(propdata$CNTY_PLT_shade,shade$CNTY_PLT_shade)]

#fruit type----(cone=0, soft=1, hard=2)
fruit<- propdata %>% group_by(COUNTY_PLOT) %>% count(fruit_type)
fruit<- fruit %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
fruit<- fruit %>% group_by(COUNTY_PLOT) %>% mutate(prop= n/total)
fruit$CNTY_PLT_fruit<-paste(fruit$COUNTY_PLOT,fruit$fruit_type,sep="#")

propdata$prop_fruit <- fruit$prop[match(propdata$CNTY_PLT_fruit,fruit$CNTY_PLT_fruit)]



#For the continuous variables we use the plot mean and stdev of each]

#specific gravity
propdata <- propdata %>% group_by(COUNTY_PLOT) %>% mutate(sgmean= mean(specific_gravity))
propdata <- propdata %>% group_by(COUNTY_PLOT) %>% mutate(sgstdev= sd(specific_gravity))

#max height
propdata <- propdata %>% group_by(COUNTY_PLOT) %>% mutate(mhmean= mean(maxheight))
propdata <- propdata %>% group_by(COUNTY_PLOT) %>% mutate(mhstdev= sd(maxheight))

#fruit length
propdata <- propdata %>% group_by(COUNTY_PLOT) %>% mutate(flmean= mean(fruit_length))
propdata <- propdata %>% group_by(COUNTY_PLOT) %>% mutate(flstdev= sd(fruit_length))


# scaling using Shannons --------------------------------------------------
shanway<-compdata
#phenology
phendata<- shanway %>% group_by(COUNTY_PLOT) %>% count(phenology)
phendata<- phendata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
phendata<- phendata %>% mutate(p= n/total)
phendata<- phendata %>% mutate(plnp = p*log(p))
phendata<- phendata %>% group_by(COUNTY_PLOT,phenology) %>% summarise(sum1 = mean(plnp))%>% ungroup()
phendata<- phendata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
phendata<- phendata %>% mutate(phenshan= -1*sumfin)

shanway$phenshan<- phendata$phenshan[match(shanway$COUNTY_PLOT, phendata$COUNTY_PLOT)]

#nfixer
shnfix<- shanway %>% group_by(COUNTY_PLOT) %>% count(n_fixer)
shnfix<- shnfix %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
shnfix<- shnfix %>% mutate(p= n/total)
shnfix<- shnfix %>% mutate(plnp = p*log(p))
shnfix<- shnfix %>% group_by(COUNTY_PLOT,n_fixer) %>% summarise(sum1 = mean(plnp))%>% ungroup()
shnfix<- shnfix %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
shnfix<- shnfix %>% mutate(nfixshan= -1*sumfin)

shanway$nfixshan<- shnfix$nfixshan[match(shanway$COUNTY_PLOT, shnfix$COUNTY_PLOT)]

#lifespan
lifesh<- shanway %>% group_by(COUNTY_PLOT) %>% count(life_span)
lifesh<- lifesh %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
lifesh<- lifesh %>% mutate(p= n/total)
lifesh<- lifesh %>% mutate(plnp = p*log(p))
lifesh<- lifesh %>% group_by(COUNTY_PLOT,life_span) %>% summarise(sum1 = mean(plnp))%>% ungroup()
lifesh<- lifesh %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
lifesh<- lifesh %>% mutate(lifeshan= -1*sumfin)

shanway$lifeshan<- lifesh$lifeshan[match(shanway$COUNTY_PLOT, lifesh$COUNTY_PLOT)]

#growthrate
growthdata<- shanway %>% group_by(COUNTY_PLOT) %>% count(growth_rate)
growthdata<- growthdata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
growthdata<- growthdata %>% mutate(p= n/total)
growthdata<- growthdata %>% mutate(plnp = p*log(p))
growthdata<- growthdata %>% group_by(COUNTY_PLOT,growth_rate) %>% summarise(sum1 = mean(plnp))%>% ungroup()
growthdata<- growthdata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
growthdata<- growthdata %>% mutate(growthshan= -1*sumfin)

shanway$growthshan<- growthdata$growthshan[match(shanway$COUNTY_PLOT, growthdata$COUNTY_PLOT)]

#drought tol
droughtdata<- shanway %>% group_by(COUNTY_PLOT) %>% count(drought_tol)
droughtdata<- droughtdata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
droughtdata<- droughtdata %>% mutate(p= n/total)
droughtdata<- droughtdata %>% mutate(plnp = p*log(p))
droughtdata<- droughtdata %>% group_by(COUNTY_PLOT,drought_tol) %>% summarise(sum1 = mean(plnp))%>% ungroup()
droughtdata<- droughtdata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
droughtdata<- droughtdata %>% mutate(droughtshan= -1*sumfin)

shanway$droughtshan<- droughtdata$droughtshan[match(shanway$COUNTY_PLOT, droughtdata$COUNTY_PLOT)]

#fire tol
firedata<- shanway %>% group_by(COUNTY_PLOT) %>% count(fire_tol)
firedata<- firedata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
firedata<- firedata %>% mutate(p= n/total)
firedata<- firedata %>% mutate(plnp = p*log(p))
firedata<- firedata %>% group_by(COUNTY_PLOT,fire_tol) %>% summarise(sum1 = mean(plnp))%>% ungroup()
firedata<- firedata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
firedata<- firedata %>% mutate(fireshan= -1*sumfin)

shanway$fireshan<- firedata$fireshan[match(shanway$COUNTY_PLOT, firedata$COUNTY_PLOT)]

#anaerobic tol
anaerobicdata<- shanway %>% group_by(COUNTY_PLOT) %>% count(anaerobic_tol)
anaerobicdata<- anaerobicdata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
anaerobicdata<- anaerobicdata %>% mutate(p= n/total)
anaerobicdata<- anaerobicdata %>% mutate(plnp = p*log(p))
anaerobicdata<- anaerobicdata %>% group_by(COUNTY_PLOT,anaerobic_tol) %>% summarise(sum1 = mean(plnp))%>% ungroup()
anaerobicdata<- anaerobicdata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
anaerobicdata<- anaerobicdata %>% mutate(anaerobicshan= -1*sumfin)

shanway$anaerobicshan<- anaerobicdata$anaerobicshan[match(shanway$COUNTY_PLOT, anaerobicdata$COUNTY_PLOT)]

#shade tol
shadedata<- shanway %>% group_by(COUNTY_PLOT) %>% count(shade_tol)
shadedata<- shadedata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
shadedata<- shadedata %>% mutate(p= n/total)
shadedata<- shadedata %>% mutate(plnp = p*log(p))
shadedata<- shadedata %>% group_by(COUNTY_PLOT,shade_tol) %>% summarise(sum1 = mean(plnp))%>% ungroup()
shadedata<- shadedata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
shadedata<- shadedata %>% mutate(shadeshan= -1*sumfin)

shanway$shadeshan<- shadedata$shadeshan[match(shanway$COUNTY_PLOT, shadedata$COUNTY_PLOT)]

#fruit type
fruitdata<- shanway %>% group_by(COUNTY_PLOT) %>% count(fruit_type)
fruitdata<- fruitdata %>% group_by(COUNTY_PLOT) %>% mutate(total= sum(n))
fruitdata<- fruitdata %>% mutate(p= n/total)
fruitdata<- fruitdata %>% mutate(plnp = p*log(p))
fruitdata<- fruitdata %>% group_by(COUNTY_PLOT,fruit_type) %>% summarise(sum1 = mean(plnp))%>% ungroup()
fruitdata<- fruitdata %>% group_by(COUNTY_PLOT) %>% mutate(sumfin= sum(sum1)) %>% ungroup()
fruitdata<- fruitdata %>% mutate(fruitshan= -1*sumfin)

shanway$fruitshan<- fruitdata$fruitshan[match(shanway$COUNTY_PLOT, fruitdata$COUNTY_PLOT)]

#continuous should be the same
#specific gravity
shanway <- shanway %>% group_by(COUNTY_PLOT) %>% mutate(sgmean= mean(specific_gravity))
shanway <- shanway %>% group_by(COUNTY_PLOT) %>% mutate(sgstdev= sd(specific_gravity))

#max height
shanway <- shanway %>% group_by(COUNTY_PLOT) %>% mutate(mhmean= mean(maxheight))
shanway <- shanway %>% group_by(COUNTY_PLOT) %>% mutate(mhstdev= sd(maxheight))

#fruit length
shanway <- shanway %>% group_by(COUNTY_PLOT) %>% mutate(flmean= mean(fruit_length))
shanway <- shanway %>% group_by(COUNTY_PLOT) %>% mutate(flstdev= sd(fruit_length))

