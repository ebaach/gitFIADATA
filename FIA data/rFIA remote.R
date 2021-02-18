
# set up ------------------------------------------------------------------


#first lets load our packages
library(rFIA)
library(dplyr)
library(tidyverse)
# 
# for downloading
# getFIA(c('MS'), dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', load = FALSE)

#use this to load--- last code is for downloading if this doesnt work
MS <- readFIA('C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', inMemory = FALSE)


# rFIA functions(loading) -------------------------------------------------


#must first define db at fia 
biomass(db = MS)

#grouping by plt origin code
originbio<- biomass(MS, byPlot = TRUE, grpBy = c("STDORGCD")) %>% filter(STDORGCD ==0)
origindiv<- diversity(MS, byPlot = TRUE, grpBy = "STDORGCD")%>% filter(STDORGCD ==0)
originvit<- vitalRates(MS, byPlot = TRUE, grpBy = "STDORGCD")%>% filter(STDORGCD ==0)


# merging rFIA functions --------------------------------------------------


#adding columns I want from div and vit to bio under new name
adddata<- originbio
#from the other rFIA tables
adddata$S <- origindiv$S[match(adddata$pltID, origindiv$pltID)]
adddata$H<- origindiv$H[match(adddata$pltID, origindiv$pltID)]
adddata$BIO_GROW<- originvit$BIO_GROW[match(adddata$pltID,originvit$pltID)]
adddata$BIO_GROW_AC<- originvit$BIO_GROW_AC[match(adddata$pltID, originvit$pltID)]
#now from the other 'main' tables

# loading main tables and merging them to main ----------------------------


#now to read the 'main' tables we will end up needing
TREE <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'TREE', inMemory = TRUE)
COND <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'COND', inMemory = TRUE)
#heres from tree
adddata$SPCD<- TREE$TREE$SPCD[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
adddata$COUNTYCD<- TREE$TREE$COUNTYCD[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
adddata$UNITCD <- TREE$TREE$UNITCD[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
adddata$DRYBIO_AG <- TREE$TREE$DRYBIO_AG[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
#heres from cond
adddata$FORTYPCD <- COND$COND$FORTYPCD[match(adddata$PLT_CN, COND$COND$PLT_CN)]

#ok lets try the spread function
newtest<- adddata %>% group_by(pltID,YEAR) %>% summarise(biomass= sum(DRYBIO_AG))
newtest<- newtest %>% filter(biomass>0)
spdtest<- spread(newtest, YEAR, biomass, fill = NA)
#this part below isnt working
spdtest$mct <- rowSums(is.na(spdtest))
spdtest<- spdtest %>% filter(mct<10)
#testing
vittest<- adddata %>% filter(pltID=='3_28_101_51')
vittest$BIO_GROW_AC

#lets do one for diversity
divtest<- adddata %>% group_by(pltID,UNITCD) %>% summarise(speciesrichness= S)
divtest<- divtest %>% filter(speciesrichness>0)
shantest<- adddata %>% group_by(pltID, UNITCD) %>% summarise(Shannon=H)
shantest<- shantest %>% filter(Shannon>0)

#species richness (S)
#divided by unitcd
ggplot(divtest, aes(x= speciesrichness))+geom_histogram(binwidth = .5)+ labs(title = "Distribution of Plot Tree Species Richness", y="Number of Plots",x="Species Richness")+facet_grid(UNITCD~.,labeller = labeller(UNITCD=supp.labs))+theme(plot.title = element_text(hjust = 0.5))
supp.labs <- c("Delta", "North","Central","South","Southwest")
names(supp.labs)<- c("1","2","3","4","5")

#overall
ggplot(divtest, aes(x= speciesrichness))+geom_histogram(binwidth = .5)+ labs(title = "Distribution of Plot Tree Species Richness", y="Number of Plots",x="Species Richness")+theme(plot.title = element_text(hjust = 0.5))


#Shannons index (H)

#divided by unitcd
ggplot(shantest, aes(x= Shannon))+geom_histogram(binwidth = .05)+ labs(title = "Distribution of Plot Tree Shannon's Index", y="Number of Plots",x="Shannon's Index")+facet_grid(UNITCD~.,labeller = labeller(UNITCD=supp.labs))+theme(plot.title = element_text(hjust = 0.5))
supp.labs <- c("Delta", "North","Central","South","Southwest")
names(supp.labs)<- c("1","2","3","4","5")

#overall
ggplot(shantest, aes(x= Shannon))+geom_histogram(binwidth = .05)+ labs(title = "Distribution of Plot Tree Shannon's Index", y="Number of Plots",x="Shannon's Index")+theme(plot.title = element_text(hjust = 0.5))


# #PAST work
#By species
# biosp<-biomass(MS, bySpecies = TRUE)

# #by plot
# bioplt<- biomass(MS, byPlot = TRUE)
# n_distinct(bioplt$BIO_ACRE)
# 
# #diversity by plot
# div_plt<- diversity(MS, byPlot = TRUE)
# n_distinct(div_plt$H)

# #growth rates
# # vits<- vitalRates(MS, byPlot = TRUE)
# n_distinct(vits$BIO_GROW_AC)
# 
# #merge bioplt and divplt
# div_bio<- div_plt %>% left_join(bioplt, by =c("pltID"="pltID","YEAR"="YEAR","PLOT_STATUS"="PLOT_STATUS","PLOT_STATUS_CD"="PLOT_STATUS_CD","nStems"="nStems","PLT_CN"="PLT_CN"))
# 
# div_bio<- div_bio %>% filter(PLOT_STATUS=="Forest")
# n_distinct(bioplt$S)
# 
# bioplt$H<- div_plt$H[match(bioplt$pltID,div_plt$pltID)]
# 
# bioplt$S <- div_plt$S[match(bioplt$pltID, div_plt$pltID)]
# bioplt$STDORGCD<- originbio$STDORGCD[match(bioplt$pltID, originbio$pltID)]
# 
# divbiovital<- bioplt %>% left_join(vits, by = c("pltID"="pltID"))
# 
# n_distinct(divbiovital$BIO_ACRE)
# 
# ## Alternative estimators (annual panels)
# yrbio<-biomass(MS, method = 'ANNUAL')
# #plot of biomass/acre/yr

       