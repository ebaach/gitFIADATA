
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

#not working properly (?)
  # adddata$BIO_GROW<- originvit$BIO_GROW[match(adddata$pltID,originvit$pltID)]
  # adddata$BIO_GROW_AC<- originvit$BIO_GROW_AC[match(adddata$pltID, originvit$pltID)]

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
newtest<- adddata %>% group_by(pltID,YEAR,COUNTYCD) %>% summarise(biomass= sum(DRYBIO_AG))
newtest<- newtest %>% filter(biomass>0)
spdtest<- spread(newtest, YEAR, biomass, fill = NA)
data<- spdtest %>% filter(COUNTYCD=="1")
#this part below isnt working
  # spdtest$mct <- rowSums(is.na(spdtest))
  # spdtest<- spdtest %>% filter(mct<10)
#lets try Austin's code to subtract!
data$diff=if_else ((data$x2019>0)&(data$x2018>0),(data$X2019-data$X2018)/1, 
          if_else((data$X2019>0)&(data$X2017>0),(data$X2019-data$X2017)/2,
          if_else((data$X2019>0)&(data$X2016>0),(data$X2019-data$X2016)/3,
          if_else((data$X2019>0)&(data$X2015>0),(data$X2019-data$X2015)/4,
            if_else((data$X2019>0)&(data$X2014>0),(data$X2019-data$X2014)/5,
            if_else((data$X2019>0)&(data$X2013>0),(data$X2019-data$X2013)/6,
            if_else((data$X2019>0)&(data$X2012>0),(data$X2019-data$X2012)/7,
            if_else((data$X2019>0)&(data$X2011>0),(data$X2019-data$X2011)/8,
            if_else((data$X2019>0)&(data$X2010>0),(data$X2019-data$X2010)/9,
            if_else((data$X2019>0)&(data$X2009>0),(data$X2019-data$X2009)/10,
            if_else((data$X2019>0)&(data$X2008>0),(data$X2019-data$X2008)/11,
            if_else((data$X2019>0)&(data$X2007>0),(data$X2019-data$X2007)/12,
            if_else((data$X2019>0)&(data$X2006>0),(data$X2019-data$X2006)/13,
          if_else((data$X2018>0)&(data$X2017>0),(data$X2018-data$X2017)/1,
          if_else((data$X2018>0)&(data$X2016>0),(data$X2018-data$X2016)/2,
          if_else((data$X2018>0)&(data$X2015>0),(data$X2018-data$X2015)/3,
            if_else((data$X2018>0)&(data$X2014>0),(data$X2018-data$X2014)/4,
            if_else((data$X2018>0)&(data$X2013>0),(data$X2018-data$X2013)/5,
            if_else((data$X2018>0)&(data$X2012>0),(data$X2018-data$X2012)/6,
            if_else((data$X2018>0)&(data$X2011>0),(data$X2018-data$X2011)/7,
            if_else((data$X2018>0)&(data$X2010>0),(data$X2018-data$X2010)/8,
            if_else((data$X2018>0)&(data$X2009>0),(data$X2018-data$X2009)/9,
            if_else((data$X2018>0)&(data$X2008>0),(data$X2018-data$X2008)/10,
            if_else((data$X2018>0)&(data$X2007>0),(data$X2018-data$X2007)/11,
            if_else((data$X2018>0)&(data$X2006>0),(data$X2018-data$X2006)/12,
          if_else((data$X2017>0)&(data$X2016>0),(data$X2017-data$X2016)/1,
          if_else((data$X2017>0)&(data$X2015>0),(data$X2017-data$X2015)/2,
            if_else((data$X2017>0)&(data$X2014>0),(data$X2017-data$X2014)/3,
            if_else((data$X2017>0)&(data$X2013>0),(data$X2017-data$X2013)/4,
            if_else((data$X2017>0)&(data$X2012>0),(data$X2017-data$X2012)/5,
            if_else((data$X2017>0)&(data$X2011>0),(data$X2017-data$X2011)/6,
            if_else((data$X2017>0)&(data$X2010>0),(data$X2017-data$X2010)/7,
            if_else((data$X2017>0)&(data$X2009>0),(data$X2017-data$X2009)/8,
            if_else((data$X2017>0)&(data$X2008>0),(data$X2017-data$X2008)/9,
            if_else((data$X2017>0)&(data$X2007>0),(data$X2017-data$X2007)/10,
            if_else((data$X2017>0)&(data$X2006>0),(data$X2017-data$X2006)/11,
          if_else((data$X2016>0)&(data$X2015>0),(data$X2016-data$X2015)/1,
             if_else((data$X2016>0)&(data$X2014>0),(data$X2016-data$X2014)/2,
             if_else((data$X2016>0)&(data$X2013>0),(data$X2016-data$X2013)/3,
             if_else((data$X2016>0)&(data$X2012>0),(data$X2016-data$X2012)/4,
             if_else((data$X2016>0)&(data$X2011>0),(data$X2016-data$X2011)/5,
             if_else((data$X2016>0)&(data$X2010>0),(data$X2016-data$X2010)/6,
             if_else((data$X2016>0)&(data$X2009>0),(data$X2016-data$X2009)/7,
             if_else((data$X2016>0)&(data$X2008>0),(data$X2016-data$X2008)/8,
             if_else((data$X2016>0)&(data$X2007>0),(data$X2016-data$X2007)/9,
             if_else((data$X2016>0)&(data$X2006>0),(data$X2016-data$X2006)/10,
         if_else((data$X2015>0)&(data$X2014>0),(data$X2015-data$X2014)/1,
             if_else((data$X2015>0)&(data$X2013>0),(data$X2015-data$X2013)/2,
             if_else((data$X2015>0)&(data$X2012>0),(data$X2015-data$X2012)/3,
             if_else((data$X2015>0)&(data$X2011>0),(data$X2015-data$X2011)/4,
             if_else((data$X2015>0)&(data$X2010>0),(data$X2015-data$X2010)/5,
             if_else((data$X2015>0)&(data$X2009>0),(data$X2015-data$X2009)/6,
             if_else((data$X2015>0)&(data$X2008>0),(data$X2015-data$X2008)/7,
             if_else((data$X2015>0)&(data$X2007>0),(data$X2015-data$X2007)/8,
             if_else((data$X2015>0)&(data$X2006>0),(data$X2015-data$X2006)/9,
         if_else((data$X2014>0)&(data$X2013>0),(data$X2014-data$X2013)/1,
             if_else((data$X2014>0)&(data$X2012>0),(data$X2014-data$X2012)/2,
             if_else((data$X2014>0)&(data$X2011>0),(data$X2014-data$X2011)/3,
             if_else((data$X2014>0)&(data$X2010>0),(data$X2014-data$X2010)/4,
             if_else((data$X2014>0)&(data$X2009>0),(data$X2014-data$X2009)/5,
             if_else((data$X2014>0)&(data$X2008>0),(data$X2014-data$X2008)/6,
             if_else((data$X2014>0)&(data$X2007>0),(data$X2014-data$X2007)/7,
             if_else((data$X2014>0)&(data$X2006>0),(data$X2014-data$X2006)/8,
          if_else((data$X2013>0)&(data$X2012>0),(data$X2013-data$X2012)/1,
             if_else((data$X2013>0)&(data$X2011>0),(data$X2013-data$X2011)/2,
             if_else((data$X2013>0)&(data$X2010>0),(data$X2013-data$X2010)/3,
             if_else((data$X2013>0)&(data$X2009>0),(data$X2013-data$X2009)/4,
             if_else((data$X2013>0)&(data$X2008>0),(data$X2013-data$X2008)/5,
             if_else((data$X2013>0)&(data$X2007>0),(data$X2013-data$X2007)/6,
             if_else((data$X2013>0)&(data$X2006>0),(data$X2013-data$X2006)/7,
           if_else((data$X2012>0)&(data$X2011>0),(data$X2012-data$X2011)/1,
             if_else((data$X2012>0)&(data$X2010>0),(data$X2012-data$X2010)/2,
             if_else((data$X2012>0)&(data$X2009>0),(data$X2012-data$X2009)/3,
             if_else((data$X2012>0)&(data$X2008>0),(data$X2012-data$X2008)/4,
             if_else((data$X2012>0)&(data$X2007>0),(data$X2012-data$X2007)/5,
             if_else((data$X2012>0)&(data$X2006>0),(data$X2012-data$X2006)/6,
           if_else((data$X2011>0)&(data$X2010>0),(data$X2011-data$X2010)/1,
             if_else((data$X2011>0)&(data$X2009>0),(data$X2011-data$X2009)/2,
             if_else((data$X2011>0)&(data$X2008>0),(data$X2011-data$X2008)/3,
             if_else((data$X2011>0)&(data$X2007>0),(data$X2011-data$X2007)/4,
             if_else((data$X2011>0)&(data$X2006>0),(data$X2011-data$X2006)/5,
          if_else((data$X2010>0)&(data$X2009>0),(data$X2010-data$X2009)/1,
             if_else((data$X2010>0)&(data$X2008>0),(data$X2010-data$X2008)/2,
             if_else((data$X2010>0)&(data$X2007>0),(data$X2010-data$X2007)/3,
             if_else((data$X2010>0)&(data$X2006>0),(data$X2010-data$X2006)/4, 
          if_else((data$X2009>0)&(data$X2008>0),(data$X2009-data$X2008)/1,
             if_else((data$X2009>0)&(data$X2007>0),(data$X2009-data$X2007)/2,
             if_else((data$X2009>0)&(data$X2006>0),(data$X2009-data$X2006)/3,
          if_else((data$X2008>0)&(data$X2007>0),(data$X2008-data$X2007)/1,
             if_else((data$X2008>0)&(data$X2006>0),(data$X2008-data$X2006)/2,
          if_else((data$X2007>0)&(data$X2006>0),(data$X2007-data$X2006)/1,
                  0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
##for some reason it says there are too many/few parenthesis but when i add or delete there is still an error



#making some graphs (poster1)
#lets do one for diversity
divtest<- adddata %>% group_by(pltID,UNITCD) %>% summarise(speciesrichness= S)
divtest<- divtest %>% filter(speciesrichness>0)
shantest<- adddata %>% group_by(pltID, UNITCD) %>% summarise(Shannon=H)
shantest<- shantest %>% filter(Shannon>0)

#species richness (S)
#divided by unitcd
ggplot(divtest, aes(x= speciesrichness))+geom_histogram(binwidth = .5)+ ggtitle("Distribution of Plot Tree Species Richness")+ xlab("Species Richness")+ ylab("Number of Plots")+facet_grid(UNITCD~.,labeller = labeller(UNITCD=supp.labs))+theme(plot.title = element_text(size=20, face="bold",hjust = .5),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+ theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"))+ theme(strip.text.x = element_text(size = 14))

supp.labs <- c("Delta", "North","Central","South","Southwest")
names(supp.labs)<- c("1","2","3","4","5")

#overall
ggplot(divtest, aes(x= speciesrichness))+geom_histogram(binwidth = .5)+ggtitle("Distribution of Plot Tree Species Richness")+ xlab("Specues Richness")+ ylab("Number of Plots")+theme(plot.title = element_text(size=20, face="bold",hjust = .5),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+ theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"))+ theme(strip.text.x = element_text(size = 14))

#Shannons index (H)

#divided by unitcd
ggplot(shantest, aes(x= Shannon))+geom_histogram(binwidth = .05)+ ggtitle("Distribution of Plot Tree Shannon's Index")+ xlab("Shannon's Index")+ ylab("Number of Plots")+facet_grid(UNITCD~.,labeller = labeller(UNITCD=supp.labs))+theme(plot.title = element_text(size=20, face="bold",hjust = .5),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+ theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"))+ theme(strip.text.x = element_text(size = 14))

#overall
ggplot(shantest, aes(x= Shannon))+geom_histogram(binwidth = .05)+ggtitle("Distribution of Plot Tree Shannon's Index")+ xlab("Shannon's Index")+ ylab("Number of Plots")+theme(plot.title = element_text(size=20, face="bold",hjust = .5),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+ theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"))+ theme(strip.text.x = element_text(size = 14))


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

       