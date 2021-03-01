
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
data<- spdtest  %>% select(-c("2006","2007","2008"))
data<- rename(data, c(X2019="2019", X2018="2018", X2017="2017", X2016="2016", X2015="2015", X2014="2014", X2013="2013", X2012="2012", X2011="2011", X2010="2010", X2009="2009"))
#this part below isnt working
  # spdtest$mct <- rowSums(is.na(spdtest))
  # spdtest<- spdtest %>% filter(mct<10)
#lets try Austin's code to subtract!
data$diff1918=   if_else((data$X2019>0)&(data$X2018>0),(data$X2019-data$X2018)/1,0) 
data$diff1917=  if_else((data$X2019>0)&(data$X2017>0),(data$X2019-data$X2017)/2,0)
data$diff1916=  if_else((data$X2019>0)&(data$X2016>0),(data$X2019-data$X2016)/3,0)
data$diff1915=  if_else((data$X2019>0)&(data$X2015>0),(data$X2019-data$X2015)/4,0)
data$diff1914= if_else((data$X2019>0)&(data$X2014>0),(data$X2019-data$X2014)/5,0)
data$diff1913=  if_else((data$X2019>0)&(data$X2013>0),(data$X2019-data$X2013)/6,0)
data$diff1912=  if_else((data$X2019>0)&(data$X2012>0),(data$X2019-data$X2012)/7,0)
data$diff1911=  if_else((data$X2019>0)&(data$X2011>0),(data$X2019-data$X2011)/8,0)
data$diff1910=  if_else((data$X2019>0)&(data$X2010>0),(data$X2019-data$X2010)/9,0)
data$diff1909=  if_else((data$X2019>0)&(data$X2009>0),(data$X2019-data$X2009)/10,0)
data$diff1908=  if_else((data$X2019>0)&(data$X2008>0),(data$X2019-data$X2008)/11,0)
data$diff1907=  if_else((data$X2019>0)&(data$X2007>0),(data$X2019-data$X2007)/12,0)
data$diff1906=  if_else((data$X2019>0)&(data$X2006>0),(data$X2019-data$X2006)/13,0)
data$diff1817=             if_else((data$X2018>0)&(data$X2017>0),(data$X2018-data$X2017)/1,0)
data$diff1816=             if_else((data$X2018>0)&(data$X2016>0),(data$X2018-data$X2016)/2,0)
data$diff1815=             if_else((data$X2018>0)&(data$X2015>0),(data$X2018-data$X2015)/3,0)
data$diff1814=          if_else((data$X2018>0)&(data$X2014>0),(data$X2018-data$X2014)/4,0)
data$diff1813=             if_else((data$X2018>0)&(data$X2013>0),(data$X2018-data$X2013)/5,0)
data$diff1812=       if_else((data$X2018>0)&(data$X2012>0),(data$X2018-data$X2012)/6,0)
data$diff1811=         if_else((data$X2018>0)&(data$X2011>0),(data$X2018-data$X2011)/7,0)
data$diff1810=            if_else((data$X2018>0)&(data$X2010>0),(data$X2018-data$X2010)/8,0)
data$diff1809=            if_else((data$X2018>0)&(data$X2009>0),(data$X2018-data$X2009)/9,0)
data$diff1808=           if_else((data$X2018>0)&(data$X2008>0),(data$X2018-data$X2008)/10,0)
data$diff1807=            if_else((data$X2018>0)&(data$X2007>0),(data$X2018-data$X2007)/11,0)
data$diff1806=           if_else((data$X2018>0)&(data$X2006>0),(data$X2018-data$X2006)/12,0)
data$diff1716=            if_else((data$X2017>0)&(data$X2016>0),(data$X2017-data$X2016)/1,0)
data$diff1715=            if_else((data$X2017>0)&(data$X2015>0),(data$X2017-data$X2015)/2,0)
data$diff1714=            if_else((data$X2017>0)&(data$X2014>0),(data$X2017-data$X2014)/3,0)
data$diff1713=           if_else((data$X2017>0)&(data$X2013>0),(data$X2017-data$X2013)/4,0)
data$diff1712=       if_else((data$X2017>0)&(data$X2012>0),(data$X2017-data$X2012)/5,0)
data$diff1711=        if_else((data$X2017>0)&(data$X2011>0),(data$X2017-data$X2011)/6,0)
data$diff1710=        if_else((data$X2017>0)&(data$X2010>0),(data$X2017-data$X2010)/7,0)
data$diff1709=          if_else((data$X2017>0)&(data$X2009>0),(data$X2017-data$X2009)/8,0)
data$diff1708=          if_else((data$X2017>0)&(data$X2008>0),(data$X2017-data$X2008)/9,0)
data$diff1707=           if_else((data$X2017>0)&(data$X2007>0),(data$X2017-data$X2007)/10,0)
data$diff1706=          if_else((data$X2017>0)&(data$X2006>0),(data$X2017-data$X2006)/11,0)
data$diff1615=      if_else((data$X2016>0)&(data$X2015>0),(data$X2016-data$X2015)/1,0)
data$diff1614=   if_else((data$X2016>0)&(data$X2014>0),(data$X2016-data$X2014)/2,0)
data$diff1613=    if_else((data$X2016>0)&(data$X2013>0),(data$X2016-data$X2013)/3,0)
data$diff1612=    if_else((data$X2016>0)&(data$X2012>0),(data$X2016-data$X2012)/4,0)
data$diff1611=    if_else((data$X2016>0)&(data$X2011>0),(data$X2016-data$X2011)/5,0)
data$diff1610=       if_else((data$X2016>0)&(data$X2010>0),(data$X2016-data$X2010)/6,0)
data$diff1609=    if_else((data$X2016>0)&(data$X2009>0),(data$X2016-data$X2009)/7,0)
data$diff1608=    if_else((data$X2016>0)&(data$X2008>0),(data$X2016-data$X2008)/8,0)
data$diff1607=     if_else((data$X2016>0)&(data$X2007>0),(data$X2016-data$X2007)/9,0)
data$diff1606=        if_else((data$X2016>0)&(data$X2006>0),(data$X2016-data$X2006)/10,0)
data$diff1514=        if_else((data$X2015>0)&(data$X2014>0),(data$X2015-data$X2014)/1,0)
data$diff1513=         if_else((data$X2015>0)&(data$X2013>0),(data$X2015-data$X2013)/2,0)
data$diff1512=        if_else((data$X2015>0)&(data$X2012>0),(data$X2015-data$X2012)/3,0)
data$diff1511=        if_else((data$X2015>0)&(data$X2011>0),(data$X2015-data$X2011)/4,0)
data$diff1510=       if_else((data$X2015>0)&(data$X2010>0),(data$X2015-data$X2010)/5,0)
data$diff1509=       if_else((data$X2015>0)&(data$X2009>0),(data$X2015-data$X2009)/6,0)
data$diff1508=       if_else((data$X2015>0)&(data$X2008>0),(data$X2015-data$X2008)/7,0)
data$diff1507=      if_else((data$X2015>0)&(data$X2007>0),(data$X2015-data$X2007)/8,0)
data$diff1506=      if_else((data$X2015>0)&(data$X2006>0),(data$X2015-data$X2006)/9,0)
data$diff1413=       if_else((data$X2014>0)&(data$X2013>0),(data$X2014-data$X2013)/1,0)
data$diff1412=       if_else((data$X2014>0)&(data$X2012>0),(data$X2014-data$X2012)/2,0)
data$diff1411=       if_else((data$X2014>0)&(data$X2011>0),(data$X2014-data$X2011)/3,0)
data$diff1410=       if_else((data$X2014>0)&(data$X2010>0),(data$X2014-data$X2010)/4,0)
data$diff1409=       if_else((data$X2014>0)&(data$X2009>0),(data$X2014-data$X2009)/5,0)
data$diff1408=       if_else((data$X2014>0)&(data$X2008>0),(data$X2014-data$X2008)/6,0)
data$diff1407=        if_else((data$X2014>0)&(data$X2007>0),(data$X2014-data$X2007)/7,0)
data$diff1406=       if_else((data$X2014>0)&(data$X2006>0),(data$X2014-data$X2006)/8,0)
data$diff1312=       if_else((data$X2013>0)&(data$X2012>0),(data$X2013-data$X2012)/1,0)
data$diff1311=        if_else((data$X2013>0)&(data$X2011>0),(data$X2013-data$X2011)/2,0)
data$diff1310=         if_else((data$X2013>0)&(data$X2010>0),(data$X2013-data$X2010)/3,0)
data$diff1309=         if_else((data$X2013>0)&(data$X2009>0),(data$X2013-data$X2009)/4,0)
data$diff1308=         if_else((data$X2013>0)&(data$X2008>0),(data$X2013-data$X2008)/5,0)
data$diff1307=          if_else((data$X2013>0)&(data$X2007>0),(data$X2013-data$X2007)/6,0)
data$diff1306=          if_else((data$X2013>0)&(data$X2006>0),(data$X2013-data$X2006)/7,0)
data$diff1211=          if_else((data$X2012>0)&(data$X2011>0),(data$X2012-data$X2011)/1,0)
data$diff1210=          if_else((data$X2012>0)&(data$X2010>0),(data$X2012-data$X2010)/2,0)
data$diff1209=           if_else((data$X2012>0)&(data$X2009>0),(data$X2012-data$X2009)/3,0)
data$diff1208=           if_else((data$X2012>0)&(data$X2008>0),(data$X2012-data$X2008)/4,0)
data$diff1207=     if_else((data$X2012>0)&(data$X2007>0),(data$X2012-data$X2007)/5,0)
data$diff1206=     if_else((data$X2012>0)&(data$X2006>0),(data$X2012-data$X2006)/6,0)
data$diff1110=       if_else((data$X2011>0)&(data$X2010>0),(data$X2011-data$X2010)/1,0)
data$diff1109=       if_else((data$X2011>0)&(data$X2009>0),(data$X2011-data$X2009)/2,0)
data$diff1108=       if_else((data$X2011>0)&(data$X2008>0),(data$X2011-data$X2008)/3,0)
data$diff1107=        if_else((data$X2011>0)&(data$X2007>0),(data$X2011-data$X2007)/4,0)
data$diff1106=        if_else((data$X2011>0)&(data$X2006>0),(data$X2011-data$X2006)/5,0)
data$diff1009=        if_else((data$X2010>0)&(data$X2009>0),(data$X2010-data$X2009)/1,0)
data$diff1008=        if_else((data$X2010>0)&(data$X2008>0),(data$X2010-data$X2008)/2,0)
data$diff1007=        if_else((data$X2010>0)&(data$X2007>0),(data$X2010-data$X2007)/3,0)
data$diff1006=       if_else((data$X2010>0)&(data$X2006>0),(data$X2010-data$X2006)/4,0)
data$diff0908=       if_else((data$X2009>0)&(data$X2008>0),(data$X2009-data$X2008)/1,0)
data$diff0907=       if_else((data$X2009>0)&(data$X2007>0),(data$X2009-data$X2007)/2,0)
data$diff0906=       if_else((data$X2009>0)&(data$X2006>0),(data$X2009-data$X2006)/3,0)
data$diff0807=       if_else((data$X2008>0)&(data$X2007>0),(data$X2008-data$X2007)/1,0)
data$diff0806=       if_else((data$X2008>0)&(data$X2006>0),(data$X2008-data$X2006)/2,0)
data$diff0706=       if_else((data$X2007>0)&(data$X2006>0),(data$X2007-data$X2006)/1,0)



#heres a copy to delete 06-09
data$diff1918=   if_else((data$X2019>0)&(data$X2018>0),(data$X2019-data$X2018)/1,0) 
data$diff1917=  if_else((data$X2019>0)&(data$X2017>0),(data$X2019-data$X2017)/2,0)
data$diff1916=  if_else((data$X2019>0)&(data$X2016>0),(data$X2019-data$X2016)/3,0)
data$diff1915=  if_else((data$X2019>0)&(data$X2015>0),(data$X2019-data$X2015)/4,0)
data$diff1914= if_else((data$X2019>0)&(data$X2014>0),(data$X2019-data$X2014)/5,0)
data$diff1913=  if_else((data$X2019>0)&(data$X2013>0),(data$X2019-data$X2013)/6,0)
data$diff1912=  if_else((data$X2019>0)&(data$X2012>0),(data$X2019-data$X2012)/7,0)
data$diff1911=  if_else((data$X2019>0)&(data$X2011>0),(data$X2019-data$X2011)/8,0)
data$diff1910=  if_else((data$X2019>0)&(data$X2010>0),(data$X2019-data$X2010)/9,0)
data$diff1909=  if_else((data$X2019>0)&(data$X2009>0),(data$X2019-data$X2009)/10,0)

data$diff1817=             if_else((data$X2018>0)&(data$X2017>0),(data$X2018-data$X2017)/1,0)
data$diff1816=             if_else((data$X2018>0)&(data$X2016>0),(data$X2018-data$X2016)/2,0)
data$diff1815=             if_else((data$X2018>0)&(data$X2015>0),(data$X2018-data$X2015)/3,0)
data$diff1814=          if_else((data$X2018>0)&(data$X2014>0),(data$X2018-data$X2014)/4,0)
data$diff1813=             if_else((data$X2018>0)&(data$X2013>0),(data$X2018-data$X2013)/5,0)
data$diff1812=       if_else((data$X2018>0)&(data$X2012>0),(data$X2018-data$X2012)/6,0)
data$diff1811=         if_else((data$X2018>0)&(data$X2011>0),(data$X2018-data$X2011)/7,0)
data$diff1810=            if_else((data$X2018>0)&(data$X2010>0),(data$X2018-data$X2010)/8,0)
data$diff1809=            if_else((data$X2018>0)&(data$X2009>0),(data$X2018-data$X2009)/9,0)

data$diff1716=            if_else((data$X2017>0)&(data$X2016>0),(data$X2017-data$X2016)/1,0)
data$diff1715=            if_else((data$X2017>0)&(data$X2015>0),(data$X2017-data$X2015)/2,0)
data$diff1714=            if_else((data$X2017>0)&(data$X2014>0),(data$X2017-data$X2014)/3,0)
data$diff1713=           if_else((data$X2017>0)&(data$X2013>0),(data$X2017-data$X2013)/4,0)
data$diff1712=       if_else((data$X2017>0)&(data$X2012>0),(data$X2017-data$X2012)/5,0)
data$diff1711=        if_else((data$X2017>0)&(data$X2011>0),(data$X2017-data$X2011)/6,0)
data$diff1710=        if_else((data$X2017>0)&(data$X2010>0),(data$X2017-data$X2010)/7,0)
data$diff1709=          if_else((data$X2017>0)&(data$X2009>0),(data$X2017-data$X2009)/8,0)

data$diff1615=      if_else((data$X2016>0)&(data$X2015>0),(data$X2016-data$X2015)/1,0)
data$diff1614=   if_else((data$X2016>0)&(data$X2014>0),(data$X2016-data$X2014)/2,0)
data$diff1613=    if_else((data$X2016>0)&(data$X2013>0),(data$X2016-data$X2013)/3,0)
data$diff1612=    if_else((data$X2016>0)&(data$X2012>0),(data$X2016-data$X2012)/4,0)
data$diff1611=    if_else((data$X2016>0)&(data$X2011>0),(data$X2016-data$X2011)/5,0)
data$diff1610=       if_else((data$X2016>0)&(data$X2010>0),(data$X2016-data$X2010)/6,0)
data$diff1609=    if_else((data$X2016>0)&(data$X2009>0),(data$X2016-data$X2009)/7,0)

data$diff1514=        if_else((data$X2015>0)&(data$X2014>0),(data$X2015-data$X2014)/1,0)
data$diff1513=         if_else((data$X2015>0)&(data$X2013>0),(data$X2015-data$X2013)/2,0)
data$diff1512=        if_else((data$X2015>0)&(data$X2012>0),(data$X2015-data$X2012)/3,0)
data$diff1511=        if_else((data$X2015>0)&(data$X2011>0),(data$X2015-data$X2011)/4,0)
data$diff1510=       if_else((data$X2015>0)&(data$X2010>0),(data$X2015-data$X2010)/5,0)
data$diff1509=       if_else((data$X2015>0)&(data$X2009>0),(data$X2015-data$X2009)/6,0)

data$diff1413=       if_else((data$X2014>0)&(data$X2013>0),(data$X2014-data$X2013)/1,0)
data$diff1412=       if_else((data$X2014>0)&(data$X2012>0),(data$X2014-data$X2012)/2,0)
data$diff1411=       if_else((data$X2014>0)&(data$X2011>0),(data$X2014-data$X2011)/3,0)
data$diff1410=       if_else((data$X2014>0)&(data$X2010>0),(data$X2014-data$X2010)/4,0)
data$diff1409=       if_else((data$X2014>0)&(data$X2009>0),(data$X2014-data$X2009)/5,0)

data$diff1312=       if_else((data$X2013>0)&(data$X2012>0),(data$X2013-data$X2012)/1,0)
data$diff1311=        if_else((data$X2013>0)&(data$X2011>0),(data$X2013-data$X2011)/2,0)
data$diff1310=         if_else((data$X2013>0)&(data$X2010>0),(data$X2013-data$X2010)/3,0)
data$diff1309=         if_else((data$X2013>0)&(data$X2009>0),(data$X2013-data$X2009)/4,0)

data$diff1211=          if_else((data$X2012>0)&(data$X2011>0),(data$X2012-data$X2011)/1,0)
data$diff1210=          if_else((data$X2012>0)&(data$X2010>0),(data$X2012-data$X2010)/2,0)
data$diff1209=           if_else((data$X2012>0)&(data$X2009>0),(data$X2012-data$X2009)/3,0)

data$diff1110=       if_else((data$X2011>0)&(data$X2010>0),(data$X2011-data$X2010)/1,0)
data$diff1109=       if_else((data$X2011>0)&(data$X2009>0),(data$X2011-data$X2009)/2,0)

data$diff1009=        if_else((data$X2010>0)&(data$X2009>0),(data$X2010-data$X2009)/1,0)



#lets make one without columns that are only NAs

dataavg<- data

dataavg$mean <- rowMeans(dataavg[,16:70], na.rm=TRUE)

dataavg <- dataavg %>% select("mean", everything())

#scatterplot anyone?
datasum<- dataavg %>% group_by(pltID) %>% summarise(changeinbiomass=mean)
ggplot(datasum, aes(x= pltID, y= changeinbiomass))+ geom_point()



#making some graphs (poster1)
#lets do one for diversity
divtest<- adddata %>% group_by(pltID,UNITCD) %>% summarise(speciesrichness= S)
divtest<- divtest %>% filter(speciesrichness>0)
shantest<- adddata %>% group_by(pltID, UNITCD) %>% summarise(Shannon=H)
shantest<- shantest %>% filter(Shannon>0)


max(divtest$speciesrichness)
min(divtest$speciesrichness)
max(shantest$Shannon)
min(shantest$Shannon)

#species richness (S)
#divided by unitcd
ggplot(divtest, aes(x= speciesrichness))+geom_histogram(binwidth = .5)+ ggtitle("Distribution of Plot Tree Species Richness")+ xlab("Species Richness")+ ylab("Number of Plots")+facet_grid(UNITCD~.,labeller = labeller(UNITCD=supp.labs))+theme(plot.title = element_text(size=20, face="bold",hjust = .5),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+ theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"))+ theme(strip.text.x = element_text(size = 14))

supp.labs <- c("Delta", "North","Central","South","Southwest")
names(supp.labs)<- c("1","2","3","4","5")

#overall
ggplot(divtest, aes(x= speciesrichness))+geom_histogram(binwidth = .5)+ggtitle("Distribution of Plot Tree Species Richness")+ xlab("Species Richness")+ ylab("Number of Plots")+theme(plot.title = element_text(size=20, face="bold",hjust = .5),axis.title.x = element_text(size=14),axis.title.y = element_text(size=14))+ theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"))+ theme(strip.text.x = element_text(size = 14))

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

       