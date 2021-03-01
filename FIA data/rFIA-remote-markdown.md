---
title: "rFIA remote markdown"
author: "Elizabeth Baach"
date: "2/10/2021"
output: 
  html_document: 
    keep_md: yes
---
first we need to load the packages

```r
library(rFIA)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.0.6     v stringr 1.4.0
## v tidyr   1.1.2     v forcats 0.5.1
## v readr   1.4.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```
#then we can remotely add MS FIA data
use this to download... but if already downloaded

```r
getFIA(c('MS'), dir = 'name', load = FALSE)
```

```
## Saving to name/. NOTE: modifying FIA tables in Excel may corrupt csv files.
```
use this to load preferred

```r
MS <- readFIA('C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', inMemory = FALSE)
```

#next, lets run the rFIA functions
first lets define our datbase as MS

```r
#must first define db at fia 
biomass(db = MS)
```

```
## Warning: The `.dots` argument of `group_by()` is deprecated as of dplyr 1.0.0.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
```

```
## # A tibble: 12 x 7
##     YEAR BIO_ACRE CARB_ACRE BIO_ACRE_SE CARB_ACRE_SE nPlots_TREE nPlots_AREA
##    <int>    <dbl>     <dbl>       <dbl>        <dbl>       <dbl>       <dbl>
##  1  2006     38.6      19.3        1.26         1.26        3538        3665
##  2  2009     39.0      19.5        1.24         1.24        3548        3666
##  3  2010     39.5      19.7        1.24         1.24        3547        3670
##  4  2011     40.2      20.1        1.23         1.23        3537        3668
##  5  2012     40.7      20.4        1.22         1.22        3539        3665
##  6  2013     41.7      20.8        1.21         1.21        3545        3668
##  7  2014     42.7      21.4        1.13         1.13        3835        3944
##  8  2015     44.2      22.1        1.11         1.11        3885        3989
##  9  2016     45.8      22.9        1.09         1.09        3937        4038
## 10  2017     48.2      24.1        1.06         1.06        3935        4033
## 11  2018     50.6      25.3        1.04         1.04        3920        4013
## 12  2019     52.2      26.1        1.04         1.04        3895        3996
```
filtering using natural stand origin we get:

```r
originbio<- biomass(MS, byPlot = TRUE, grpBy = c("STDORGCD")) %>% filter(STDORGCD ==0)
origindiv<- diversity(MS, byPlot = TRUE, grpBy = "STDORGCD")%>% filter(STDORGCD ==0)
originvit<- vitalRates(MS, byPlot = TRUE, grpBy = "STDORGCD")%>% filter(STDORGCD ==0)
```
great! now we can add our favorite columns from the rFIA functions we just ran to a common table lets call it adddata

```r
adddata<- originbio
adddata$S <- origindiv$S[match(adddata$pltID, origindiv$pltID)]
adddata$H <- origindiv$H[match(adddata$pltID, origindiv$pltID)]
adddata$BIO_GROW<- originvit$BIO_GROW[match(adddata$pltID,originvit$pltID)]
adddata$BIO_GROW_AC<- originvit$BIO_GROW_AC[match(adddata$pltID, originvit$pltID)]
```

#sweet! now we want to add columns from the general FIA database
first we need to load those tables remotely

```r
TREE <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'TREE', inMemory = TRUE)
COND <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'COND', inMemory = TRUE)
```
nice! now we can add our favorite columns from TREE and COND to adddata so everything we want is in one place
lets start with tree

```r
adddata$SPCD<- TREE$TREE$SPCD[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
adddata$COUNTYCD<- TREE$TREE$COUNTYCD[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
adddata$UNITCD <- TREE$TREE$UNITCD[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
adddata$DRYBIO_AG <- TREE$TREE$DRYBIO_AG[match(adddata$PLT_CN, TREE$TREE$PLT_CN)]
```
and heres from cond

```r
adddata$FORTYPCD <- COND$COND$FORTYPCD[match(adddata$PLT_CN, COND$COND$PLT_CN)]
```
awesome! now lets see if we can make the spread function work to see biomass overtime

```r
newtest<- adddata %>% group_by(pltID,YEAR) %>% summarise(biomass=sum(DRYBIO_AG))
```

```
## `summarise()` has grouped output by 'pltID'. You can override using the `.groups` argument.
```

```r
newtest<- newtest %>% filter(biomass>0)
spdtest<- spread(newtest, YEAR, biomass, fill = NA)
view(spdtest)
```
ok, nows the time to get some subtraction going
first lets select only 2009-2019, then name them so we can use them properly

```r
data<- spdtest  %>% select(-c("2006","2007","2008"))
data<- rename(data, c(X2019="2019", X2018="2018", X2017="2017", X2016="2016", X2015="2015", X2014="2014", X2013="2013", X2012="2012", X2011="2011", X2010="2010", X2009="2009"))
```
sweet! Now time for the monster if_else section

```r
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
```

very good! now that thats done, lets make a new table to work with and make some changes

```r
dataavg<- data
dataavg$mean <- rowMeans(dataavg[,16:69], na.rm=TRUE)
dataavg <- dataavg %>% select("mean", everything())
summary(dataavg$mean)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
## -1280.209     3.296    19.958    36.768    59.467  2033.920      1890
```

awesome! moment of truth... lets make a scatterplot!

```r
datasum<- dataavg %>% group_by(pltID) %>% summarise(changeinbiomass=mean)
ggplot(datasum, aes(x= pltID, y= changeinbiomass))+ geom_point()
```

```
## Warning: Removed 1890 rows containing missing values (geom_point).
```

![](rFIA-remote-markdown_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

heres another that compares species richness to change in biomass

```r
dataavg$S <- adddata$S[match(dataavg$pltID, adddata$pltID)]
datatry<- dataavg %>% group_by(pltID) %>% summarise(changeinbiomass=mean, speciesrichness=S)
ggplot(datatry, aes(x=speciesrichness, y=changeinbiomass))+geom_point()
```

```
## Warning: Removed 1890 rows containing missing values (geom_point).
```

![](rFIA-remote-markdown_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

