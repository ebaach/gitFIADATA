---
title: "usingbaseFIAtables"
author: "Elizabeth Baach"
date: "4/8/2021"
output: 
  html_document: 
    keep_md: yes
---


## setting up
loading libraries

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

```r
library(vegan)
```

```
## Warning: package 'vegan' was built under R version 4.0.5
```

```
## Loading required package: permute
```

```
## Warning: package 'permute' was built under R version 4.0.5
```

```
## Loading required package: lattice
```

```
## This is vegan 2.5-7
```

```
## 
## Attaching package: 'vegan'
```

```
## The following object is masked from 'package:rFIA':
## 
##     diversity
```
making a space to work out of

```r
MS <- readFIA('C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', inMemory = FALSE)
```
loading tables to use

```r
TREE <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'TREE', inMemory = TRUE)
COND <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'COND', inMemory = TRUE)
PLOT <- readFIA(dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', tables = 'PLOT', inMemory = TRUE)
```
## filtering
tree table

```r
tree_tab <- TREE$TREE
tree_tab <- select(tree_tab,c(CN, PLT_CN, PREV_TRE_CN, INVYR, COUNTYCD, PLOT, SUBP, CONDID, STATUSCD, SPCD, DIA, HT, SPGRPCD,CR, CCLCD,SPGRPCD))
tree_tab <- tree_tab %>% mutate(diam = (2.54 * tree_tab$DIA ))
tree_tab<- tree_tab %>% select(-c(DIA))
tree_tab$DIA<- tree_tab$diam
tree_tab<- tree_tab %>% select(-c(diam))
```
plot table

```r
plot_tab <- PLOT$PLOT
plot_tab<- select(plot_tab, c(PLOT, INVYR, COUNTYCD, LAT, LON, ELEV, RDDISTCD, ECOSUBCD))
plot_tab$COUNTY_PLOT <- paste(plot_tab$COUNTYCD,plot_tab$PLOT, sep="_")
plot_tab<- plot_tab %>% filter(INVYR>=2009)
```
cond table

```r
cond_tab <- COND$COND
cond_tab<- select(cond_tab, c(CN, PLT_CN, INVYR, COUNTYCD, PLOT, FORTYPCD, STDAGE, STDORGCD, DSTRBCD1,TRTCD1, SITECLCD, SICOND,PHYSCLCD, FIRE_SRS, GRAZING_SRS, OWNCD, STAND_STRUCTURE_SRS))
cond_tab<- cond_tab %>% filter(INVYR>=2009)
cond_tab<- cond_tab %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
cond_tab$COUNTY_PLOT <- paste(cond_tab$COUNTYCD,cond_tab$PLOT, sep="_")
```
## subsetting by year
tree

```r
tree09 <- tree_tab %>% subset(INVYR==2009)
    tree10 <- tree_tab %>% subset(INVYR==2010)
    tree11 <- tree_tab %>% subset(INVYR==2011)
    tree12 <- tree_tab %>% subset(INVYR==2012)
    tree13 <- tree_tab %>% subset(INVYR==2013)
    tree14 <- tree_tab %>% subset(INVYR==2014)
    tree15 <- tree_tab %>% subset(INVYR==2015)
    tree16 <- tree_tab %>% subset(INVYR==2016)
    tree17 <- tree_tab %>% subset(INVYR==2017)
    tree18 <- tree_tab %>% subset(INVYR==2018)
    tree19 <- tree_tab %>% subset(INVYR==2019)
```
plot

```r
plot09<- plot_tab %>% subset(INVYR==2009)
    plot10<- plot_tab %>% subset(INVYR==2010)
    plot11<- plot_tab %>% subset(INVYR==2011)
    plot12<- plot_tab %>% subset(INVYR==2012)
    plot13<- plot_tab %>% subset(INVYR==2013)
    plot14<- plot_tab %>% subset(INVYR==2014)
    plot15<- plot_tab %>% subset(INVYR==2015)
    plot16<- plot_tab %>% subset(INVYR==2016)
    plot17<- plot_tab %>% subset(INVYR==2017)
    plot18<- plot_tab %>% subset(INVYR==2018)
    plot19<- plot_tab %>% subset(INVYR==2019)
```
cond

```r
 cond09<- cond_tab %>% subset(INVYR==2009)
    cond10<- cond_tab %>% subset(INVYR==2010)
    cond11<- cond_tab %>% subset(INVYR==2011)
    cond12<- cond_tab %>% subset(INVYR==2012)
    cond13<- cond_tab %>% subset(INVYR==2013)
    cond14<- cond_tab %>% subset(INVYR==2014)
    cond15<- cond_tab %>% subset(INVYR==2015)
    cond16<- cond_tab %>% subset(INVYR==2016)
    cond17<- cond_tab %>% subset(INVYR==2017)
    cond18<- cond_tab %>% subset(INVYR==2018)
    cond19<- cond_tab %>% subset(INVYR==2019)
```

## filtering to get matching CN and PREV_TRE_CN

```r
 #2009 to 2016
    checkdat1609 <- subset(tree16, tree16$PREV_TRE_CN %in% tree09$CN)
    checkdat1609 <- subset(checkdat1609, select = -c(CN))
    checkdat1609$CN <- checkdat1609$PREV_TRE_CN
    checkdat1609 <- subset(checkdat1609, select = -c(PREV_TRE_CN, PLT_CN))
  #2009 to 2017
    checkdat1709 <- subset(tree17, tree17$PREV_TRE_CN %in% tree09$CN)
    checkdat1709 <- subset(checkdat1709, select = -c(CN))
    checkdat1709$CN <- checkdat1709$PREV_TRE_CN
    checkdat1709 <- subset(checkdat1709, select = -c(PREV_TRE_CN, PLT_CN))
  #2009 to 2018
    checkdat1809 <- subset(tree18, tree18$PREV_TRE_CN %in% tree09$CN)
    checkdat1809 <- subset(checkdat1809, select = -c(CN))
    checkdat1809$CN <- checkdat1809$PREV_TRE_CN
    checkdat1809 <- subset(checkdat1809, select = -c(PREV_TRE_CN, PLT_CN))
  #2010 to 2016 
    checkdat1610 <- subset(tree16, tree16$PREV_TRE_CN %in% tree10$CN)
    checkdat1610 <- subset(checkdat1610, select = -c(CN))
    checkdat1610$CN <- checkdat1610$PREV_TRE_CN
    checkdat1610 <- subset(checkdat1610, select = -c(PREV_TRE_CN, PLT_CN))
  #2010 to 2018
    checkdat1810 <- subset(tree18, tree18$PREV_TRE_CN %in% tree10$CN)
    checkdat1810 <- subset(checkdat1810, select = -c(CN))
    checkdat1810$CN <- checkdat1810$PREV_TRE_CN
    checkdat1810 <- subset(checkdat1810, select = -c(PREV_TRE_CN, PLT_CN))
  #2010 to 2019
    checkdat1910 <- subset(tree19, tree19$PREV_TRE_CN %in% tree10$CN)
    checkdat1910 <- subset(checkdat1910, select = -c(CN))
    checkdat1910$CN <- checkdat1910$PREV_TRE_CN
    checkdat1910 <- subset(checkdat1910, select = -c(PREV_TRE_CN, PLT_CN))
  #2011 to 2016 
    checkdat1611 <- subset(tree16, tree16$PREV_TRE_CN %in% tree11$CN)
    checkdat1611 <- subset(checkdat1611, select = -c(CN))
    checkdat1611$CN <- checkdat1611$PREV_TRE_CN
    checkdat1611 <- subset(checkdat1611, select = -c(PREV_TRE_CN, PLT_CN))
  #2011 to 2017 
    checkdat1711 <- subset(tree17, tree17$PREV_TRE_CN %in% tree11$CN)
    checkdat1711 <- subset(checkdat1711, select = -c(CN))
    checkdat1711$CN <- checkdat1711$PREV_TRE_CN
    checkdat1711 <- subset(checkdat1711, select = -c(PREV_TRE_CN, PLT_CN))
  #2011 to 2018
    checkdat1811 <- subset(tree18, tree18$PREV_TRE_CN %in% tree11$CN)
    checkdat1811 <- subset(checkdat1811, select = -c(CN))
    checkdat1811$CN <- checkdat1811$PREV_TRE_CN
    checkdat1811 <- subset(checkdat1811, select = -c(PREV_TRE_CN, PLT_CN))
  #2011 to 2019
    checkdat1911 <- subset(tree19, tree19$PREV_TRE_CN %in% tree11$CN)
    checkdat1911 <- subset(checkdat1911, select = -c(CN))
    checkdat1911$CN <- checkdat1911$PREV_TRE_CN
    checkdat1911 <- subset(checkdat1911, select = -c(PREV_TRE_CN, PLT_CN))
  #2012 to 2016
    checkdat1612 <- subset(tree16, tree16$PREV_TRE_CN %in% tree12$CN)
    checkdat1612 <- subset(checkdat1612, select = -c(CN))
    checkdat1612$CN <- checkdat1612$PREV_TRE_CN
    checkdat1612 <- subset(checkdat1612, select = -c(PREV_TRE_CN, PLT_CN))
  #2012 to 2017
    checkdat1712 <- subset(tree17, tree17$PREV_TRE_CN %in% tree12$CN)
    checkdat1712 <- subset(checkdat1712, select = -c(CN))
    checkdat1712$CN <- checkdat1712$PREV_TRE_CN
    checkdat1712 <- subset(checkdat1712, select = -c(PREV_TRE_CN, PLT_CN))
  #2012 to 2018
    checkdat1812 <- subset(tree18, tree18$PREV_TRE_CN %in% tree12$CN)
    checkdat1812 <- subset(checkdat1812, select = -c(CN))
    checkdat1812$CN <- checkdat1812$PREV_TRE_CN
    checkdat1812 <- subset(checkdat1812, select = -c(PREV_TRE_CN, PLT_CN))
  #2012 to 2019
    checkdat1912 <- subset(tree19, tree19$PREV_TRE_CN %in% tree12$CN)
    checkdat1912 <- subset(checkdat1912, select = -c(CN))
    checkdat1912$CN <- checkdat1912$PREV_TRE_CN
    checkdat1912 <- subset(checkdat1912, select = -c(PREV_TRE_CN, PLT_CN))
  #2013 to 2018
    checkdat1813 <- subset(tree18, tree18$PREV_TRE_CN %in% tree13$CN)
    checkdat1813 <- subset(checkdat1813, select = -c(CN))
    checkdat1813$CN <- checkdat1813$PREV_TRE_CN
    checkdat1813 <- subset(checkdat1813, select = -c(PREV_TRE_CN, PLT_CN))
  #2013 to 2019
    checkdat1913 <- subset(tree19, tree19$PREV_TRE_CN %in% tree13$CN)
    checkdat1913 <- subset(checkdat1913, select = -c(CN))
    checkdat1913$CN <- checkdat1913$PREV_TRE_CN
    checkdat1913<- subset(checkdat1913, select = -c(PREV_TRE_CN, PLT_CN))
  #2014 to 2018
    checkdat1814 <- subset(tree18, tree18$PREV_TRE_CN %in% tree14$CN)
    checkdat1814 <- subset(checkdat1814, select = -c(CN))
    checkdat1814$CN <- checkdat1814$PREV_TRE_CN
    checkdat1814 <- subset(checkdat1814, select = -c(PREV_TRE_CN, PLT_CN))
  #2014 to 2019
    checkdat1914 <- subset(tree19, tree19$PREV_TRE_CN %in% tree14$CN)
    checkdat1914 <- subset(checkdat1914, select = -c(CN))
    checkdat1914$CN <- checkdat1914$PREV_TRE_CN
    checkdat1914 <- subset(checkdat1914, select = -c(PREV_TRE_CN, PLT_CN))
  #2015 to 2018
    checkdat1815 <- subset(tree18, tree18$PREV_TRE_CN %in% tree15$CN)
    checkdat1815 <- subset(checkdat1815, select = -c(CN))
    checkdat1815$CN <- checkdat1815$PREV_TRE_CN
    checkdat1815 <- subset(checkdat1815, select = -c(PREV_TRE_CN, PLT_CN))
  #2015 to 2019
    checkdat1915 <- subset(tree19, tree19$PREV_TRE_CN %in% tree15$CN)
    checkdat1915 <- subset(checkdat1915, select = -c(CN))
    checkdat1915$CN <- checkdat1915$PREV_TRE_CN
    checkdat1915 <- subset(checkdat1915, select = -c(PREV_TRE_CN, PLT_CN))
```
now for the other way around

```r
#2009-16
    checkdat0916 <- subset(tree09, tree09$CN %in%  tree16$PREV_TRE_CN)
    checkdat0916 <- subset(checkdat0916, select = -c(PREV_TRE_CN))
  #2009-17
    checkdat0917 <- subset(tree09, tree09$CN %in%  tree17$PREV_TRE_CN)
    checkdat0917 <- subset(checkdat0917, select = -c(PREV_TRE_CN))
  #2009-18
    checkdat0918 <- subset(tree09, tree09$CN %in%  tree18$PREV_TRE_CN)
    checkdat0918 <- subset(checkdat0918, select = -c(PREV_TRE_CN))
  #2010-16
    checkdat1016 <- subset(tree10, tree10$CN %in%  tree16$PREV_TRE_CN)
    checkdat1016 <- subset(checkdat1016, select = -c(PREV_TRE_CN))
  #2010-18
    checkdat1018 <- subset(tree10, tree10$CN %in%  tree18$PREV_TRE_CN)
    checkdat1018 <- subset(checkdat1018, select = -c(PREV_TRE_CN))
  #2010-19
    checkdat1019 <- subset(tree10, tree10$CN %in%  tree19$PREV_TRE_CN)
    checkdat1019 <- subset(checkdat1019, select = -c(PREV_TRE_CN))
  #2011-16
    checkdat1116 <- subset(tree11, tree11$CN %in%  tree16$PREV_TRE_CN)
    checkdat1116 <- subset(checkdat1116, select = -c(PREV_TRE_CN))
  #2011-17
    checkdat1117 <- subset(tree11, tree11$CN %in%  tree17$PREV_TRE_CN)
    checkdat1117 <- subset(checkdat1117, select = -c(PREV_TRE_CN))
  #2011-18
    checkdat1118 <- subset(tree11, tree11$CN %in%  tree18$PREV_TRE_CN)
    checkdat1118 <- subset(checkdat1118, select = -c(PREV_TRE_CN))
  #2011-19
    checkdat1119 <- subset(tree11, tree11$CN %in%  tree19$PREV_TRE_CN)
    checkdat1119 <- subset(checkdat1119, select = -c(PREV_TRE_CN))
  #2012-16
    checkdat1216 <- subset(tree12, tree12$CN %in%  tree16$PREV_TRE_CN)
    checkdat1216 <- subset(checkdat1216, select = -c(PREV_TRE_CN))
  #2012-17
    checkdat1217 <- subset(tree12, tree12$CN %in%  tree17$PREV_TRE_CN)
    checkdat1217 <- subset(checkdat1217, select = -c(PREV_TRE_CN))
  #2012-18
    checkdat1218 <- subset(tree12, tree12$CN %in%  tree18$PREV_TRE_CN)
    checkdat1218 <- subset(checkdat1218, select = -c(PREV_TRE_CN))
  #2012-19
    checkdat1219 <- subset(tree12, tree12$CN %in%  tree19$PREV_TRE_CN)
    checkdat1219 <- subset(checkdat1219, select = -c(PREV_TRE_CN))
  #2013-18
    checkdat1318 <- subset(tree13, tree13$CN %in%  tree18$PREV_TRE_CN)
    checkdat1318 <- subset(checkdat1318, select = -c(PREV_TRE_CN))
  #2013-19
    checkdat1319 <- subset(tree13, tree13$CN %in%  tree19$PREV_TRE_CN)
    checkdat1319 <- subset(checkdat1319, select = -c(PREV_TRE_CN))
  #2014-18
    checkdat1418 <- subset(tree14, tree14$CN %in%  tree18$PREV_TRE_CN)
    checkdat1418 <- subset(checkdat1418, select = -c(PREV_TRE_CN))
  #2014-19
    checkdat1419 <- subset(tree14, tree14$CN %in%  tree19$PREV_TRE_CN)
    checkdat1419 <- subset(checkdat1419, select = -c(PREV_TRE_CN))
  #2015-18
    checkdat1518 <- subset(tree15, tree15$CN %in%  tree18$PREV_TRE_CN)
    checkdat1519 <- subset(checkdat1518, select = -c(PREV_TRE_CN))
  #2015-19
    checkdat1519 <- subset(tree15, tree15$CN %in%  tree19$PREV_TRE_CN)
    checkdat1519 <- subset(checkdat1519, select = -c(PREV_TRE_CN))
```
## Merging remeasured plots

```r
#now we merge the beginning and ending years for each cycle

#2009 to 2016
data0916<- checkdat1609
data0916$DIA09 <- checkdat0916$DIA[match(checkdat1609$CN,checkdat0916$CN)]
data0916$DIA16<- data0916$DIA
data0916<- data0916 %>% select(-c(DIA ))
data0916$HT09<- checkdat0916$HT[match(checkdat1609$CN, checkdat0916$CN)]
data0916$HT16<- data0916$HT
data0916<- data0916 %>% select(-c(HT, INVYR))
data0916<- data0916 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
#now i will make a new col that has county and plot
data0916$COUNTY_PLOT <- paste(data0916$COUNTYCD,data0916$PLOT, sep="_")

#2009 to 2017
data0917<- checkdat1709
data0917$DIA09 <- checkdat0917$DIA[match(checkdat1709$CN,checkdat0917$CN)]
data0917$DIA17<- data0917$DIA
data0917<- data0917 %>% select(-c(DIA ))
data0917$HT09<- checkdat0917$HT[match(checkdat1709$CN, checkdat0917$CN)]
data0917$HT17<- data0917$HT
data0917<- data0917 %>% select(-c(HT, INVYR))
data0917<- data0917 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data0917$COUNTY_PLOT <- paste(data0917$COUNTYCD,data0917$PLOT, sep="_")

#2009 to 2018
data0918<- checkdat1809
data0918$DIA09 <- checkdat0918$DIA[match(checkdat1809$CN,checkdat0918$CN)]
data0918$DIA18<- data0918$DIA
data0918<- data0918 %>% select(-c(DIA ))
data0918$HT09<- checkdat0918$HT[match(checkdat1809$CN, checkdat0918$CN)]
data0918$HT18<- data0918$HT
data0918<- data0918 %>% select(-c(HT, INVYR))
data0918<- data0918 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data0918$COUNTY_PLOT <- paste(data0918$COUNTYCD,data0918$PLOT, sep="_")

#2010 to 2016
data1016<-checkdat1610
data1016$DIA10 <- checkdat1016$DIA[match(checkdat1610$CN,checkdat1016$CN)]
data1016$DIA16<- data1016$DIA
data1016<- data1016 %>% select(-c(DIA ))
data1016$HT10<- checkdat1016$HT[match(checkdat1610$CN, checkdat1016$CN)]
data1016$HT16<- data1016$HT
data1016<- data1016 %>% select(-c(HT, INVYR))
data1016<- data1016 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1016$COUNTY_PLOT <- paste(data1016$COUNTYCD,data1016$PLOT, sep="_")

#2010 to 2018
data1018<-checkdat1810
data1018$DIA10 <- checkdat1018$DIA[match(checkdat1810$CN,checkdat1018$CN)]
data1018$DIA18<- data1018$DIA
data1018<- data1018 %>% select(-c(DIA ))
data1018$HT10<- checkdat1018$HT[match(checkdat1810$CN, checkdat1018$CN)]
data1018$HT18<- data1018$HT
data1018<- data1018 %>% select(-c(HT, INVYR))
data1018<- data1018 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1018$COUNTY_PLOT <- paste(data1018$COUNTYCD,data1018$PLOT, sep="_")

#2010 to 2019
data1019<-checkdat1910
data1019$DIA10 <- checkdat1019$DIA[match(checkdat1910$CN,checkdat1019$CN)]
data1019$DIA19<- data1019$DIA
data1019<- data1019 %>% select(-c(DIA ))
data1019$HT10<- checkdat1019$HT[match(checkdat1910$CN, checkdat1019$CN)]
data1019$HT19<- data1019$HT
data1019<- data1019 %>% select(-c(HT, INVYR))
data1019<- data1019 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1019$COUNTY_PLOT <- paste(data1019$COUNTYCD,data1019$PLOT, sep="_")

#2011 to 2016
data1116<-checkdat1611
data1116$DIA11 <- checkdat1116$DIA[match(checkdat1611$CN,checkdat1116$CN)]
data1116$DIA16<- data1116$DIA
data1116<- data1116 %>% select(-c(DIA ))
data1116$HT11<- checkdat1116$HT[match(checkdat1611$CN, checkdat1116$CN)]
data1116$HT16<- data1116$HT
data1116<- data1116 %>% select(-c(HT, INVYR))
data1116<- data1116 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1116$COUNTY_PLOT <- paste(data1116$COUNTYCD,data1116$PLOT, sep="_")

#2011 to 2017
data1117<-checkdat1711
data1117$DIA11 <- checkdat1117$DIA[match(checkdat1711$CN,checkdat1117$CN)]
data1117$DIA17<- data1117$DIA
data1117<- data1117 %>% select(-c(DIA ))
data1117$HT11<- checkdat1117$HT[match(checkdat1711$CN, checkdat1117$CN)]
data1117$HT17<- data1117$HT
data1117<- data1117 %>% select(-c(HT, INVYR))
data1117<- data1117 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1117$COUNTY_PLOT <- paste(data1117$COUNTYCD,data1117$PLOT, sep="_")

#2011 to 2018
data1118<-checkdat1811
data1118$DIA11 <- checkdat1118$DIA[match(checkdat1811$CN,checkdat1118$CN)]
data1118$DIA18<- data1118$DIA
data1118<- data1118 %>% select(-c(DIA ))
data1118$HT11<- checkdat1118$HT[match(checkdat1811$CN, checkdat1118$CN)]
data1118$HT18<- data1118$HT
data1118<- data1118 %>% select(-c(HT, INVYR))
data1118<- data1118 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1118$COUNTY_PLOT <- paste(data1118$COUNTYCD,data1118$PLOT, sep="_")

#2011 to 2019
data1119<-checkdat1911
data1119$DIA11 <- checkdat1119$DIA[match(checkdat1911$CN,checkdat1119$CN)]
data1119$DIA19<- data1119$DIA
data1119<- data1119 %>% select(-c(DIA ))
data1119$HT11<- checkdat1119$HT[match(checkdat1911$CN, checkdat1119$CN)]
data1119$HT19<- data1119$HT
data1119<- data1119 %>% select(-c(HT, INVYR))
data1119<- data1119 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1119$COUNTY_PLOT <- paste(data1119$COUNTYCD,data1119$PLOT, sep="_")

#2012 to 2016
data1216<-checkdat1612
data1216$DIA12 <- checkdat1216$DIA[match(checkdat1612$CN,checkdat1216$CN)]
data1216$DIA16<- data1216$DIA
data1216<- data1216 %>% select(-c(DIA ))
data1216$HT12<- checkdat1216$HT[match(checkdat1612$CN, checkdat1216$CN)]
data1216$HT16<- data1216$HT
data1216<- data1216 %>% select(-c(HT, INVYR))
data1216<- data1216 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1216$COUNTY_PLOT <- paste(data1216$COUNTYCD,data1216$PLOT, sep="_")

#2012 to 2017
data1217<-checkdat1712
data1217$DIA12 <- checkdat1217$DIA[match(checkdat1712$CN,checkdat1217$CN)]
data1217$DIA17<- data1217$DIA
data1217<- data1217 %>% select(-c(DIA ))
data1217$HT12<- checkdat1217$HT[match(checkdat1712$CN, checkdat1217$CN)]
data1217$HT17<- data1217$HT
data1217<- data1217 %>% select(-c(HT, INVYR))
data1217<- data1217 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1217$COUNTY_PLOT <- paste(data1217$COUNTYCD,data1217$PLOT, sep="_")

#2012 to 2018
data1218<-checkdat1812
data1218$DIA12 <- checkdat1218$DIA[match(checkdat1812$CN,checkdat1218$CN)]
data1218$DIA18<- data1218$DIA
data1218<- data1218 %>% select(-c(DIA ))
data1218$HT12<- checkdat1218$HT[match(checkdat1812$CN, checkdat1218$CN)]
data1218$HT18<- data1218$HT
data1218<- data1218 %>% select(-c(HT, INVYR))
data1218<- data1218 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1218$COUNTY_PLOT <- paste(data1218$COUNTYCD,data1218$PLOT, sep="_")

#2012 to 2019
data1219<-checkdat1912
data1219$DIA12 <- checkdat1219$DIA[match(checkdat1912$CN,checkdat1219$CN)]
data1219$DIA19<- data1219$DIA
data1219<- data1219 %>% select(-c(DIA ))
data1219$HT12<- checkdat1219$HT[match(checkdat1912$CN, checkdat1219$CN)]
data1219$HT19<- data1219$HT
data1219<- data1219 %>% select(-c(HT, INVYR))
data1219<- data1219 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1219$COUNTY_PLOT <- paste(data1219$COUNTYCD,data1219$PLOT, sep="_")

#2013 to 2018
data1318<-checkdat1813
data1318$DIA13 <- checkdat1318$DIA[match(checkdat1813$CN,checkdat1318$CN)]
data1318$DIA18<- data1318$DIA
data1318<- data1318 %>% select(-c(DIA ))
data1318$HT13<- checkdat1318$HT[match(checkdat1813$CN, checkdat1318$CN)]
data1318$HT18<- data1318$HT
data1318<- data1318 %>% select(-c(HT, INVYR))
data1318<- data1318 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1318$COUNTY_PLOT <- paste(data1318$COUNTYCD,data1318$PLOT, sep="_")

#2013 to 2019
data1319<-checkdat1913
data1319$DIA13 <- checkdat1319$DIA[match(checkdat1913$CN,checkdat1319$CN)]
data1319$DIA19<- data1319$DIA
data1319<- data1319 %>% select(-c(DIA ))
data1319$HT13<- checkdat1319$HT[match(checkdat1913$CN, checkdat1319$CN)]
data1319$HT19<- data1319$HT
data1319<- data1319 %>% select(-c(HT, INVYR))
data1319<- data1319 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1319$COUNTY_PLOT <- paste(data1319$COUNTYCD,data1319$PLOT, sep="_")

#2014 to 2018
data1418<-checkdat1814
data1418$DIA14 <- checkdat1418$DIA[match(checkdat1814$CN,checkdat1418$CN)]
data1418$DIA18<- data1418$DIA
data1418<- data1418 %>% select(-c(DIA ))
data1418$HT14<- checkdat1418$HT[match(checkdat1814$CN, checkdat1418$CN)]
data1418$HT18<- data1418$HT
data1418<- data1418 %>% select(-c(HT, INVYR))
data1418<- data1418 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1418$COUNTY_PLOT <- paste(data1418$COUNTYCD,data1418$PLOT, sep="_")

#2014 to 2019
data1419<-checkdat1914
data1419$DIA14 <- checkdat1419$DIA[match(checkdat1914$CN,checkdat1419$CN)]
data1419$DIA19<- data1419$DIA
data1419<- data1419 %>% select(-c(DIA ))
data1419$HT14<- checkdat1419$HT[match(checkdat1914$CN, checkdat1419$CN)]
data1419$HT19<- data1419$HT
data1419<- data1419 %>% select(-c(HT, INVYR))
data1419<- data1419 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1419$COUNTY_PLOT <- paste(data1419$COUNTYCD,data1419$PLOT, sep="_")

#2015 to 2018
data1518<-checkdat1815
data1518$DIA15 <- checkdat1518$DIA[match(checkdat1815$CN,checkdat1518$CN)]
data1518$DIA18<- data1518$DIA
data1518<- data1518 %>% select(-c(DIA ))
data1518$HT15<- checkdat1518$HT[match(checkdat1815$CN, checkdat1518$CN)]
data1518$HT18<- data1518$HT
data1518<- data1518 %>% select(-c(HT, INVYR))
data1518<- data1518 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1518$COUNTY_PLOT <- paste(data1518$COUNTYCD,data1518$PLOT, sep="_")

#2015 to 2019
data1519<-checkdat1915
data1519$DIA15 <- checkdat1519$DIA[match(checkdat1915$CN,checkdat1519$CN)]
data1519$DIA19<- data1519$DIA
data1519<- data1519 %>% select(-c(DIA ))
data1519$HT15<- checkdat1519$HT[match(checkdat1915$CN, checkdat1519$CN)]
data1519$HT19<- data1519$HT
data1519<- data1519 %>% select(-c(HT, INVYR))
data1519<- data1519 %>% group_by(COUNTYCD) %>% arrange(PLOT, .by_group=TRUE)
data1519$COUNTY_PLOT <- paste(data1519$COUNTYCD,data1519$PLOT, sep="_")
```

## Adding columns from COND to filter then removing them

```r
#2009-16
data0916$STDORGCD09 <- cond09$STDORGCD[match(data0916$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0916$STDORGCD16 <- cond16$STDORGCD[match(data0916$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data0916<- data0916 %>% filter(STDORGCD09==0)
data0916<- data0916 %>% filter(STDORGCD16==0)
data0916<- data0916 %>% select(-c(STDORGCD09,STDORGCD16))

data0916$DSTRB09 <- cond09$DSTRBCD1[match(data0916$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0916$DSTRB16 <- cond16$DSTRBCD1[match(data0916$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data0916<- data0916 %>% filter(DSTRB09==0)
data0916<- data0916 %>% filter(DSTRB16==0)
data0916<- data0916 %>% select(-c(DSTRB09, DSTRB16))

data0916$TRT09 <- cond09$TRTCD1[match(data0916$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0916$TRT16 <- cond16$TRTCD1[match(data0916$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data0916<- data0916 %>% filter(TRT09==0)
data0916<- data0916 %>% filter(TRT16==0)
data0916<- data0916 %>% select(-c(TRT09, TRT16))

#2009-17
data0917$STDORGCD09 <- cond09$STDORGCD[match(data0917$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0917$STDORGCD17 <- cond17$STDORGCD[match(data0917$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data0917<- data0917 %>% filter(STDORGCD09==0)
data0917<- data0917 %>% filter(STDORGCD17==0)
data0917<- data0917 %>% select(-c(STDORGCD09,STDORGCD17))

data0917$DSTRB09 <- cond09$DSTRBCD1[match(data0917$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0917$DSTRB17 <- cond17$DSTRBCD1[match(data0917$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data0917<- data0917 %>% filter(DSTRB09==0)
data0917<- data0917 %>% filter(DSTRB17==0)
data0917<- data0917 %>% select(-c(DSTRB09, DSTRB17))

data0917$TRT09 <- cond09$TRTCD1[match(data0917$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0917$TRT17 <- cond17$TRTCD1[match(data0917$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data0917<- data0917 %>% filter(TRT09==0)
data0917<- data0917 %>% filter(TRT17==0)
data0917<- data0917 %>% select(-c(TRT09, TRT17))

#2009-18
data0918$STDORGCD09 <- cond09$STDORGCD[match(data0918$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0918$STDORGCD18 <- cond18$STDORGCD[match(data0918$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data0918<- data0918 %>% filter(STDORGCD09==0)
data0918<- data0918 %>% filter(STDORGCD18==0)
data0918<- data0918 %>% select(-c(STDORGCD09,STDORGCD18))

data0918$DSTRB09 <- cond09$DSTRBCD1[match(data0918$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0918$DSTRB18 <- cond18$DSTRBCD1[match(data0918$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data0918<- data0918 %>% filter(DSTRB09==0)
data0918<- data0918 %>% filter(DSTRB18==0)
data0918<- data0918 %>% select(-c(DSTRB09, DSTRB18))

data0918$TRT09 <- cond09$TRTCD1[match(data0918$COUNTY_PLOT, cond09$COUNTY_PLOT)]
data0918$TRT18 <- cond18$TRTCD1[match(data0918$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data0918<- data0918 %>% filter(TRT09==0)
data0918<- data0918 %>% filter(TRT18==0)
data0918<- data0918 %>% select(-c(TRT09, TRT18))

#2010-16
data1016$STDORGCD10 <- cond10$STDORGCD[match(data1016$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1016$STDORGCD16 <- cond16$STDORGCD[match(data1016$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1016<- data1016 %>% filter(STDORGCD10==0)
data1016<- data1016 %>% filter(STDORGCD16==0)
data1016<- data1016 %>% select(-c(STDORGCD10,STDORGCD16))

data1016$DSTRB10 <- cond10$DSTRBCD1[match(data1016$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1016$DSTRB16 <- cond16$DSTRBCD1[match(data1016$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1016<- data1016 %>% filter(DSTRB10==0)
data1016<- data1016 %>% filter(DSTRB16==0)
data1016<- data1016 %>% select(-c(DSTRB10, DSTRB16))

data1016$TRT10 <- cond10$TRTCD1[match(data1016$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1016$TRT16 <- cond16$TRTCD1[match(data1016$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1016<- data1016 %>% filter(TRT10==0)
data1016<- data1016 %>% filter(TRT16==0)
data1016<- data1016 %>% select(-c(TRT10, TRT16))

#2010-18
data1018$STDORGCD10 <- cond10$STDORGCD[match(data1018$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1018$STDORGCD18 <- cond18$STDORGCD[match(data1018$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1018<- data1018 %>% filter(STDORGCD10==0)
data1018<- data1018 %>% filter(STDORGCD18==0)
data1018<- data1018 %>% select(-c(STDORGCD10,STDORGCD18))

data1018$DSTRB10 <- cond10$DSTRBCD1[match(data1018$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1018$DSTRB18 <- cond18$DSTRBCD1[match(data1018$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1018<- data1018 %>% filter(DSTRB10==0)
data1018<- data1018 %>% filter(DSTRB18==0)
data1018<- data1018 %>% select(-c(DSTRB10, DSTRB18))

data1018$TRT10 <- cond10$TRTCD1[match(data1018$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1018$TRT18 <- cond18$TRTCD1[match(data1018$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1018<- data1018 %>% filter(TRT10==0)
data1018<- data1018 %>% filter(TRT18==0)
data1018<- data1018 %>% select(-c(TRT10, TRT18))

#2010-19
data1019$STDORGCD10 <- cond10$STDORGCD[match(data1019$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1019$STDORGCD19 <- cond19$STDORGCD[match(data1019$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1019<- data1019 %>% filter(STDORGCD10==0)
data1019<- data1019 %>% filter(STDORGCD19==0)
data1019<- data1019 %>% select(-c(STDORGCD10,STDORGCD19))

data1019$DSTRB10 <- cond10$DSTRBCD1[match(data1019$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1019$DSTRB19 <- cond19$DSTRBCD1[match(data1019$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1019<- data1019 %>% filter(DSTRB10==0)
data1019<- data1019 %>% filter(DSTRB19==0)
data1019<- data1019 %>% select(-c(DSTRB10, DSTRB19))

data1019$TRT10 <- cond10$TRTCD1[match(data1019$COUNTY_PLOT, cond10$COUNTY_PLOT)]
data1019$TRT19 <- cond19$TRTCD1[match(data1019$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1019<- data1019 %>% filter(TRT10==0)
data1019<- data1019 %>% filter(TRT19==0)
data1019<- data1019 %>% select(-c(TRT10, TRT19))

#2011-16
data1116$STDORGCD11 <- cond11$STDORGCD[match(data1116$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1116$STDORGCD16 <- cond16$STDORGCD[match(data1116$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1116<- data1116 %>% filter(STDORGCD11==0)
data1116<- data1116 %>% filter(STDORGCD16==0)
data1116<- data1116 %>% select(-c(STDORGCD11,STDORGCD16))

data1116$DSTRB11 <- cond11$DSTRBCD1[match(data1116$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1116$DSTRB16 <- cond16$DSTRBCD1[match(data1116$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1116<- data1116 %>% filter(DSTRB11==0)
data1116<- data1116 %>% filter(DSTRB16==0)
data1116<- data1116 %>% select(-c(DSTRB11, DSTRB16))

data1116$TRT11 <- cond11$TRTCD1[match(data1116$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1116$TRT16 <- cond16$TRTCD1[match(data1116$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1116<- data1116 %>% filter(TRT11==0)
data1116<- data1116 %>% filter(TRT16==0)
data1116<- data1116 %>% select(-c(TRT11, TRT16))

#2011-17
data1117$STDORGCD11 <- cond11$STDORGCD[match(data1117$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1117$STDORGCD17 <- cond17$STDORGCD[match(data1117$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data1117<- data1117 %>% filter(STDORGCD11==0)
data1117<- data1117 %>% filter(STDORGCD17==0)
data1117<- data1117 %>% select(-c(STDORGCD11,STDORGCD17))

data1117$DSTRB11 <- cond11$DSTRBCD1[match(data1117$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1117$DSTRB17 <- cond17$DSTRBCD1[match(data1117$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data1117<- data1117 %>% filter(DSTRB11==0)
data1117<- data1117 %>% filter(DSTRB17==0)
data1117<- data1117 %>% select(-c(DSTRB11, DSTRB17))

data1117$TRT11 <- cond11$TRTCD1[match(data1117$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1117$TRT17 <- cond17$TRTCD1[match(data1117$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data1117<- data1117 %>% filter(TRT11==0)
data1117<- data1117 %>% filter(TRT17==0)
data1117<- data1117 %>% select(-c(TRT11, TRT17))

#2011-18
data1118$STDORGCD11 <- cond11$STDORGCD[match(data1118$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1118$STDORGCD18 <- cond18$STDORGCD[match(data1118$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1118<- data1118 %>% filter(STDORGCD11==0)
data1118<- data1118 %>% filter(STDORGCD18==0)
data1118<- data1118 %>% select(-c(STDORGCD11,STDORGCD18))

data1118$DSTRB11 <- cond11$DSTRBCD1[match(data1118$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1118$DSTRB18 <- cond18$DSTRBCD1[match(data1118$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1118<- data1118 %>% filter(DSTRB11==0)
data1118<- data1118 %>% filter(DSTRB18==0)
data1118<- data1118 %>% select(-c(DSTRB11, DSTRB18))

data1118$TRT11 <- cond11$TRTCD1[match(data1118$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1118$TRT18 <- cond18$TRTCD1[match(data1118$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1118<- data1118 %>% filter(TRT11==0)
data1118<- data1118 %>% filter(TRT18==0)
data1118<- data1118 %>% select(-c(TRT11, TRT18))

#2011-19
data1119$STDORGCD11 <- cond11$STDORGCD[match(data1119$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1119$STDORGCD19 <- cond19$STDORGCD[match(data1119$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1119<- data1119 %>% filter(STDORGCD11==0)
data1119<- data1119 %>% filter(STDORGCD19==0)
data1119<- data1119 %>% select(-c(STDORGCD11,STDORGCD19))

data1119$DSTRB11 <- cond11$DSTRBCD1[match(data1119$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1119$DSTRB19 <- cond19$DSTRBCD1[match(data1119$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1119<- data1119 %>% filter(DSTRB11==0)
data1119<- data1119 %>% filter(DSTRB19==0)
data1119<- data1119 %>% select(-c(DSTRB11, DSTRB19))

data1119$TRT11 <- cond11$TRTCD1[match(data1119$COUNTY_PLOT, cond11$COUNTY_PLOT)]
data1119$TRT19 <- cond19$TRTCD1[match(data1119$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1119<- data1119 %>% filter(TRT11==0)
data1119<- data1119 %>% filter(TRT19==0)
data1119<- data1119 %>% select(-c(TRT11, TRT19))

#2012-16
data1216$STDORGCD12 <- cond12$STDORGCD[match(data1216$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1216$STDORGCD16 <- cond16$STDORGCD[match(data1216$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1216<- data1216 %>% filter(STDORGCD12==0)
data1216<- data1216 %>% filter(STDORGCD16==0)
data1216<- data1216 %>% select(-c(STDORGCD12,STDORGCD16))

data1216$DSTRB12 <- cond12$DSTRBCD1[match(data1216$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1216$DSTRB16 <- cond16$DSTRBCD1[match(data1216$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1216<- data1216 %>% filter(DSTRB12==0)
data1216<- data1216 %>% filter(DSTRB16==0)
data1216<- data1216 %>% select(-c(DSTRB12, DSTRB16))

data1216$TRT12 <- cond12$TRTCD1[match(data1216$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1216$TRT16 <- cond16$TRTCD1[match(data1216$COUNTY_PLOT, cond16$COUNTY_PLOT)]
data1216<- data1216 %>% filter(TRT12==0)
data1216<- data1216 %>% filter(TRT16==0)
data1216<- data1216 %>% select(-c(TRT12, TRT16))

#2012-17
data1217$STDORGCD12 <- cond12$STDORGCD[match(data1217$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1217$STDORGCD17 <- cond17$STDORGCD[match(data1217$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data1217<- data1217 %>% filter(STDORGCD12==0)
data1217<- data1217 %>% filter(STDORGCD17==0)
data1217<- data1217 %>% select(-c(STDORGCD12,STDORGCD17))

data1217$DSTRB12 <- cond12$DSTRBCD1[match(data1217$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1217$DSTRB17 <- cond17$DSTRBCD1[match(data1217$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data1217<- data1217 %>% filter(DSTRB12==0)
data1217<- data1217 %>% filter(DSTRB17==0)
data1217<- data1217 %>% select(-c(DSTRB12, DSTRB17))

data1217$TRT12 <- cond12$TRTCD1[match(data1217$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1217$TRT17 <- cond17$TRTCD1[match(data1217$COUNTY_PLOT, cond17$COUNTY_PLOT)]
data1217<- data1217 %>% filter(TRT12==0)
data1217<- data1217 %>% filter(TRT17==0)
data1217<- data1217 %>% select(-c(TRT12, TRT17))

#2012-18
data1218$STDORGCD12 <- cond12$STDORGCD[match(data1218$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1218$STDORGCD18 <- cond18$STDORGCD[match(data1218$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1218<- data1218 %>% filter(STDORGCD12==0)
data1218<- data1218 %>% filter(STDORGCD18==0)
data1218<- data1218 %>% select(-c(STDORGCD12,STDORGCD18))

data1218$DSTRB12 <- cond12$DSTRBCD1[match(data1218$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1218$DSTRB18 <- cond18$DSTRBCD1[match(data1218$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1218<- data1218 %>% filter(DSTRB12==0)
data1218<- data1218 %>% filter(DSTRB18==0)
data1218<- data1218 %>% select(-c(DSTRB12, DSTRB18))

data1218$TRT12 <- cond12$TRTCD1[match(data1218$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1218$TRT18 <- cond18$TRTCD1[match(data1218$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1218<- data1218 %>% filter(TRT12==0)
data1218<- data1218 %>% filter(TRT18==0)
data1218<- data1218 %>% select(-c(TRT12, TRT18))

#2012-19
data1219$STDORGCD12 <- cond12$STDORGCD[match(data1219$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1219$STDORGCD19 <- cond19$STDORGCD[match(data1219$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1219<- data1219 %>% filter(STDORGCD12==0)
data1219<- data1219 %>% filter(STDORGCD19==0)
data1219<- data1219 %>% select(-c(STDORGCD12,STDORGCD19))

data1219$DSTRB12 <- cond12$DSTRBCD1[match(data1219$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1219$DSTRB19 <- cond19$DSTRBCD1[match(data1219$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1219<- data1219 %>% filter(DSTRB12==0)
data1219<- data1219 %>% filter(DSTRB19==0)
data1219<- data1219 %>% select(-c(DSTRB12, DSTRB19))

data1219$TRT12 <- cond12$TRTCD1[match(data1219$COUNTY_PLOT, cond12$COUNTY_PLOT)]
data1219$TRT19 <- cond19$TRTCD1[match(data1219$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1219<- data1219 %>% filter(TRT12==0)
data1219<- data1219 %>% filter(TRT19==0)
data1219<- data1219 %>% select(-c(TRT12, TRT19))

#2013-18
data1318$STDORGCD13 <- cond13$STDORGCD[match(data1318$COUNTY_PLOT, cond13$COUNTY_PLOT)]
data1318$STDORGCD18 <- cond18$STDORGCD[match(data1318$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1318<- data1318 %>% filter(STDORGCD13==0)
data1318<- data1318 %>% filter(STDORGCD18==0)
data1318<- data1318 %>% select(-c(STDORGCD13,STDORGCD18))

data1318$DSTRB13 <- cond13$DSTRBCD1[match(data1318$COUNTY_PLOT, cond13$COUNTY_PLOT)]
data1318$DSTRB18 <- cond18$DSTRBCD1[match(data1318$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1318<- data1318 %>% filter(DSTRB13==0)
data1318<- data1318 %>% filter(DSTRB18==0)
data1318<- data1318 %>% select(-c(DSTRB13, DSTRB18))

data1318$TRT13 <- cond13$TRTCD1[match(data1318$COUNTY_PLOT, cond13$COUNTY_PLOT)]
data1318$TRT18 <- cond18$TRTCD1[match(data1318$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1318<- data1318 %>% filter(TRT13==0)
data1318<- data1318 %>% filter(TRT18==0)
data1318<- data1318 %>% select(-c(TRT13, TRT18))

#2013-19
data1319$STDORGCD13 <- cond13$STDORGCD[match(data1319$COUNTY_PLOT, cond13$COUNTY_PLOT)]
data1319$STDORGCD19 <- cond19$STDORGCD[match(data1319$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1319<- data1319 %>% filter(STDORGCD13==0)
data1319<- data1319 %>% filter(STDORGCD19==0)
data1319<- data1319 %>% select(-c(STDORGCD13,STDORGCD19))

data1319$DSTRB13 <- cond13$DSTRBCD1[match(data1319$COUNTY_PLOT, cond13$COUNTY_PLOT)]
data1319$DSTRB19 <- cond19$DSTRBCD1[match(data1319$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1319<- data1319 %>% filter(DSTRB13==0)
data1319<- data1319 %>% filter(DSTRB19==0)
data1319<- data1319 %>% select(-c(DSTRB13, DSTRB19))

data1319$TRT13 <- cond13$TRTCD1[match(data1319$COUNTY_PLOT, cond13$COUNTY_PLOT)]
data1319$TRT19 <- cond19$TRTCD1[match(data1319$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1319<- data1319 %>% filter(TRT13==0)
data1319<- data1319 %>% filter(TRT19==0)
data1319<- data1319 %>% select(-c(TRT13, TRT19))

#2014-18
data1418$STDORGCD14 <- cond14$STDORGCD[match(data1418$COUNTY_PLOT, cond14$COUNTY_PLOT)]
data1418$STDORGCD18 <- cond18$STDORGCD[match(data1418$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1418<- data1418 %>% filter(STDORGCD14==0)
data1418<- data1418 %>% filter(STDORGCD18==0)
data1418<- data1418 %>% select(-c(STDORGCD14,STDORGCD18))

data1418$DSTRB14 <- cond14$DSTRBCD1[match(data1418$COUNTY_PLOT, cond14$COUNTY_PLOT)]
data1418$DSTRB18 <- cond18$DSTRBCD1[match(data1418$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1418<- data1418 %>% filter(DSTRB14==0)
data1418<- data1418 %>% filter(DSTRB18==0)
data1418<- data1418 %>% select(-c(DSTRB14, DSTRB18))

data1418$TRT14 <- cond14$TRTCD1[match(data1418$COUNTY_PLOT, cond14$COUNTY_PLOT)]
data1418$TRT18 <- cond18$TRTCD1[match(data1418$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1418<- data1418 %>% filter(TRT14==0)
data1418<- data1418 %>% filter(TRT18==0)
data1418<- data1418 %>% select(-c(TRT14, TRT18))

#2014-19
data1419$STDORGCD14 <- cond14$STDORGCD[match(data1419$COUNTY_PLOT, cond14$COUNTY_PLOT)]
data1419$STDORGCD19 <- cond19$STDORGCD[match(data1419$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1419<- data1419 %>% filter(STDORGCD14==0)
data1419<- data1419 %>% filter(STDORGCD19==0)
data1419<- data1419 %>% select(-c(STDORGCD14,STDORGCD19))

data1419$DSTRB14 <- cond14$DSTRBCD1[match(data1419$COUNTY_PLOT, cond14$COUNTY_PLOT)]
data1419$DSTRB19 <- cond19$DSTRBCD1[match(data1419$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1419<- data1419 %>% filter(DSTRB14==0)
data1419<- data1419 %>% filter(DSTRB19==0)
data1419<- data1419 %>% select(-c(DSTRB14, DSTRB19))

data1419$TRT14 <- cond14$TRTCD1[match(data1419$COUNTY_PLOT, cond14$COUNTY_PLOT)]
data1419$TRT19 <- cond19$TRTCD1[match(data1419$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1419<- data1419 %>% filter(TRT14==0)
data1419<- data1419 %>% filter(TRT19==0)
data1419<- data1419 %>% select(-c(TRT14, TRT19))

#2015-18
data1518$STDORGCD15 <- cond15$STDORGCD[match(data1518$COUNTY_PLOT, cond15$COUNTY_PLOT)]
data1518$STDORGCD18 <- cond18$STDORGCD[match(data1518$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1518<- data1518 %>% filter(STDORGCD15==0)
data1518<- data1518 %>% filter(STDORGCD18==0)
data1518<- data1518 %>% select(-c(STDORGCD15,STDORGCD18))

data1518$DSTRB15 <- cond15$DSTRBCD1[match(data1518$COUNTY_PLOT, cond15$COUNTY_PLOT)]
data1518$DSTRB18 <- cond18$DSTRBCD1[match(data1518$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1518<- data1518 %>% filter(DSTRB15==0)
data1518<- data1518 %>% filter(DSTRB18==0)
data1518<- data1518 %>% select(-c(DSTRB15, DSTRB18))

data1518$TRT15 <- cond14$TRTCD1[match(data1518$COUNTY_PLOT, cond15$COUNTY_PLOT)]
data1518$TRT18 <- cond18$TRTCD1[match(data1518$COUNTY_PLOT, cond18$COUNTY_PLOT)]
data1518<- data1518 %>% filter(TRT15==0)
data1518<- data1518 %>% filter(TRT18==0)
data1518<- data1518 %>% select(-c(TRT15, TRT18))

#2015-19
data1519$STDORGCD15 <- cond15$STDORGCD[match(data1519$COUNTY_PLOT, cond15$COUNTY_PLOT)]
data1519$STDORGCD19 <- cond19$STDORGCD[match(data1519$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1519<- data1519 %>% filter(STDORGCD15==0)
data1519<- data1519 %>% filter(STDORGCD19==0)
data1519<- data1519 %>% select(-c(STDORGCD15,STDORGCD19))

data1519$DSTRB15 <- cond15$DSTRBCD1[match(data1519$COUNTY_PLOT, cond15$COUNTY_PLOT)]
data1519$DSTRB19 <- cond19$DSTRBCD1[match(data1519$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1519<- data1519 %>% filter(DSTRB15==0)
data1519<- data1519 %>% filter(DSTRB19==0)
data1519<- data1519 %>% select(-c(DSTRB15, DSTRB19))

data1519$TRT15 <- cond15$TRTCD1[match(data1519$COUNTY_PLOT, cond15$COUNTY_PLOT)]
data1519$TRT19 <- cond19$TRTCD1[match(data1519$COUNTY_PLOT, cond19$COUNTY_PLOT)]
data1519<- data1519 %>% filter(TRT15==0)
data1519<- data1519 %>% filter(TRT19==0)
data1519<- data1519 %>% select(-c(TRT15, TRT19))
```

##biomass function

```r
biomass_function <- function(spp, dbh) { 
  case_when(
    spp== 43  ~ exp( -2.7765   +    2.4195 *log(dbh)),                
    spp== 68  ~ exp( -2.6327   +    2.4757 *log(dbh)),
    spp== 110 ~ exp( -3.0506   +    2.6465 *log(dbh)),
    spp== 111 ~ exp( -3.0506   +    2.6465 *log(dbh)),
    spp== 115 ~ exp( -2.6177   +    2.4638 *log(dbh)),
    spp== 121 ~ exp( -3.0506   +    2.6465 *log(dbh)),
    spp== 131 ~ exp( -3.0506   +    2.6465 *log(dbh)),
    spp== 132 ~ exp( -3.0506   +    2.6465 *log(dbh)),
    spp== 221 ~ exp( -2.6327   +    2.4757 *log(dbh)),
    spp== 222 ~ exp( -2.6327   +    2.4757 *log(dbh)),
    spp== 311 ~ exp( -2.0470   +    2.3852 *log(dbh)),
    spp== 313 ~ exp( -2.0470   +    2.3852 *log(dbh)),
    spp== 316 ~ exp( -2.0470   +    2.3852 *log(dbh)),
    spp== 317 ~ exp( -2.0470   +    2.3852 *log(dbh)),
    spp== 341 ~ exp( -1.8011   +    2.3852 *log(dbh)),
    spp== 345 ~ exp( -2.5095   +    2.5437 *log(dbh)),
    spp== 356 ~ exp( -2.2118   +    2.4133 *log(dbh)), 
    spp== 367 ~ exp( -2.5497   +    2.5011 *log(dbh)),
    spp== 373 ~ exp( -1.8096   +    2.3480 *log(dbh)),
    spp== 381 ~ exp( -2.0470   +    2.3852 *log(dbh)),
    spp== 391 ~ exp( -2.2652   +    2.5349 *log(dbh)),
    spp== 401 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 402 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 403 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 404 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 405 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 407 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 408 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 409 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 413 ~ exp( -2.5095   +    2.6175 *log(dbh)),
    spp== 451 ~ exp( -2.0314   +    2.3524 *log(dbh)),
    spp== 461 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 471 ~ exp( -2.5095   +    2.5437 *log(dbh)),
    spp== 491 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 500 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 521 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 531 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 541 ~ exp( -1.8384   +    2.3524 *log(dbh)),
    spp== 544 ~ exp( -2.0314   +    2.3524 *log(dbh)),
    spp== 552 ~ exp( -2.5095   +    2.5437 *log(dbh)),
    spp== 555 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 571 ~ exp( -2.5095   +    2.5437 *log(dbh)),
    spp== 582 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 591 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 602 ~ exp( -2.5095   +    2.5437 *log(dbh)),
    spp== 611 ~ exp( -2.6390   +    2.5466 *log(dbh)),
    spp== 621 ~ exp( -2.5497   +    2.5011 *log(dbh)),
    spp== 641 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 651 ~ exp( -2.5497   +    2.5011 *log(dbh)),
    spp== 652 ~ exp( -2.5497   +    2.5011 *log(dbh)),
    spp== 653 ~ exp( -2.5497   +    2.5011 *log(dbh)),
    spp== 654 ~ exp( -2.5497   +    2.5011 *log(dbh)),
    spp== 662 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 682 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 691 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 693 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 694 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 701 ~ exp( -2.2652   +    2.5349 *log(dbh)),
    spp== 711 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 712 ~ exp( -2.0314   +    2.3524 *log(dbh)),
    spp== 721 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 722 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 731 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 742 ~ exp( -2.4441   +    2.4561 *log(dbh)),
    spp== 760 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 762 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 763 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 802 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 806 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 808 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 812 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 813 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 819 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 820 ~ exp( -2.2198   +    2.4410 *log(dbh)),
    spp== 822 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 824 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 825 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 826 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 827 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 828 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 831 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 832 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 833 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 834 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 835 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 837 ~ exp( -2.0705   +    2.4410 *log(dbh)),
    spp== 838 ~ exp( -2.2198   +    2.4410 *log(dbh)),
    spp== 901 ~ exp( -2.5095   +    2.5437 *log(dbh)),
    spp== 922 ~ exp( -2.4441   +    2.4561 *log(dbh)),
    spp== 929 ~ exp( -2.4441   +    2.4561 *log(dbh)),
    spp== 931 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 951 ~ exp( -2.4108   +    2.4177 *log(dbh)),
    spp== 953 ~ exp( -2.4108   +    2.4177 *log(dbh)),
    spp== 971 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 972 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 973 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 975 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 993 ~ exp( -1.8011   +    2.3852 *log(dbh)),
    spp== 994 ~ exp( -2.2118   +    2.4133 *log(dbh)),
    spp== 995 ~ exp( -2.2118   +    2.4133 *log(dbh)),)
}
```

#applying biomass equation

```r
#2009-16
bio0916<- data0916 %>% mutate(bio09 = biomass_function(spp = SPCD, dbh = DIA09))
bio0916<- bio0916 %>% mutate(bio16 = biomass_function(spp = SPCD, dbh = DIA16))

#2009-17
bio0917<- data0917 %>% mutate(bio09 = biomass_function(spp = SPCD, dbh = DIA09))
bio0917<- bio0917 %>% mutate(bio17 = biomass_function(spp = SPCD, dbh = DIA17))

#2009-18
bio0918<- data0918 %>% mutate(bio09 = biomass_function(spp = SPCD, dbh = DIA09))
bio0918<- bio0918 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2010-16
bio1016<- data1016 %>% mutate(bio10 = biomass_function(spp = SPCD, dbh = DIA10))
bio1016<- bio1016 %>% mutate(bio16 = biomass_function(spp = SPCD, dbh = DIA16))

#2010-18
bio1018<- data1018 %>% mutate(bio10 = biomass_function(spp = SPCD, dbh = DIA10))
bio1018<- bio1018 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2010-19
bio1019<- data1019 %>% mutate(bio10 = biomass_function(spp = SPCD, dbh = DIA10))
bio1019<- bio1019 %>% mutate(bio19 = biomass_function(spp = SPCD, dbh = DIA19))

#2011-16
bio1116<- data1116 %>% mutate(bio11 = biomass_function(spp = SPCD, dbh = DIA11))
bio1116<- bio1116 %>% mutate(bio16 = biomass_function(spp = SPCD, dbh = DIA16))

#2011-17
bio1117<- data1117 %>% mutate(bio11 = biomass_function(spp = SPCD, dbh = DIA11))
bio1117<- bio1117 %>% mutate(bio17 = biomass_function(spp = SPCD, dbh = DIA17))

#2011-18
bio1118<- data1118 %>% mutate(bio11 = biomass_function(spp = SPCD, dbh = DIA11))
bio1118<- bio1118 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2011-19
bio1119<- data1119 %>% mutate(bio11 = biomass_function(spp = SPCD, dbh = DIA11))
bio1119<- bio1119 %>% mutate(bio19 = biomass_function(spp = SPCD, dbh = DIA19))

#2012-16
bio1216<- data1216 %>% mutate(bio12 = biomass_function(spp = SPCD, dbh = DIA12))
bio1216<- bio1216 %>% mutate(bio16 = biomass_function(spp = SPCD, dbh = DIA16))

#2012-17
bio1217<- data1217 %>% mutate(bio12 = biomass_function(spp = SPCD, dbh = DIA12))
bio1217<- bio1217 %>% mutate(bio17 = biomass_function(spp = SPCD, dbh = DIA17))

#2012-18
bio1218<- data1218 %>% mutate(bio12 = biomass_function(spp = SPCD, dbh = DIA12))
bio1218<- bio1218 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2012-19
bio1219<- data1219 %>% mutate(bio12 = biomass_function(spp = SPCD, dbh = DIA12))
bio1219<- bio1219 %>% mutate(bio19 = biomass_function(spp = SPCD, dbh = DIA19))

#2013-18
bio1318<- data1318 %>% mutate(bio13 = biomass_function(spp = SPCD, dbh = DIA13))
bio1318<- bio1318 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2013-19
bio1319<- data1319 %>% mutate(bio13 = biomass_function(spp = SPCD, dbh = DIA13))
bio1319<- bio1319 %>% mutate(bio19 = biomass_function(spp = SPCD, dbh = DIA19))

#2014-18
bio1418<- data1418 %>% mutate(bio14 = biomass_function(spp = SPCD, dbh = DIA14))
bio1418<- bio1418 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2014-19
bio1419<- data1419 %>% mutate(bio14 = biomass_function(spp = SPCD, dbh = DIA14))
bio1419<- bio1419 %>% mutate(bio19 = biomass_function(spp = SPCD, dbh = DIA19))

#2015-18
bio1518<- data1518 %>% mutate(bio15 = biomass_function(spp = SPCD, dbh = DIA15))
bio1518<- bio1518 %>% mutate(bio18 = biomass_function(spp = SPCD, dbh = DIA18))

#2015-19
bio1519<- data1519 %>% mutate(bio15 = biomass_function(spp = SPCD, dbh = DIA15))
bio1519<- bio1519 %>% mutate(bio19 = biomass_function(spp = SPCD, dbh = DIA19))
```
#summing biomass for each plot

```r
#2009-16
bio0916<- bio0916 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio09=sum(bio09),sumbio16=sum(bio16))
bio0916$bio_change <- ((bio0916$sumbio16- bio0916$sumbio09)/7)
#2009-17
bio0917<- bio0917 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio09=sum(bio09),sumbio17=sum(bio17))
bio0917$bio_change <- ((bio0917$sumbio17- bio0917$sumbio09)/8)
#2009-18
bio0918<- bio0918 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio09=sum(bio09),sumbio18=sum(bio18))
bio0918$bio_change <- ((bio0918$sumbio18- bio0918$sumbio09)/9)
#2010-16
bio1016<- bio1016 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio10=sum(bio10),sumbio16=sum(bio16))
bio1016$bio_change <- ((bio1016$sumbio16- bio1016$sumbio10)/6)
#2010-18
bio1018<- bio1018 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio10=sum(bio10),sumbio18=sum(bio18))
bio1018$bio_change <- ((bio1018$sumbio18- bio1018$sumbio10)/8)
#2010-19
bio1019<- bio1019 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio10=sum(bio10),sumbio19=sum(bio19))
bio1019$bio_change <- ((bio1019$sumbio19- bio1019$sumbio10)/9)
#2011-16
bio1116<- bio1116 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio11=sum(bio11),sumbio16=sum(bio16))
bio1116$bio_change <- ((bio1116$sumbio16- bio1116$sumbio11)/5)
#2011-17
bio1117<- bio1117 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio11=sum(bio11),sumbio17=sum(bio17))
bio1117$bio_change <- ((bio1117$sumbio17- bio1117$sumbio11)/6)
#2011-18
bio1118<- bio1118 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio11=sum(bio11),sumbio18=sum(bio18))
bio1118$bio_change <- ((bio1118$sumbio18- bio1118$sumbio11)/7)
#2011-19
bio1119<- bio1119 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio11=sum(bio11),sumbio19=sum(bio19))
bio1119$bio_change <- ((bio1119$sumbio19- bio1119$sumbio11)/8)
#2012-16
bio1216<- bio1216 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio12=sum(bio12),sumbio16=sum(bio16))
bio1216$bio_change <- ((bio1216$sumbio16- bio1216$sumbio12)/4)
#2012-17
bio1217<- bio1217 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio12=sum(bio12),sumbio17=sum(bio17))
bio1217$bio_change <- ((bio1217$sumbio17- bio1217$sumbio12)/5)
#2012-18
bio1218<- bio1218 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio12=sum(bio12),sumbio18=sum(bio18))
bio1218$bio_change <- ((bio1218$sumbio18- bio1218$sumbio12)/6)
#2012-19
bio1219<- bio1219 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio12=sum(bio12),sumbio19=sum(bio19))
bio1219$bio_change <- ((bio1219$sumbio19- bio1219$sumbio12)/7)
#2013-18
bio1318<- bio1318 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio13=sum(bio13),sumbio18=sum(bio18))
bio1318$bio_change <- ((bio1318$sumbio18- bio1318$sumbio13)/5)
#2013-19
bio1319<- bio1319 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio13=sum(bio13),sumbio19=sum(bio19))
bio1319$bio_change <- ((bio1319$sumbio19- bio1319$sumbio13)/6)
#2014-18
bio1418<- bio1418 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio14=sum(bio14),sumbio18=sum(bio18))
bio1418$bio_change <- ((bio1418$sumbio18- bio1418$sumbio14)/4)
#2014-19
bio1419<- bio1419 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio14=sum(bio14),sumbio19=sum(bio19))
bio1419$bio_change <- ((bio1419$sumbio19- bio1419$sumbio14)/5)
#2015-18
bio1518<- bio1518 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio15=sum(bio15),sumbio18=sum(bio18))
bio1518$bio_change <- ((bio1518$sumbio18- bio1518$sumbio15)/3)
#2015-19
bio1519<- bio1519 %>% group_by(COUNTY_PLOT) %>% mutate(sumbio15=sum(bio15),sumbio19=sum(bio19))
bio1519$bio_change <- ((bio1519$sumbio19- bio1519$sumbio15)/4)
```
#adding cols i want

```r
#making new col with county plot and spcd
bio0916$CNTY_PLT_SPCD<-paste(bio0916$COUNTY_PLOT,data0916$SPCD,sep="#")
bio0917$CNTY_PLT_SPCD<-paste(bio0917$COUNTY_PLOT,data0917$SPCD,sep="#")
bio0918$CNTY_PLT_SPCD<-paste(bio0918$COUNTY_PLOT,data0918$SPCD,sep="#")
bio1016$CNTY_PLT_SPCD<-paste(bio1016$COUNTY_PLOT,data1016$SPCD,sep="#")
bio1018$CNTY_PLT_SPCD<-paste(bio1018$COUNTY_PLOT,data1018$SPCD,sep="#")
bio1019$CNTY_PLT_SPCD<-paste(bio1019$COUNTY_PLOT,data1019$SPCD,sep="#")
bio1116$CNTY_PLT_SPCD<-paste(bio1116$COUNTY_PLOT,data1116$SPCD,sep="#")
bio1117$CNTY_PLT_SPCD<-paste(bio1117$COUNTY_PLOT,data1117$SPCD,sep="#")
bio1118$CNTY_PLT_SPCD<-paste(bio1118$COUNTY_PLOT,data1118$SPCD,sep="#")
bio1119$CNTY_PLT_SPCD<-paste(bio1119$COUNTY_PLOT,data1119$SPCD,sep="#")
bio1216$CNTY_PLT_SPCD<-paste(bio1216$COUNTY_PLOT,data1216$SPCD,sep="#")
bio1217$CNTY_PLT_SPCD<-paste(bio1217$COUNTY_PLOT,data1217$SPCD,sep="#")
bio1218$CNTY_PLT_SPCD<-paste(bio1218$COUNTY_PLOT,data1218$SPCD,sep="#")
bio1219$CNTY_PLT_SPCD<-paste(bio1219$COUNTY_PLOT,data1219$SPCD,sep="#")
bio1318$CNTY_PLT_SPCD<-paste(bio1318$COUNTY_PLOT,data1318$SPCD,sep="#")
bio1319$CNTY_PLT_SPCD<-paste(bio1319$COUNTY_PLOT,data1319$SPCD,sep="#")
bio1418$CNTY_PLT_SPCD<-paste(bio1418$COUNTY_PLOT,data1418$SPCD,sep="#")
bio1419$CNTY_PLT_SPCD<-paste(bio1419$COUNTY_PLOT,data1419$SPCD,sep="#")
bio1518$CNTY_PLT_SPCD<-paste(bio1518$COUNTY_PLOT,data1518$SPCD,sep="#")
bio1519$CNTY_PLT_SPCD<-paste(bio1519$COUNTY_PLOT,data1519$SPCD,sep="#")
```

#species reference list

```r
#SPCD  COMMON_NAME        B0           B1

#43  = Atl.white cedar  -2.7765       2.4195                
#68  = E. redcedar      -2.6327       2.4757
#110 = shortleaf pine   -3.0506       2.6465
#111 = slash pine       -3.0506       2.6465
#115 = spruce pine      -2.6177       2.4638
#121 = longleaf pine    -3.0506       2.6465
#131 = loblolly pine    -3.0506       2.6465
#132 = Virginia pine    -3.0506       2.6465
#221 = baldcypress      -2.6327       2.4757
#222 = pondcypress      -2.6327       2.4757
#311 = Florida maple    -2.0470       2.3852
#313 = boxelder         -2.0470       2.3852
#316 = red maple        -2.0470       2.3852
#317 = silver maple     -2.0470       2.3852
#341 = ailanthus        -1.8011       2.3852
#345 = mimosa           -2.5095       2.5437
#356 = serviceberry sp. -2.2118       2.4133 
#367 = pawpaw           -2.5497       2.5011
#373 = river birch      -1.8096       2.3480
#381 = chittamwood      -2.0470       2.3852
#391 = Am. hornbeam     -2.2652       2.5349
#401 = water hickory    -2.5095       2.6175
#402 = bitternut hickory-2.5095       2.6175
#403 = pignut hickory   -2.5095       2.6175
#404 = pecan            -2.5095       2.6175
#405 = shellbark hickory-2.5095       2.6175
#407 = shagbark hickory -2.5095       2.6175
#408 = black hicory     -2.5095       2.6175
#409 = mockernut hickory-2.5095       2.6175
#413 = S. shag hick     -2.5095       2.6175
#451 = S. catalpa       -2.0314       2.3524
#461 = sugarberry       -2.2118       2.4133
#471 = eastern redbud   -2.5095       2.5437
#491 = flowering dogwood-2.2118       2.4133
#500 = hawthorn spp.    -2.2118       2.4133
#521 = common persimmon -2.2118       2.4133
#531 = Am. beech        -2.0705       2.4410
#541 = white ash        -1.8384       2.3524
#544 = green ash        -2.0314       2.3524
#552 = honeylocust      -2.5095       2.5437
#555 = loblolly-bay     -2.2118       2.4133
#571 = Kentucky coffeetree-2.5095     2.5437
#582 = two-wing silverbell-2.2118     2.4133
#591 = Am. holly        -2.2118       2.4133
#602 = black walnut     -2.5095       2.5437
#611 = sweet gum        -2.6390       2.5466
#621 = yellow-poplar    -2.5497       2.5011
#641 = osage-orange     -2.2118       2.4133
#651 = cucumber tree    -2.5497       2.5011
#652 = southern magnolia-2.5497       2.5011
#653 = sweetbay magnolia-2.5497       2.5011
#654 = bigleaf magnolia -2.5497       2.5011
#662 = southern crabapple-2.2118      2.4133
#682 = red mulberry     -2.2118       2.4133
#691 = water tupelo     -2.2118       2.4133
#693 = blackgum         -2.2118       2.4133
#694 = swamp tupelo     -2.2118       2.4133
#701 = hophornbeam      -2.2652       2.5349
#711 = sourwood         -2.2118       2.4133
#712 = paulownia        -2.0314       2.3524
#721 = redbay           -2.2118       2.4133
#722 = water-elm        -2.2118       2.4133
#731 = Am. sycamore     -2.2118       2.4133
#742 = eastern cottonwood-2.4441      2.4561
#760 = Prunus spp.      -2.2118       2.4133
#762 = black cherry     -2.2118       2.4133
#763 = chokecherry      -2.2118       2.4133
#802 = white oak        -2.0705       2.4410
#806 = scarlet oak      -2.0705       2.4410
#808 = Durand oak       -2.0705       2.4410
#812 = southern red oak -2.0705       2.4410
#813 = cherrybark oak   -2.0705       2.4410
#819 = turkey oak       -2.0705       2.4410
#820 = laurel oak       -2.2198       2.4410
#822 = overcup oak      -2.0705       2.4410
#824 = blackjack oak    -2.0705       2.4410
#825 = swamp chestnut oak-2.0705      2.4410
#826 = chinkapin oak    -2.0705       2.4410
#827 = water oak        -2.0705       2.4410
#828 = Texas red oak    -2.0705       2.4410
#831 = willow oak       -2.0705       2.4410
#832 = chestnut oak     -2.0705       2.4410
#833 = N. red oak       -2.0705       2.4410
#834 = Shumard oak      -2.0705       2.4410
#835 = post oak         -2.0705       2.4410
#837 = black oak        -2.0705       2.4410
#838 = live oak         -2.2198       2.4410
#901 = black locust     -2.5095       2.5437
#922 = black willow     -2.4441       2.4561
#929 = weeping willow   -2.4441       2.4561
#931 = sassafras        -2.2118       2.4133
#951 = Am. basswood     -2.4108       2.4177
#953 = carolina basswood-2.4108       2.4177
#971 = winged elm       -2.2118       2.4133
#972 = Am. elm          -2.2118       2.4133
#973 = cedar elm        -2.2118       2.4133
#975 = slippery elm     -2.2118       2.4133
#993 = chinaberry       -1.8011       2.3852
#994 = chinese tallow   -2.2118       2.4133
#995 = tungoil tree     -2.2118       2.4133

#998 = unknown dead hardwood
#999 = unknown live tree
```


## Merging all plots and years

```r
mer0916 <- bio0916 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer0917 <- bio0917 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer0918 <- bio0918 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1016 <- bio1016 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1018 <- bio1018 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1019 <- bio1019 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1116 <- bio1116 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1117 <- bio1117 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1118 <- bio1118 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1119 <- bio1119 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1216 <- bio1216 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1217 <- bio1217 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1218 <- bio1218 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1219 <- bio1219 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1318 <- bio1318 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1319 <- bio1319 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1418 <- bio1418 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1419 <- bio1419 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1518 <- bio1518 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')
mer1519 <- bio1519 %>% select('CNTY_PLT_SPCD','COUNTY_PLOT', 'bio_change','SPCD','CR','CCLCD','SPGRPCD')

#now for the complete merge
compdata<- mer0916 %>% bind_rows(mer0917, mer0918, mer1016, mer1018, mer1019, mer1116, mer1117, mer1118, mer1119, mer1216, mer1217, mer1218, mer1219, mer1318, mer1319, mer1418, mer1419, mer1518, mer1519)
```

## Adding descriptive columns

```r
#COND
compdata$FORTYPCD <- cond_tab$FORTYPCD[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)] 
compdata$SITECLCD <- cond_tab$SITECLCD[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$SICOND <- cond_tab$SICOND[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$STDAGE<- cond_tab$STDAGE[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$PHYSCLCD <- cond_tab$PHYSCLCD[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$OWNCD <- cond_tab$OWNCD[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$FIRE <- cond_tab$FIRE_SRS[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$GRAZING <- cond_tab$GRAZING_SRS[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]
compdata$STAND_STRUC <- cond_tab$STAND_STRUCTURE_SRS[match(compdata$COUNTY_PLOT, cond_tab$COUNTY_PLOT)]


#PLOT
compdata$LAT <- plot_tab$LAT[match(compdata$COUNTY_PLOT, plot_tab$COUNTY_PLOT)]
compdata$LON <- plot_tab$LON[match(compdata$COUNTY_PLOT, plot_tab$COUNTY_PLOT)]
compdata$ELEV<-plot_tab$ELEV[match(compdata$COUNTY_PLOT, plot_tab$COUNTY_PLOT)]
compdata$RD <- plot_tab$RDDISTCD[match(compdata$COUNTY_PLOT, plot_tab$COUNTY_PLOT)]
compdata$ECOSUBCD <- plot_tab$ECOSUBCD[match(compdata$COUNTY_PLOT, plot_tab$COUNTY_PLOT)]
```

lets caluculate species richness per plot

```r
compdata<- compdata %>% group_by(COUNTY_PLOT) %>% mutate(S = n_distinct(SPCD)) %>% ungroup()
```

lets calculate shannon's index

```r
compdata<- compdata %>% group_by(COUNTY_PLOT) %>% mutate(H = diversity(SPCD, index = "shannon")) %>% ungroup()
```

calculating mean biochange and 95% interval

```r
#making function to compute se for errorbars
se <- function(x) sqrt(var(x)/length(x))

smeandata<- group_by(compdata,S) %>% summarise(mean= mean(bio_change, na.rm = TRUE))
smeandata <- smeandata %>% mutate(upper = (mean + 1.96* se(x=mean)))
smeandata <- smeandata %>% mutate(lower = (mean - 1.96* se(x=mean)))
```


## Making diversity graphs agaist biochange!

species richness and change in biomass

```
## Warning: Removed 1884 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

shannons index and change in biomass

```
## Warning: Removed 1884 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

mean change in biomass at each species richness

```
## Warning: `fun.y` is deprecated. Use `fun` instead.
```

```
## Warning: Removed 1884 rows containing non-finite values (stat_boxplot).
```

```
## Warning: Removed 1884 rows containing non-finite values (stat_summary).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

#other graphs using biochange
continuous graphs

```r
#compacted crown ration
    ggplot(compdata, aes(x=CR, y= bio_change))+ geom_jitter()
```

```
## Warning: Removed 8070 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
    #longitude
    ggplot(compdata, aes(x=LON, y= bio_change))+ geom_point()
```

```
## Warning: Removed 1884 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-27-2.png)<!-- -->

```r
    #elevation
    ggplot(compdata, aes(x=ELEV, y= bio_change))+ geom_point()
```

```
## Warning: Removed 1884 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-27-3.png)<!-- -->

```r
    #stand age
    ggplot(compdata, aes(x=STDAGE, y= bio_change))+ geom_point()
```

```
## Warning: Removed 1884 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-27-4.png)<!-- -->

```r
    #site index
    ggplot(compdata, aes(x=SICOND, y= bio_change))+ geom_point()
```

```
## Warning: Removed 11265 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-27-5.png)<!-- -->

```r
    #distance to improved road
    ggplot(compdata, aes(x=RD, y= bio_change))+ geom_point()
```

```
## Warning: Removed 1884 rows containing missing values (geom_point).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-27-6.png)<!-- -->

categorical graphs

```
## Warning: Removed 1884 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```
## Warning: Removed 863 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-2.png)<!-- -->

```
## Warning: Removed 863 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-3.png)<!-- -->

```
## Warning: Removed 863 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-4.png)<!-- -->

```
## Warning: Removed 863 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-5.png)<!-- -->

```
## Warning: Removed 631 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-6.png)<!-- -->

```
## Warning: Removed 631 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-7.png)<!-- -->

```
## Warning: Removed 631 rows containing non-finite values (stat_boxplot).
```

![](usingbaseFIAtables_markdown_files/figure-html/unnamed-chunk-28-8.png)<!-- -->

