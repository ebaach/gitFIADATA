#first lets load our packages
library(rFIA)
library(dplyr)
library(tidyverse)

#will this pathway work better?
getFIA(c('MS'), dir = 'C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', load = FALSE)

MS <- readFIA('C:/Users/elbaa/OneDrive/Desktop/gitMSDATA', inMemory = FALSE)


#from tips on working with big data
#must first define db at fia 
biomass(db = MS)

## By species
biosp<-biomass(MS, bySpecies = TRUE)

## Alternative estimators (annual panels)
yrbio<-biomass(MS, method = 'ANNUAL')
#plot of biomass/acre/yr
ggplot(yrbio, aes(x= yrbio$YEAR, y= yrbio$BIO_ACRE))+ geom_line()

       