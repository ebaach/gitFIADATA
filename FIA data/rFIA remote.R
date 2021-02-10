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

#by plot
bioplt<- biomass(MS, byPlot = TRUE)
n_distinct(bioplt$BIO_ACRE)

#diversity by plot
div_plt<- diversity(MS, byPlot = TRUE)
n_distinct(div_plt$H)


#grouping by plt origin code
originbio<- biomass(MS, byPlot = TRUE, grpBy = c("STDORGCD")) %>% filter(STDORGCD ==0)
origindiv<- diversity(MS, byPlot = TRUE, grpBy = "STDORGCD")%>% filter(STDORGCD ==0)
originvit<- vitalRates(MS, byPlot = TRUE, grpBy = "STDORGCD")%>% filter(STDORGCD ==0)

#growth rates
vits<- vitalRates(MS, byPlot = TRUE)

n_distinct(vits$BIO_GROW_AC)
#merge bioplt and divplt
div_bio<- div_plt %>% left_join(bioplt, by =c("pltID"="pltID","YEAR"="YEAR","PLOT_STATUS"="PLOT_STATUS","PLOT_STATUS_CD"="PLOT_STATUS_CD","nStems"="nStems","PLT_CN"="PLT_CN"))

div_bio<- div_bio %>% filter(PLOT_STATUS=="Forest")
n_distinct(bioplt$S)

bioplt$H<- div_plt$H[match(bioplt$pltID,div_plt$pltID)]

bioplt$S <- div_plt$S[match(bioplt$pltID, div_plt$pltID)]
bioplt$STDORGCD<- originbio$STDORGCD[match(bioplt$pltID, originbio$pltID)]

divbiovital<- bioplt %>% left_join(vits, by = c("pltID"="pltID"))

n_distinct(divbiovital$BIO_ACRE)

## Alternative estimators (annual panels)
yrbio<-biomass(MS, method = 'ANNUAL')
#plot of biomass/acre/yr
ggplot(yrbio, aes(x= yrbio$YEAR, y= yrbio$BIO_ACRE))+ geom_line()

       