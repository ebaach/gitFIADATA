#set working directory
#load packages
library(dplyr)
library(rFIA)
library(tidyverse)

getFIA(states='MS')

#creating databases
MS_TREEread = read.csv("MS_TREE.csv", header = T)
MS_TREE <- MS_TREEread %>% select("CN","PLT_CN","INVYR", "COUNTYCD","PLOT","CONDID","TREE","SUBP","SPCD","STATUSCD","DIA","HT","PREVDIA","DRYBIO_BOLE","DRYBIO_TOP","DRYBIO_STUMP","DRYBIO_BG","DRYBIO_AG","CARBON_AG","CARBON_BG","CYCLE","SUBCYCLE","PREVDIA_FLD","PREV_TRE_CN","CR") %>% filter(between(INVYR,2009,2019))

MS_CONDread = read.csv("MS_COND.csv", header = T)
MS_COND<- MS_CONDread %>% select("CN","PLT_CN","COUNTYCD","PLOT","CONDID","COND_STATUS_CD","FORTYPCD","FLDTYPCD","STDAGE","SITECLCD","SISP","BALIVE","FLDAGE","SITETREE_TREE","CYCLE","SUBCYCLE","STDORGCD")

MS_PLOTread = read.csv("MS_PLOT.csv", header = T)
MS_PLOT<- MS_PLOTread %>% select("CN","PREV_PLT_CN","INVYR","COUNTYCD","PLOT","MEASYEAR","REMPER","CYCLE","SUBCYCLE")%>% filter(between(INVYR,2009,2019))

TREE_GRMread = read.csv("MS_TREE_GRM_COMPONENT.csv", header=T)
TREE_GRM <- TREE_GRMread %>% select("TRE_CN","PREV_TRE_CN","PLT_CN","DIA_BEGIN","DIA_MIDPT","DIA_END","ANN_DIA_GROWTH","HT_BEGIN","HT_MIDPT","HT_END","ANN_HT_GROWTH") 

#reference tables
MS_FORTYPread= read.csv("REF_FOREST_TYPE.csv", header=T)
MS_FORTYP<- MS_FORTYPread %>% select("VALUE","MEANING","TYPGRPCD")

MS_FORTYPGRPread = read.csv("REF_FOREST_TYPE_GROUP.csv", header = T)
MS_FORTYPGRP <- MS_FORTYPGRPread %>% select("VALUE","MEANING")

SP_NAME= read.csv("REF_SPECIES.csv", header = T)

REG_BIO = read.csv("MS_TREE_REGIONAL_BIOMASS.csv", header= T)

REF_POPread = read.csv("REF_POP_ATTRIBUTE.csv", header = T)
REF_POP<- REF_POPread %>% filter(ATTRIBUTE_NBR >= 301) %>% filter(ATTRIBUTE_NBR <=322) 

#merge ref tables
MS_FOR <- MS_FORTYP %>% left_join(MS_FORTYPGRP, by= c("TYPGRPCD"="VALUE"))
names(MS_FOR)[2]<- "Forest_Type"
names(MS_FOR)[4]<- "Forest_Group_Type"

#merging database
tcp<- MS_PLOT %>% mutate(PLT_CN=CN) %>% left_join(MS_COND, by= 'PLT_CN')%>% left_join(MS_TREE, by = c('PLT_CN','CONDID'))

tcp_grm <- tcp %>% left_join(TREE_GRM, by= c("CN"="TRE_CN","PREV_TRE_CN"))

tcp_full <- tcp_grm %>% left_join(MS_FOR, by= c("FORTYPCD"="VALUE")) %>% left_join(SP_NAME, by = c("SPCD"="SPCD"))
#natural stand origin
natstand<- tcp_full %>% filter(STDORGCD==0)


#how many plots in data=144------ duplicates... how to make new labels including countycd + plot
n_distinct(natstand$PLOT)
natstand %>% count(PLOT, COUNTYCD)

#how many forest types=45
n_distinct(natstand$Forest_Type)
#how many forest type groups= 10

#more variables to figure out
complot<- natstand %>% group_by(COUNTYCD, PLOT) %>% count(COMMON_NAME)
complot$plotgrp <-group_indices(complot, COUNTYCD, PLOT)

#converting to data.table to make a summary table
as.data.table(complot)
setDT(complot)

class(complot)
summary.table(complot)

library("data.table")

cplot<- natstand %>% group_by(COUNTYCD) %>% summarise(PLOT)
pplot<- cplot %>% group_by(PLOT)
aplot<- pplot %>% count(COMMON_NAME,PLOT)

natstand %>% 
tplot<- natstand %>% mutate(plotgrp= group_indices(natstand, COUNTYCD, PLOT))
tplot %>% group_by(plotgrp) %>% summarise(n=n(COMMON_NAME))                 
?group_indices
summary.table(tplot)
n_distinct(complot$plotgrp)

#make a starter graph!

#scatterplot


#boxplot of forest type group to ann dia growth
tcp_full %>% filter(Forest_Group_Type != "NA") %>% ggplot(tcp_full, mapping= aes(x=Forest_Group_Type, y=ANN_DIA_GROWTH))+ geom_boxplot()+ ylab("Annual Change in Diameter")+xlab("Forest Type")+ggtitle("Distribution of Annual Diameter Change in Various Forest Types")+ theme(axis.text.x = element_text(angle=45, hjust=1),plot.title = element_text(hjust = 0.5))+stat_summary(fun.y = "mean",color="cornflowerblue",size=.5)

#plot of forest type group to ann ht growth
tcp_full %>% filter(Forest_Group_Type!="NA") %>% ggplot(tcp_full, mapping= aes(x=Forest_Group_Type, y=ANN_HT_GROWTH))+ geom_boxplot()+ ylab("Annual Change in Height")+xlab("Forest Type")+ggtitle("Distribution of Annual Height Change in Various Forest Types")+ theme(plot.title = element_text(hjust = 0.5))+ coord_flip()+stat_summary(fun.y = "mean",color="firebrick",size=.5)

#forest type vs ann dia growth
tcp_full %>% filter(Forest_Type != "NA") %>% ggplot(tcp_full, mapping= aes(x=Forest_Type, y=ANN_DIA_GROWTH))+ geom_boxplot()+ ylab("Annual Change in Diameter")+xlab("Forest Type")+ggtitle("Distribution of Annual Diameter Change in Various Forest Types")+ theme(axis.text.x = element_text(angle=45, hjust=1),plot.title = element_text(hjust = 0.5))+ stat_summary(fun.y = "mean",color="cornflowerblue",size=.1)

#forest type vs ann ht growth
tcp_full %>% filter(Forest_Type!="NA") %>% ggplot(tcp_full, mapping= aes(x=Forest_Type, y=ANN_HT_GROWTH))+ geom_boxplot(outlier.size=.001)+ ylab("Annual Change in Height")+xlab("Forest Type")+ggtitle("Distribution of Annual Height Change in Various Forest Types")+ theme(axis.text.x=element_text(angle = 45,hjust = 1), plot.title = element_text(hjust = 0.5))+ stat_summary(fun.y = "mean",color="green", size= .01)+ coord_flip()

#removing a x column----> if needed later
treecondplot_xminus <- treecondplot[-c(1)]

#FILTERING DATA:
#only live trees(?)----> should be changed to only live in 2019? used at all?
livtredata<- tcp_full %>% filter(STATUSCD==1) 



###SPECIFICATIONS USED:
#only MS info--> applied
#years from 2009 to 2019---> applied
#only live trees--> applied
  #STATUSCD==1
#only natural stand origin--> applied
  #STDORGCD==0



##Codes for future sorting:
#major group codes
  #pines=1
  #other softwoods =2
  #soft hardwoods = 3
  #hard hardwoods =4
#forest type codes
  #elm/ash/cottonwood= 700
  #exotic hardwoods= 990
  #loblolly/shortleaf pine= 160
  #longleaf/slash pine =140
  #nonstocked= 999
  #oak/gum/cypress= 600
  #oak/hickory=500
  #other eastern softwoods= 170
  #other hardwoods= 960
  #spruce/fir= 120
