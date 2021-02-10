#set working directory
setwd("C:/Users/elbaa/OneDrive/Desktop/MS DATA")

#load libraries
library(rFIA)
library(dplyr)
library(tidyverse)
#download database (fiaMS)

if(file.exists("fia_ms.rdata")){
  fiaMS <- read_rds("fia_ms.rdata")
} else {
  fiaMS <- getFIA(states = 'MS')
  write_rds(fiaMS, file = "fia_ms.rdata")
}



#identifying and subsetting necessary data
db<-fiaMS

#rm(fiaMS)

ids <- db$POP_EVAL %>%
  select('CN', 'END_INVYR', 'EVALID') %>%
  inner_join(select(db$POP_EVAL_TYP, c('EVAL_CN', 'EVAL_TYP')), by = c('CN' = 'EVAL_CN')) %>%
  ## Now we filter out everything except current area and 
  ## current volume ids
  filter(EVAL_TYP %in% c('EXPCURR', 'EXPVOL'))

## Now that we have those EVALIDs, let's use clipFIA to subset
db <- clipFIA(db, evalid = ids$EVALID)

## Select only the columns we need from each table
PLOT <- select(db$PLOT, CN, MACRO_BREAKPOINT_DIA, COUNTYCD, INVYR) %>% filter(between(INVYR,2009,2019))
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD, FORTYPCD, BALIVE, STDORGCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD,DRYBIO_BOLE,DRYBIO_TOP,DRYBIO_STUMP,DRYBIO_BG,SPGRPCD)
POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)
MS_FORTYP = read.csv("REF_FOREST_TYPE.csv", header=T) %>% select("VALUE","MEANING","TYPGRPCD")
MS_FORTYPGRP = read.csv("REF_FOREST_TYPE_GROUP.csv", header = T) %>% select("VALUE","MEANING")
SP_NAME= read.csv("REF_SPECIES.csv", header = T)
SP_GROUP = read.csv("REF_SPECIES_GROUP.csv", header = T)



#premerge of ref tables
MS_FOR <- MS_FORTYP %>% left_join(MS_FORTYPGRP, by= c("TYPGRPCD"="VALUE"))
names(MS_FOR)[2]<- "Forest_Type"
names(MS_FOR)[4]<- "Forest_Group_Type"



#merging 
data <- PLOT %>%
  ## Add a PLT_CN column for easy joining
  mutate(PLT_CN = CN) %>%
  ## Join COND & TREE
  left_join(COND, by = 'PLT_CN') %>%
  left_join(TREE, by = c('PLT_CN', 'CONDID')) %>%
  ## Population tables
  left_join(POP_PLOT_STRATUM_ASSGN, by = 'PLT_CN') %>%
  left_join(POP_STRATUM, by = c('STRATUM_CN' = 'CN')) %>%
  left_join(POP_ESTN_UNIT, by = c('ESTN_UNIT_CN' = 'CN')) %>%
  left_join(POP_EVAL, by = c('EVAL_CN' = 'CN')) %>%
  left_join(POP_EVAL_TYP, by = 'EVAL_CN') %>% 
  left_join(MS_FOR, by = c("FORTYPCD"="VALUE")) %>% 
  left_join(SP_NAME, by = c("SPCD"="SPCD")) %>% 
  left_join(SP_GROUP, by = c("SPGRPCD" = "SPGRPCD"))

#adjusting for nonadjustment factors
data <- data %>%
mutate(
  ## AREA
  aAdj = case_when(
    ## When NA, stay NA
    is.na(PROP_BASIS) ~ NA_real_,
    ## If the proportion was measured for a macroplot,
    ## use the macroplot value
    PROP_BASIS == 'MACR' ~ as.numeric(ADJ_FACTOR_MACR),
    ## Otherwise, use the subpplot value
    PROP_BASIS == 'SUBP' ~ ADJ_FACTOR_SUBP),
  ## TREE
  tAdj = case_when(
    ## When DIA is na, adjustment is NA
    is.na(DIA) ~ ADJ_FACTOR_SUBP,
    ## When DIA is less than 5", use microplot value
    DIA < 5 ~ ADJ_FACTOR_MICR,
    ## When DIA is greater than 5", use subplot value
    DIA >= 5 ~ ADJ_FACTOR_SUBP
  ))

#changing year name and removing nas
data <- data %>%
  mutate(YEAR = END_INVYR) %>%
  ## remove any NAs
  filter(!is.na(YEAR))

#getting rid of unnecessary columns in data before merging further
data<- data %>% select(-c(VARIETY, SUBSPECIES,E_SPGRPCD, W_SPGRPCD, C_SPGRPCD,P_SPGRPCD, EXISTS_IN_NCRS, EXISTS_IN_NERS, EXISTS_IN_PNWRS, EXISTS_IN_RMRS,EXISTS_IN_SRS, SITETREE, ST_EXISTS_IN_NCRS, ST_EXISTS_IN_NERS, ST_EXISTS_IN_PNWRS, ST_EXISTS_IN_RMRS, ST_EXISTS_IN_SRS, CORE, EAST, WEST, CARIBBEAN, PACIFIC, WOODLAND, CREATED_BY.x, CREATED_BY.y, CREATED_DATE.x, CREATED_IN_INSTANCE.x, CREATED_DATE.y, CREATED_IN_INSTANCE.y, MODIFIED_BY.x, MODIFIED_DATE.x, MODIFIED_IN_INSTANCE.x, MODIFIED_BY.y, MODIFIED_DATE.y, MODIFIED_IN_INSTANCE.y ))
                       

#diversity by plot
divplot<-diversity(db,byPlot = TRUE, totals = TRUE)
#biomass by plot
bioplot <- biomass(db, byPlot = TRUE, landType = "forest", bySpecies = TRUE)

#growth estimates---> not working (figure out why)
vitals<- vitalRates(fiaMS, landType = 'forest',  byPlot = TRUE, bySpecies = TRUE)
vitals<- vitals %>% filter(PLOT_STATUS== 'Forest')


#combinging biomass with diversity(By plot)
div_biomass<- divplot %>% left_join(bioplot, by = c("pltID"="pltID"))
#removing columns 
div_biomass<- div_biomass[-c(1,3,4,5)]
div_biomass<- div_biomass[-c(5)]
div_biomass<- mutate(div_biomass, PLT_CN= PLT_CN.y)
div_biomass<- mutate(div_biomass, YEAR= YEAR.y)
div_biomass<- mutate(div_biomass, PLOT_STATUS_CD= PLOT_STATUS_CD.y)
div_biomass<- mutate(div_biomass, PLOT_STATUS= PLOT_STATUS.y)
div_biomass<- mutate(div_biomass, nStems= nStems.y)
div_biomass<- div_biomass[-c(5,6,7,8,10)]
div_biomass<- div_biomass[-c(5,6,8)]
#merging with vitals
div_bio_vit <- div_biomass %>% left_join(vitals, by = c("pltID"="pltID"))
#cleaning up before merge with data
div_bio_vit<- div_bio_vit[-c(12,16,17,18,27)]
div_bio_vit<- mutate(div_bio_vit, PLT_CN = PLT_CN.x)
div_bio_vit<- mutate(div_bio_vit, YEAR= YEAR.x)
div_bio_vit<- mutate(div_bio_vit, PLOT_STATUS_CD= PLOT_STATUS_CD.x)
div_bio_vit<- mutate(div_bio_vit, PLOT_STATUS= PLOT_STATUS.x)
div_bio_vit<- mutate(div_bio_vit, nStems= nStems.x)
div_bio_vit<- div_bio_vit[-c(7,8,9,10,11)]

#adding biomass, diversity and vitals to whole data---> cannot allocate vector
totdata<- data %>% left_join(div_bio_vit, by = c("PLT_CN"="PLT_CN","YEAR"="YEAR","COMMON_NAME"="COMMON_NAME","SPCD"="SPCD"))


#filtering for only forests and only relevant forest types
totdata<- totdata %>% filter(PLOT_STATUS.x=="Forest") %>% filter(Forest_Group_Type != c("NA","Exotic hardwoods group","Nonstocked"))
totdata<- totdata %>% filter(STDORGCD=="0")


#making graphs using total data!! 
pidmaxS<- totdata %>% group_by(pltID, YEAR) %>% summarise(species_richness = max(S), BIO_ACRE)

ggplot(pidmaxS, aes(x=pidmaxS$pltID, y= species_richness, color= pidmaxS$YEAR ))+ geom_point()

#ok thats a lot of points----> try seeing one county (oktibbeha)

test<-totdata %>% filter(COUNTYCD== "105")
ggplot(test, aes( x= BIO_ACRE, y= H, color= Forest_Type))+ geom_point()
ggplot(test, aes( x= pltID, y= BIO_ACRE, color= Forest_Type))+ geom_point()

#seeing if vitals has nice change in biomass-- oktibbeha for clarity 
vittest<- test %>% group_by(pltID, YEAR) %>% summarise(biochange= BIO_GROW)
vittest<- vittest %>% filter(biochange>0)

ggplot(vittest, aes(x= pltID, y= biochange, color= YEAR))+ geom_point()

singplot<- vittest %>% filter(pltID== "2_28_105_19")

#testing by plot
newtestf<- totdata %>% group_by(pltID,YEAR) %>% summarise(changebiomass= sum(DRYBIO_AG))
newtestf<- newtestf %>% filter(biomass>0)
spdtest<- spread(newtestf, YEAR, biomass, fill = NA)

spdtest$mct <- rowSums(is.na(spdtest))
spdtest<- spdtest %>% filter(mct<10)

#comparing sums of 1 plot to see why decreasing
pltcomp<- totdata %>% filter(pltID== "2_28_105_19")
pltcomp_10 <-pltcomp %>%  filter(YEAR ==2010)
sum(pltcomp_10$DRYBIO_AG)
#114451.4
pltcomp_18 <- pltcomp %>% filter(YEAR== 2018)
sum(pltcomp_18$DRYBIO_AG)
#82701.77
pltcompsumm<- pltcomp %>%  group_by(pltID, YEAR) %>% summarise(BIO_GROW_AC,sum(DRYBIO_AG))


#testing by CN
exttest<- totdata %>% group_by(CN, YEAR) %>% summarise(diameter = DIA)
exttest<- exttest %>% filter(diameter>0)

exttest<- spread(exttest, YEAR, diameter, fill=NA)
exttest$mct<- rowSums(is.na(exttest))

exttest<- exttest %>% filter(mct<10)

?melt
?cst

#scatterplot of Species richness in each plot---- very confusing
ggplot(div_biomass, aes(pltID, S))+ geom_point()

#using rFIA function
plotFIA(div_biomass, y= S,x= pltID)
