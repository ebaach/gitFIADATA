#following rFIA
install.packages("rFIA")
library(rFIA)
library(dplyr)
data("fiaRI")
db<- fiaRI()


ids<- db$POP_EVAL %>% select('CN', 'END_INVYR', 'EVALID') %>%
  inner_join(select(db$POP_EVAL_TYP, c('EVAL_CN', 'EVAL_TYP')), by = c('CN' = 'EVAL_CN')) %>% filter(EVAL_TYP %in% c('EXPCURR', 'EXPVOL'))
db<- clipFIA(db, evalid = ids$EVALID)

PLOT <- select(db$PLOT, CN, MACRO_BREAKPOINT_DIA)
COND <- select(db$COND, PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNGRPCD)
TREE <- select(db$TREE, PLT_CN, CONDID, SUBP, TREE, STATUSCD, DRYBIO_AG, CARBON_AG, TPA_UNADJ, DIA, SPCD)
POP_ESTN_UNIT <- select(db$POP_ESTN_UNIT, CN, EVAL_CN, AREA_USED, P1PNTCNT_EU)
POP_EVAL <- select(db$POP_EVAL, EVALID, EVAL_GRP_CN, ESTN_METHOD, CN, END_INVYR, REPORT_YEAR_NM)
POP_EVAL_TYP <- select(db$POP_EVAL_TYP, EVAL_TYP, EVAL_CN)
POP_PLOT_STRATUM_ASSGN <- select(db$POP_PLOT_STRATUM_ASSGN, STRATUM_CN, PLT_CN)
POP_STRATUM <- select(db$POP_STRATUM, ESTN_UNIT_CN, EXPNS, P2POINTCNT, 
                      ADJ_FACTOR_MICR, ADJ_FACTOR_SUBP, ADJ_FACTOR_MACR, CN, P1POINTCNT)

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
  left_join(POP_EVAL_TYP, by = 'EVAL_CN')

## Make some adjustment factors
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

## Land type (all forested area)
data$aDI <- if_else(data$COND_STATUS_CD == 1, 1, 0)
## Live trees only (on forested area)
data$tDI <- if_else(data$STATUSCD == 1, 1, 0) * data$aDI

## Now, let's just rename the END_INVYR column to 'YEAR'
## for a pretty output like rFIA
data <- data %>%
  mutate(YEAR = END_INVYR) %>%
  ## remove any NAs
  filter(!is.na(YEAR))

## Estimate Tree totals
tre_bio <- data %>%
  filter(EVAL_TYP == 'EXPVOL') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, SUBP, TREE, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(YEAR, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(bioPlot = sum(DRYBIO_AG * TPA_UNADJ * tAdj * tDI * EXPNS  / 2000, na.rm = TRUE),
            carbPlot = sum(CARBON_AG * TPA_UNADJ * tAdj * tDI * EXPNS  / 2000, na.rm = TRUE)) %>%
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(YEAR) %>%
  summarize(BIO_AG_TOTAL = sum(bioPlot, na.rm = TRUE),
            CARB_AG_TOTAL = sum(carbPlot, na.rm = TRUE))

## Estimate Area totals
area_bio <- data %>%
  filter(EVAL_TYP == 'EXPCURR') %>%
  ## Make sure we only have unique observations of plots, trees, etc.
  distinct(ESTN_UNIT_CN, STRATUM_CN, PLT_CN, CONDID, .keep_all = TRUE) %>%
  ## Plot-level estimates first (multiplying by EXPNS here)
  group_by(YEAR, ESTN_UNIT_CN, ESTN_METHOD, STRATUM_CN, PLT_CN) %>%
  summarize(forArea = sum(CONDPROP_UNADJ * aAdj * aDI * EXPNS, na.rm = TRUE)) %>%
  ## Now we simply sum the values of each plot (expanded w/ EXPNS)
  ## to obtain population totals
  group_by(YEAR) %>%
  summarize(AREA_TOTAL = sum(forArea, na.rm = TRUE))

bio <- left_join(tre_bio, area_bio) %>%
  mutate(BIO_AG_ACRE = BIO_AG_TOTAL / AREA_TOTAL,
         CARB_AG_ACRE = CARB_AG_TOTAL / AREA_TOTAL) %>%
  ## Reordering the columns
  select(YEAR, BIO_AG_ACRE, CARB_AG_ACRE, BIO_AG_TOTAL, CARB_AG_TOTAL, AREA_TOTAL)

biomass(clipFIA(fiaRI), totals = TRUE)
bio
