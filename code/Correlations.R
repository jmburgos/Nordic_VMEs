library(tidyverse)
library(terra)
library(tidyterra)
library(corrplot)

## Correlation among terrain variables
cr <- read_rds("./data/backgr_terrain_cnrm_present.rds") %>%
  drop_na() %>%
  select(depth:vrm3) %>%
  cor()

testRes = cor.mtest(cr, conf.level = 0.95)
corrplot(cr, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

## Higly correlated (>.8)
## tpi3 and profc3

## Correlation among environmental variables
cr <- read_rds("./data/backgr_terrain_cnrm_present.rds") %>%
  drop_na() %>%
  select(cs:temp) %>%
  cor()


testRes = cor.mtest(cr, conf.level = 0.95)
corrplot(cr, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

## Higly correlated (>.8)
# phosphate - nitrate
# phosphate - silicate
# nitrate - silicate
# ammonium - POC_sed
