library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ks)

bat <- rast("./terrain_rasters/bathy.tif")

## Position of all cells with depths (i.e. the study area)
batxyc <- as_tibble(bat, xy = TRUE, cells = TRUE) %>%
  drop_na() %>%
  filter(depth >= -2500) %>%
  select(-depth) %>%
  arrange(cell)

batxy <- batxyc %>%
  select(-cell) %>%
  as.matrix()

## Get positions of VME records

nv <- read_sf("./data/NovasArc_VME_2024.gpkg") %>%
  bind_cols(st_coordinates(.)) %>%
  select(x = X,
         y = Y) %>%
  st_set_geometry(NULL)

ob <- read_sf("./data/OBIS_2024.gpkg")   %>%
  bind_cols(st_coordinates(.)) %>%
  select(x = X,
         y = Y) %>%
  st_set_geometry(NULL)

vmexy <- bind_rows(nv, ob) %>%
  distinct() %>%
  as.matrix()

## Kernel smoother

de <- kde(x = vmexy,
         H = Hpi.diag(vmexy),
         eval.points = batxy,
         compute.cont = FALSE,
         approx.cont = FALSE)

bias <- batxyc %>%
  as_tibble() %>%
  mutate(de = de$estimate - min(de$estimate),
         de = if_else(de > 1e-12, 1e-12, de),
         de = de / sum(de))

biasr <- rast(bat)
biasr[bias$cell] <- bias$de
writeRaster(biasr, "./data/bias.tif", overwrite = TRUE)

## Generate background points
set.seed(100)
bak <- sample(x = bias$cell,
             size = 51000,
             prob = bias$de,
             replace = TRUE) %>%
  unique()

## Cell numbers with VMEs. Remove duplicates (by VME code)

nv <- read_sf("./data/NovasArc_VME_2024.gpkg") %>%
  select(vme)

ob <- read_sf("./data/OBIS_2024.gpkg") %>%
  select(vme)

vme <- bind_rows(nv, ob)
vmec <- cells(bat, vect(vme)) %>%
  as_tibble() %>%
  mutate(vme = vme$vme) %>%
  select(-ID) %>%
  distinct()

## Load rasters with predictors
files_terr <- list.files("/home/julian/Documents/SDM_VMEs_Nordic/terrain_rasters", full.names = TRUE)
tnames <- list.files("/home/julian/Documents/SDM_VMEs_Nordic/terrain_rasters") %>%
  str_replace(".tif", "")
tnames[1] <- "depth"

mnames <- c("cs", "phos", "nitrate", "ammonium", "silicate", "POC_sed", "oxygen", "arag_sat", "pH", "sal", "temp")


## Present - CNRM model
files_cnnrm <- list.files("/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_hist",
                   full.names = TRUE)

## Present - GFDL model
files_gfdl <- list.files("/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_hist",
                   full.names = TRUE)

## Terrain and CNRM model in background points
env1 <- rast(c(files_terr, files_cnrm))
names(env1) <- c(tnames, mnames)
be1 <- extract(env1, bak, xy = TRUE) %>%
  mutate(occ = 0) %>%
  relocate(occ) %>%
  write_rds("./data/backgr_terrain_cnrm_present.rds")


## Terrain and GFDL model in background points
env2 <- rast(c(files_terr, files_gfdl))
names(env2) <- c(tnames, mnames)
be2 <- extract(env2, bak, xy = TRUE) %>%
  mutate(occ = 0) %>%
  relocate(occ) %>%
  write_rds("./data/backgr_terrain_gfdl_present.rds")

## Terrain and CNRM model in vme locations
ve1 <- extract(env1, vmec$cell, xy = TRUE) %>%
  mutate(vme = vmec$vme,
         occ = 1) %>%
  relocate(vme, occ) %>%
  write_rds("./data/vme_terrain_cnrm_present.rds")

## Terrain and CNRM model in vme locations
ve2 <- extract(env2, vmec$cell, xy = TRUE) %>%
  mutate(vme = vmec$vme,
         occ = 1) %>%
  relocate(vme, occ) %>%
  write_rds("./data/vme_terrain_gfdl_present.rds")
