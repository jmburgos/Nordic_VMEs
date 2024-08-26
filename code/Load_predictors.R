library(tidyverse)
library(terra)
library(tidyterra)

## -----------------------------------------------
## Load rasters with predictors
## Six raster stacks: two models (CNRM,GFDL), present, future under SSP1.26,
## and future under SSP3.70

tfiles <- list.files("./terrain_rasters", full.names = TRUE)
tnames <- list.files("./terrain_rasters") %>%
  str_replace(".tif", "")
tnames[1] <- "depth"

mfiles_cnrm <- list.files("./climate_rasters/CNRM_hist", full.names = TRUE)
mfiles_cnrm_126 <- list.files("./climate_rasters/CNRM_ssp126", full.names = TRUE)
mfiles_cnrm_370 <- list.files("./climate_rasters/CNRM_ssp370", full.names = TRUE)

mfiles_gfdl <- list.files("./climate_rasters/GFDL_hist", full.names = TRUE)
mfiles_gfdl_126 <- list.files("./climate_rasters/GFDL_ssp126", full.names = TRUE)
mfiles_gfdl_370 <- list.files("./climate_rasters/GFDL_ssp370", full.names = TRUE)

mnames <- c("cs", "phos", "nitrate", "ammonium", "silicate", "POC_sed", "oxygen", "arag_sat", "pH", "sal", "temp")

make_stack <- function(tfiles, mfiles) {
  st <- rast(c(tfiles, mfiles))
  names(st) <-  c(tnames, mnames)
  return(st)
}

env_cnrm <- make_stack(tfiles, mfiles_cnrm)
env_cnrm_126 <- make_stack(tfiles, mfiles_cnrm_126)
env_cnrm_370 <- make_stack(tfiles, mfiles_cnrm_370)

env_gfdl <- make_stack(tfiles, mfiles_gfdl)
env_gfdl_126 <- make_stack(tfiles, mfiles_gfdl_126)
env_gfdl_370 <- make_stack(tfiles, mfiles_gfdl_370)
