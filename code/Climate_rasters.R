library(tidyverse)
library(tidync)
library(stars)
library(stringr)
library(terra)
library(tidyterra)

bat <- rast("./terrain_rasters/bathy.tif")

# 24 years
# Monthly means (1990-2014 = 300 months)

make.raster <- function(file.in, file.out){

  dt <- tidync(file.in) %>%
    hyper_filter(
      y = index > 550,
      x = index > 400 & index < 900) %>%
    hyper_tibble() %>%
    mutate(lat = y / 4 - 89.875,
           lon = x / 4 -179.875,
           date = as_datetime(time, origin = "1900-01-01")) %>%
    filter(lon >= -45,
           lon <= 56,
           lat >= 38,
           lat <= 83,
           date >= ymd("1999-01-01"))

  names(dt)[1] = "var"

  dt <- dt %>%
    group_by(lon, lat) %>%
    summarise(var = mean(var), .groups = "drop") %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    rasterize(rast(ext(.), nrows = 141, ncols = 310),
              field = "var", fun = mean) %>%
    project(bat) %>%
    mask(bat) %>%
    writeRaster(file.out, overwrite = TRUE)

}

## CNRM hist

path.in <-"/home/julian/Documents/Climate_predictions/NEMO-ERSEM/CNRM_hist/bottom/"
path.out <-"/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_hist/"

files <- list.files(path.in, pattern = ".nc")
names <- str_replace(files, ".nc", "")
files.in <- str_c(path.in, files)
files.out <- str_c(path.out, names, ".tif") %>%
    str_replace("1990-2014", "1999-2014")


for (i in 1:12){
  make.raster(file.in = files.in[i],
              file.out = files.out[i])
}

r1 <- rast(files.out[11])
r2 <- rast(files.out[12])
r <- sqrt(r1^2 + r2^2)
writeRaster(r, str_c(path.out, "CNRM_hist_bottom_cs_1999-2014.tif"),
                     overwrite = TRUE)
file.remove(files.out[11])
file.remove(files.out[12])
tmpFiles(current = TRUE, remove = TRUE)

## GFDL_hist

path.in <- "/home/julian/Documents/Climate_predictions/NEMO-ERSEM/GFDL_hist/bottom/"
path.out <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_hist/"

files <- list.files(path.in, pattern = ".nc")
names <- str_replace(files, ".nc", "")
files.in <- str_c(path.in, files)
files.out <- str_c(path.out, names, ".tif") %>%
    str_replace("1990-2014", "1999-2014")

for (i in 1:12){
  make.raster(file.in = files.in[i],
              file.out = files.out[i])
}

r1 <- rast(files.out[11])
r2 <- rast(files.out[12])
r <- sqrt(r1^2 + r2^2)

writeRaster(r, str_c(path.out, "GFDL_hist_bottom_cs_1999-2014.tif"),
            overwrite = TRUE)
file.remove(files.out[11])
file.remove(files.out[12])
tmpFiles(current = TRUE, remove = TRUE)

############################################################################

make.rasters <- function(path.in, path.out) {

  files <- list.files (path.in, pattern = ".nc")
  names <- str_replace (files, ".nc", "") %>%
    str_replace ("2015-2070", "2055-2070")

  files.in <- str_c(path.in, files)
  files.out <- str_c(path.out, names, ".tif")

  ifelse(!dir.exists(path.out), dir.create(path.out), FALSE)

  for (i in 1:length(files)){

    dt <- tidync(files.in[i]) %>%
      hyper_filter(
        y = index > 550,
        x = index > 400 & index < 900) %>%
      hyper_tibble() %>%
      mutate(lat = y / 4 - 89.875,
             lon = x / 4 -179.875,
             date = as_datetime(time, origin = "1900-01-01")) %>%
       filter(lon >= -45,
             lon <= 56,
             lat >= 38,
             lat <= 83) %>%
      filter(date >= dmy("01-01-2055"))

    names(dt)[1] = "var"

    dt <- dt %>%
      group_by(lon, lat) %>%
      summarise(var = mean(var), .groups = "drop") %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      rasterize(rast(ext(.), nrows = 141, ncols = 310),
                field = "var", fun = mean) %>%
      project(bat) %>%
      mask(bat) %>%
      writeRaster(files.out[i], overwrite = TRUE)
    tmpFiles(current = TRUE, remove = TRUE)
  }

}

make.rasters(path.in = "/home/julian/Documents/Climate_predictions/NEMO-ERSEM/CNRM_ssp126/bottom/",
             path.out = "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_ssp126/")


f1 <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_ssp126/CNRM_ssp126_bottom_uo_2055-2070.tif"
f2 <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_ssp126/CNRM_ssp126_bottom_vo_2055-2070.tif"
r <- sqrt(rast(f1)^2 + rast(f2)^2)

writeRaster(r, "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_ssp126/CNRM_ssp126_bottom_cs_2055-2070.tif", overwrite = TRUE)

file.remove(f1)
file.remove(f2)
tmpFiles(current = TRUE, remove = TRUE)

#-------------------------------------------------------------------------

make.rasters(path.in = "/home/julian/Documents/Climate_predictions/NEMO-ERSEM/CNRM_ssp370/bottom/",
             path.out = "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/CNRM_ssp370/")

# Current speed data missing in CDF.  Assume same as in ssp126

#-------------------------------------------------------------------------


make.rasters(path.in = "/home/julian/Documents/Climate_predictions/NEMO-ERSEM/GFDL_ssp126/bottom/",
             path.out = "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp126/")

f1 <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp126/GFDL_ssp126_bottom_uo_2055-2070.tif"
f2 <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp126/GFDL_ssp126_bottom_vo_2055-2070.tif"

r <- sqrt(rast(f1)^2 + rast(f2)^2)

writeRaster(r, "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp126/GFDL_ssp126_bottom_cs_2055-2070.tif", overwrite = TRUE)

file.remove(f1)
file.remove(f2)
tmpFiles(current = TRUE, remove = TRUE)

#-------------------------------------------------------------------------
make.rasters(path.in = "/home/julian/Documents/Climate_predictions/NEMO-ERSEM/GFDL_ssp370/bottom/",
             path.out = "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp370/")

f1 <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp370/GFDL_ssp370_bottom_uo_2055-2070.tif"
f2 <- "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp370/GFDL_ssp370_bottom_vo_2055-2070.tif"

r <- sqrt(rast(f1)^2 + rast(f2)^2)


writeRaster(r, "/home/julian/Documents/SDM_VMEs_Nordic/climate_rasters/GFDL_ssp370/GFDL_ssp370_bottom_cs_2055-2070.tif", overwrite = TRUE)

file.remove(f1)
file.remove(f2)
tmpFiles(current = TRUE, remove = TRUE)
