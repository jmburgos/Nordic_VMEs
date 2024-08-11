library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(MultiscaleDTM)

novasarc_proj <-  "+proj=laea +lat_0=69 +lon_0=-4 +datum=WGS84"

sa <- read_sf("./data/study_area.gpkg")

target <- rast(ext(sa %>% st_transform(novasarc_proj)),
               res = 500, crs = novasarc_proj)

bat <- rast("/home/julian/Documents/Mapping/Bathymetry/GEBCO/gebco_2023_n83.0_s56.0_w-45.0_e38.0.tif") %>%
  rename("depth" = "gebco_2023_n83.0_s56.0_w-45.0_e38.0") %>%
  clamp(upper = 0, values = FALSE) %>%
  mask(sa) %>%
  project(target) %>%
  writeRaster("./terrain_rasters/bathy.tif", overwrite = TRUE)

# Slope, Aspect and Curvature

bat <- rast("./terrain_rasters/bathy.tif")

Sys.time()
slp_asp <- SlpAsp(r = bat, w = c(3, 3), unit = "degrees", method = "queen",
                  metrics = c("slope",  "eastness", "northness"),
                  filename = "./terrain_rasters/slo_asp3.tif")
Sys.time()

slp_asp <- rast("./terrain_rasters/slo_asp3.tif")

writeRaster(slp_asp[[1]], filename = "./terrain_rasters/slope3.tif", overwrite = TRUE)
writeRaster(slp_asp[[2]], filename = "./terrain_rasters/seast3.tif", overwrite = TRUE)
writeRaster(slp_asp[[3]], filename = "./terrain_rasters/north3.tif", overwrite = TRUE)
file.remove("./terrain_rasters/slo_asp3.tif")

tmpFiles(current = TRUE, remove = TRUE)

Sys.time()
slp_asp<- SlpAsp(r = bat, w = c(21, 21), unit = "degrees", method = "queen",
                 metrics = c("slope",  "eastness", "northness"),
                 filename = "./terrain_rasters/slo_asp21.tif")
Sys.time()

slp_asp <- rast("./terrain_rasters/slo_asp21.tif")

writeRaster(slp_asp[[1]], filename = "./terrain_rasters/slope21.tif")
writeRaster(slp_asp[[2]], filename = "./terrain_rasters/east21.tif")
writeRaster(slp_asp[[3]], filename = "./terrain_rasters/north21.tif")

file.remove("./terrain_rasters/slo_asp21.tif")


# Vector ruggedness measurei
vrm <- VRM(bat, w = c(3, 3), na.rm = TRUE,
           filename = "./terrain_rasters/vrm3.tif",
           overwrite = TRUE)

vrm <- VRM(bat, w = c(21, 21), na.rm = TRUE,
           filename = "./terrain_rasters/vrm21.tif",
           overwrite = TRUE)

tmpFiles(current = TRUE, remove = TRUE)

tpi5 <- TPI(bat, w = 3, shape= "circle", na.rm = TRUE, filename = "./terrain_rasters/tpi3.tif")

tpi21 <- TPI(bat, w = 21, shape= "circle", na.rm = TRUE, filename = "./terrain_rasters/tpi21.tif")



# Curvature
Sys.time()
qmetrics <- Qfit(bat, w = c(3,3), unit = "degrees", metrics = c("profc", "planc"), na.rm = TRUE,
                filename = "./terrain_rasters/curv3.tif")
tmpFiles(current = TRUE, remove = TRUE)
Sys.time()

qmetrics <- rast("./terrain_rasters/curv3.tif")
writeRaster(qmetrics[[1]], filename = "./terrain_rasters/profc3.tif")
writeRaster(qmetrics[[2]], filename = "./terrain_rasters/planc3.tif")
file.remove("./terrain_rasters/curv3.tif")

Sys.time()
qmetrics <- Qfit(bat, w = c(21, 21), unit = "degrees", metrics = c("profc", "planc"), na.rm = TRUE,
                 filename = "./terrain_rasters/curv21.tif")
tmpFiles(current = TRUE, remove = TRUE)
Sys.time()

qmetrics <- rast("./terrain_rasters/curv21.tif")
writeRaster(qmetrics[[1]], filename = "./terrain_rasters/profc21.tif")
writeRaster(qmetrics[[2]], filename = "./terrain_rasters/planc21.tif")
file.remove("./terrain_rasters/curv21.tif")
