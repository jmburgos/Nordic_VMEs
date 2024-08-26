library(sf)
library(dplyr)
library("terra")
library("dismo")
library("rJava")
library(MultiscaleDTM) # for bathy derived vars
library(ggplot2)
library(viridis)
library(rworldmap)
library(tidyterra)
library(stringr)
library(ks)
library(tidyverse)
library(corrplot)
library(ENMeval )
library(tictoc)

VME_i<-c("4", "4a")
#Load land massas
worldMap <- getMap(resolution = "high")[which(getMap(resolution = "high")$ADMIN%in%c("Norway", "Russia", "Sweden", "Finland", "United Kingdom", "Iceland", "Greenland", "Faroe Islands", "Ireland")),]
clipper_NA <- as(extent(-44.875,  38, 56, 82.875), "SpatialPolygons")
proj4string(clipper_NA) <- CRS(proj4string(worldMap))
world_clip <- raster::intersect(worldMap, clipper_NA)
worldMap <- spTransform(world_clip, sp::CRS("+proj=laea +lat_0=69 +lon_0=-4 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" ))
ggworldMap<-st_as_sf(worldMap)

# Load data
### Environmental data
# NEMO:
NEMO_asNA<-rast("C:/Users/a39495/Documents/Projects/NEMO/NEMO_asNA.tif")     

# Topography
files1 <- list.files("C:/Users/a39495/Documents/Projects/NEMO/terrain_rasters", full.names = TRUE)
tnames <- list.files("C:/Users/a39495/Documents/Projects/NEMO/terrain_rasters") %>%
  str_replace(".tif", "")
tnames[1] <- "depth"

# MA Climate
mnames <- c("cs", "phos", "nitrate", "ammonium", "silicate", "POC_sed", "oxygen", "arag_sat", "pH", "sal", "temp")
## Present - CNRM model
files2 <- list.files("C:/Users/a39495/Documents/Projects/NEMO/climate_rasters/CNRM_hist",
                     full.names = TRUE)
## Present - GFDL model
files3 <- list.files("C:/Users/a39495/Documents/Projects/NEMO/climate_rasters/GFDL_hist",
                     full.names = TRUE)

#JOIN ALL
## Terrain and CNRM model in background points
env1 <- rast(c(files1,#Terrain 
               files2 #CNRM model
))
names(env1) <- c(tnames, mnames)

add(env1)<-NEMO_asNA[[c('salt_scen', 'SDsalt_scen', 'temper_scen', 'SDtemper_scen', 'c_spd_scen', 'SDc_spd_scen')]]


#Background points:
#Create raster with probability to be included as background
bat <- rast("C:/Users/a39495/Documents/Projects/Mission_Atlantic/Mission_Atlantic/terrain_rasters/bathy.tif")

## Position of all cells with depths (i.e. the study area)
batxyc <- as_tibble(bat, xy = TRUE, cells = TRUE) %>%
  drop_na() %>%
  filter(depth >= -2500) %>%
  select(-depth) %>%
  arrange(cell)

batxy <- batxyc %>%
  select(-cell) %>%
  as.matrix()

## Positions of VME records
nv <- read_sf("C:/Users/a39495/Documents/Projects/Mission_Atlantic/Mission_Atlantic/NovasArc_VME_2024.gpkg") %>%
  dplyr::filter(vme %in% VME_i)%>%
  bind_cols(st_coordinates(.)) %>%
  select(x = X,
         y = Y) %>%
  st_set_geometry(NULL)

#This was here for the joining of NV and OBIS, but I dont have OBIS
vmexy <- bind_rows(nv) %>%
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
         de = if_else(de > 1e-13, 1e-13, de),
         de = de / sum(de))

biasr <- rast(bat)
biasr[bias$cell] <- bias$de
writeRaster(biasr, "C:/Users/a39495/Documents/Projects/Mission_Atlantic/Mission_Atlantic/bias.tif", overwrite = TRUE)

# Generate background points
set.seed(100)
bak <- sample(x = bias$cell,
              size = 51000,
              prob = bias$de,
              replace = TRUE) %>%
  unique()

## Cell numbers with VMEs. Remove duplicates (by VME code)
nv <- read_sf("C:/Users/a39495/Documents/Projects/Mission_Atlantic/Mission_Atlantic/NovasArc_VME_2024.gpkg") %>%
  dplyr::filter(vme %in% VME_i)%>%
  select(vme)

# VME points:
# ob <- read_sf("./data/OBIS_2024.gpkg") %>%
#   select(vme)
vme <- bind_rows(nv)
vmec <- cells(bat, vect(vme)) %>%
  as_tibble() %>%
  mutate(vme = vme$vme) %>%
  select(-ID) %>%
  distinct()


cat(paste0(VME_i, "\n"))
#vme_p<-vme[vme$vme %in% VME_i, ] # Presences

# VMEs
## Terrain and CNRM model in vme locations
ve1 <-  terra::extract(env1, vmec$cell) %>%
  mutate(vme = vmec$vme,
         occ = 1
  ) %>%
  relocate(vme, occ) %>%
  write_rds("C:/Users/a39495/Documents/Projects/NEMO/data/vme_terrain_cnrm_present.rds")


bak_sf<-bias[bias$cell %in% bak, c('x', 'y')]
vme_sf<-bias[bias$cell %in% vmec$cell, c('x', 'y')]
PredVars<-c('depth', 'planc3', 'profc3', 'slope21', 'slope3', 'tpi21', 'tpi3', 'vrm21', 'vrm3', 'salt_scen', 'SDsalt_scen', 'temper_scen', 'SDtemper_scen', 'c_spd_scen', 'SDc_spd_scen')


###############################################################################
vme_all<-rbind(data.frame(vme_sf, "occ" = 1), data.frame(bak_sf, "occ" = 0))
vme_all <- st_as_sf(vme_all, coords = c("x","y"), crs = crs(bat)) # Absences

env_df<-terra::extract(env1, vme_all)
env_df$occ<-vme_all$occ

env_df_full<-env_df[complete.cases(env_df[,PredVars]),]
vme_all_full<-vme_all[complete.cases(env_df[,PredVars]),]

#https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html

#Checkerboard2
env_sc<-stack(env1[[PredVars]])

# cb2 <- get.checkerboard2(st_coordinates(vme_sf), 
#                          env_sc$depth, 
#                          bak_sf, 
#                          aggregation.factor=c(5,5))
# evalplot.grps(pts = vme_sf, pts.grp = cb2$occs.grp, envs = env_sc$depth)
# evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", occs.z = env_df_full[env_df_full$occ == 1,], 
#                      bg.z =  env_df_full[env_df_full$occ == 0,], occs.grp = cb2$occs.grp, bg.grp = cb2$bg.grp)


tic()
e.mx <- ENMevaluate(occs = st_coordinates(vme_all_full[vme_all_full$occ == 1,]), 
                    envs = stack(env1[[PredVars]]), 
                    bg = st_coordinates(vme_all_full[vme_all_full$occ == 0,]), 
                    algorithm = 'maxnet', partitions = 'checkerboard2', 
                    tune.args = list(fc = c("LQ","LQH","H"), rm = 1:3), ## "L","LQ",
                    parallel = TRUE, numCores = 4)
toc()
###############################################################################
