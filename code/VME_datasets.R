library(tidyverse)
library(sf)

novasarc.proj <-  "+proj=laea +lat_0=69 +lon_0=-4 +datum=WGS84"

nov <- read_csv("/home/julian/Documents/NovasArc3/data/NovasArc_database_2024.csv") %>%
  mutate(lat = Lat_start,
         lon = Lon_start) %>%
  filter(lat>=56,
         lat<=83,
         lon>=-45,
         lon<=38) %>%
  select(taxon, vme, lat, lon) %>%
  drop_na() %>%
  mutate(taxon = str_replace(taxon, " sp.", ""),
         taxon = str_replace(taxon, " sp", "")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform("+proj=laea +lat_0=69 +lon_0=-4 +datum=WGS84") %>%
  write_sf("./data/NovasArc_VME_2024.gpkg")

tx <- nov %>%
  select(taxon, vme) %>%
  st_set_geometry(NULL) %>%
  distinct() %>%
  arrange(taxon)

id <- worrms::wm_name2id_(nov$taxon) %>%
  discard(. %in% c("-999", "Not found"))

ocs <- list()

for (i in 1:length(id)){
  print(i)

  ocs[[i]] <- occurrence(taxonid = id[[i]],
                   geometry = "POLYGON ((-45 56, -45 83, 38 83, 38 55, -45 56))",
                   fields = c("scientificName", "decimalLongitude",
                              "decimalLatitude", "datasetName"))
}

obis <- ocs %>%
  bind_rows() %>%
  rename(lat = decimalLatitude,
         lon = decimalLongitude,
         taxon = scientificName) %>%
  left_join(tx, by = "taxon") %>%
  filter(!is.na(vme)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform("+proj=laea +lat_0=69 +lon_0=-4 +datum=WGS84") %>%
  write_sf("./data/OBIS_2024.gpkg")
