library (sf)

p1 <- data.frame(lon = seq(-45, 9.5, by = 0.1), lat = 56)
p2 <- data.frame(lon = 10.41, lat = 57.43)
p3 <- data.frame(lon = 12.10113, lat = 57.84105)

p4 <- data.frame(lon = 11.24, lat = 59.09)
p5 <- data.frame(lon = 23.8, lat = 68)
p6 <- data.frame(lon = 38, lat = 68)
p7 <- data.frame(lon = 38, lat = seq(68, 83, by = 0.1))
p8 <- data.frame(lon = seq(38, -45, by = -0.5), lat = 83)
p9 <- data.frame(lon = -45, lat = seq(83, 56, by = -0.25))
pts <- as.matrix(rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9))

pol <- st_polygon(list(pts)) %>%  #This makes an sfg object
  st_sfc(crs = 4326)
pol <- st_sf(geometry = pol) %>%
  st_write("./data/study_area.gpkg")
