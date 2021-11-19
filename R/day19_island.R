pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream)

glat =c(-33.8, -33.3)
glon = c(-70.9, -70.4)

spatial_SCL <- SpatialPoints(coords=cbind(glon, glat), proj4string=CRS("+proj=longlat +datum=WGS84"))

DEM_SCL <- get_elev_raster(spatial_SCL, z= 10)

DEM_SCL <- crop(DEM_SCL, extent(spatial_SCL))

slopes <- raster::terrain(DEM_SCL)

DEM_mat <- raster_to_matrix(DEM_SCL)
slope_mat <-  raster_to_matrix(slopes)

plano <- (slope_mat < 0.12) && (DEM_mat < 800)
plano[is.na(plano)] <- FALSE


slope_mat[plano] <- 0
slope_mat[is.na(slope_mat)] <- 0.2

pal <- loscolores(3)

filename_map = tempfile()

DEM_mat %>%
  height_shade(texture = pal) %>% 
  add_overlay(sphere_shade(DEM_mat, texture = "imhof2", 
                           zscale=4, colorintensity = 5), alphalayer=0.7) %>%  
  add_water(detect_water(slope_mat, min_area = 50000), color="desert") %>%
  save_png(filename_map, filename = "maps/day19_islands.png", 
           title_text = "Island hills in \nSantiago",
           title_color = "black", title_offset = c(50, 50),
           title_size = 50)
