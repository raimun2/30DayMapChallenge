pacman::p_install_gh("steveharoz/painbow")
pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, osmdata, painbow)

acantilados <- read_rds("data/streams_rai600.RData")$streams %>% 
  filter(id == 3108817153) %>% 
  rename_stream()

zoom = 11

acantilados_espacial <- SpatialPoints(coords=acantilados[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

raster_acant <- get_elev_raster(acantilados_espacial, zoom)

acantilados$ele = raster::extract(raster_acant, acantilados_espacial)

acant_mat = raster_to_matrix(raster_acant)
acant_small = resize_matrix(acant_mat,0.7)

filename_map = tempfile()

acant_small %>% 
  height_shade(texture = painbow_colors) %>% 
  add_overlay(sphere_shade(acant_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_water(detect_water(acant_small), color = "blue") %>%
  add_shadow(lamb_shade(acant_small,zscale=6), 0.5) %>%
  add_shadow(ambient_shade(acant_small), 0) %>%
  add_shadow(texture_shade(acant_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>% 
  save_png(filename_map, filename = "maps/day10_raster.png", 
           title_text = "Painbow view of Chile's Coastal Range",
           title_color = "white", title_offset = c(50, 50),
           title_size = 40)
