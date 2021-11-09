pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, osmdata)

cajon_map8 <- read_rds("data/streams_rai600.RData")$streams %>% 
  filter(id == 4367078150) %>% 
  rename_stream()

zoom = 12

cajon_espacial <- SpatialPoints(coords=cajon_map8[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

raster_map8 <- get_elev_raster(cajon_espacial,zoom)

cajon_map8$ele = raster::extract(raster_map8, cajon_espacial)

map8_mat = raster_to_matrix(raster_map8)
map8_small = resize_matrix(map8_mat,0.5)

filename_map = tempfile()

map8_small %>% 
  height_shade(texture = grey.colors(256)) %>% 
  add_overlay(sphere_shade(map8_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(map8_small,zscale=6), 0.5) %>%
  add_shadow(ambient_shade(map8_small), 0) %>%
  add_shadow(texture_shade(map8_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>% 
  save_png(filename_map, filename = "maps/day09_monochrome.png", 
           title_text = "Monochromatic view of the central Andes",
           title_color = "white", title_offset = c(50, 50),
           title_size = 50)
