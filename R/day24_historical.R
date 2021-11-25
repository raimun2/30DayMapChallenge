# ruta patrimonial plomo 2d

pacman::p_load(rayshader, tidyverse, GPStream, sp, elevatr, sf)

stream <- read_stream_file("data/ruta_patrimonial_plomo.gpx")
stream <- stream[1:which(stream$ele == max(stream$ele)),]

stream <- stream %>% smooth_stream(alpha = 0.01)

stream_espacial <- SpatialPoints(coords=stream[,4:5], proj4string=CRS("+proj=longlat +datum=WGS84"))

zoom = 12
stream_raster <- get_elev_raster(stream_espacial, zoom)

stream$ele <- raster::extract(stream_raster, stream_espacial)

stream_mat <- raster_to_matrix(stream_raster) 

stream_mat_small <- resize_matrix(stream_mat,0.4)



filename_map = tempfile()

stream_mat_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(stream_mat_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(stream_mat_small,zscale=6), 0.5) %>%
  add_shadow(ambient_shade(stream_mat_small), 0) %>%
  add_shadow(texture_shade(stream_mat_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>% 
  add_overlay(generate_point_overlay(st_as_sf(stream_espacial), color="gold", size=5, pc=19, 
                                     raster::extent(stream_raster), heightmap = stream_mat_small)) %>% 
  save_png(filename_map, filename = "maps/day24_historical.png", 
           title_text = "Patrimonial route\nto Apu Wamani",
           title_color = "white", title_position  = "southeast", title_offset = c(50, 30),
           title_size = 120)

