pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, osmdata, Rcpp, rgeos, loscolores)

paleta <- loscolores(18, type = "c")
paleta_ord <- orden_paleta(paleta)

structure(paleta_ord, class = "palette", index = "all")


plomo <- read_rds("data/streams_rai600.RData")$streams %>% 
  filter(id == 2950140505 | id == 4367078150) %>% 
  rename_stream() 

zoom = 12

plomo_espacial <- SpatialPoints(coords=plomo[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

raster_plomo <- get_elev_raster(plomo_espacial,zoom)

#plomo$ele = raster::extract(raster_plomo, plomo_espacial)

plomo_mat = raster_to_matrix(raster_plomo)
plomo_small = resize_matrix(plomo_mat,0.5)



plomo_small %>% 
  height_shade(texture = heat.colors(256)) %>% 
  add_overlay(sphere_shade(plomo_small, texture = "desert", 
                           zscale=4, colorintensity = 3), alphalayer=0.8) %>% 
  plot_3d(plomo_small,zscale=10, windowsize = c(1000, 700),
          shadowcolor="#40310a", watercolor="#233aa1", background = "black",
          theta=140,  phi=53, zoom=0.17, fov=0)
  

render_snapshot(filename = "maps/day17_land.png",
                title_text = "",
                title_size = 1, title_font = "Helvetica",
                title_position = "northeast")
