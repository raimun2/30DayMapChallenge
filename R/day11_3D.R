pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, osmdata, Rcpp, rgeos)

cajon_map8 <- read_rds("data/streams_rai600.RData")$streams %>% 
  filter(id == 4367078150) %>% 
  rename_stream()

zoom = 12

cajon_espacial <- SpatialPoints(coords=cajon_map8[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

raster_map8 <- get_elev_raster(cajon_espacial,zoom)
raster_map8 <- crop(raster_map8, extent(gBuffer(as(extent(cajon_espacial), "SpatialPolygons"), width = 0.05)))

cajon_map8$ele = raster::extract(r2, cajon_espacial)

map8_mat = raster_to_matrix(raster_map8)

map8_mat %>% 
  height_shade(texture = hcl.colors(256, palette = "BrBg", rev=TRUE)) %>% 
  add_overlay(sphere_shade(map8_mat, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  plot_3d(map8_mat,zscale=10, windowsize = c(1500, 1200),
          shadowcolor="#40310a", watercolor="#233aa1", background = "tan",
          theta=15,  phi=45, zoom=0.7, fov=15)
render_path(extent = attr(raster_map8,"extent"), linewidth = 5,
            lat = unlist(cajon_map8$lat), long = unlist(cajon_map8$lon),
            altitude = cajon_map8$ele, zscale=10,color="Blue", antialias=TRUE)

render_snapshot(filename = "maps/day11_3d.png",
                 title_text = "Mapocho Canyon",
                 title_size = 60, title_font = "Helvetica",
                 title_position = "northeast")
