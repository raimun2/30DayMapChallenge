pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, osmdata, rgeos, loscolores)

calan_ids <- read_rds("data/streams_rai600.RData")$metadata %>% 
  filter(grepl("calan", name)) %>% 
  select(id) %>% 
  unlist() %>% 
  as.numeric()

calan <- read_rds("data/streams_rai600.RData")$streams %>% 
  filter(id %in% calan_ids[2]) %>% 
  rename_stream()

calan_espacial <- SpatialPoints(coords=calan[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

bufer <- gBuffer(as(extent(calan_espacial), "SpatialPolygons"), width = 0.005)

bboxc <- bbox(bufer)
extentc <- extent(bufer)


calan_all <- read_rds("data/streams_rai600.RData")$streams %>% 
  filter(id %in% calan_ids) %>% 
  rename_stream() %>% 
  filter(lat < -33.392, 
         lat > bboxc[2,1], 
         lon < bboxc[1,2], 
         lon > bboxc[1,1])

calan_espacial <- SpatialPoints(coords=calan_all[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

zoom = 14
raster_calan <- get_elev_raster(calan_espacial, zoom)
raster_calan <- crop(raster_calan, 
                     extentc)

calan_all$ele <- raster::extract(raster_calan, calan_espacial)

calan_mat <- raster_to_matrix(raster_calan) 

paleta <- loscolores(2, type = "c")




filename_map = tempfile()

calan_mat %>% 
  height_shade(texture = paleta) %>% 
  add_overlay(generate_point_overlay(st_as_sf(calan_espacial), color="black", size=1, pc=".",
                                     extentc, heightmap = calan_mat)) %>% 
  add_overlay(sphere_shade(calan_mat, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer=0.5) %>%
  add_shadow(lamb_shade(calan_mat,zscale=6), 0.5) %>%
  add_shadow(ambient_shade(calan_mat), 0) %>%
  add_shadow(texture_shade(calan_mat,detail=8/10,contrast=9,brightness = 11), 0.1) %>% 
  save_png(filename_map, filename = "maps/day16_urbanrural.png", 
           title_text = "Urban Trails",
           title_color = "white", title_offset = c(50, 30),
           title_size = 40)
  



