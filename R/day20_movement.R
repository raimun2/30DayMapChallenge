pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, geosphere, grid, gganimate, loscolores)

movement <- read_rds("data/streams_rai1058.rds")

meta <- movement$act_data

casa <- data.frame( "lon" = c(-70.53817271439917),
                    "lat" = c(-33.38891748078443 ))

casa <- SpatialPointsDataFrame(coords = casa[,3:4], data= casa, proj4string = CRS("+proj=longlat +datum=WGS84"))

inicios <- meta %>% select(id, start_longitude, start_latitude) %>% 
  mutate(borde = "inicio") %>% rename(lon = start_longitude,
                                      lat = start_latitude) %>% as_tibble()
fines <-   meta %>% select(id, end_latlng2, end_latlng1) %>% 
  mutate(borde = "final") %>% rename(lon = end_latlng2,
                                     lat = end_latlng1) %>% as_tibble()

inicios <- SpatialPointsDataFrame(coords = inicios[,2:3], data= inicios, proj4string = CRS("+proj=longlat +datum=WGS84"))
fines <- SpatialPointsDataFrame(coords = fines[,2:3], data= fines, proj4string = CRS("+proj=longlat +datum=WGS84"))

dist_ini <-   distm(casa, inicios, fun = distHaversine)
dist_fin <-   distm(casa, fines, fun = distHaversine)
dist_prom <- (dist_ini + dist_fin) / 2

meta$dist_min <- t(dist_prom)

df_fh <- meta %>% select(id, dist_min, type, elapsed_time) %>% filter(type == "Run", dist_min <= 505)

streams <- movement$streams %>% filter(id %in% df_fh$id) %>% rename_stream()
streams <- streams %>% left_join(df_fh, by = "id")

streams$rel_time <- (streams$time / streams$elapsed_time)*100

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

spatialstream <- SpatialPoints(streams[,c("lon","lat")],proj4string = CRS(prj_dd))

maptype = 'terrain'
map = ggmap::get_map(bbox(spatialstream), maptype = maptype, zoom=13)

pal180 <- c(as.character(loscolores(2,type="d")), 
            as.character(loscolores(3,type="d")), 
            as.character(loscolores(4,type="d")))

p <- ggmap(map) + 
      geom_point(streams, mapping = aes(lon, lat, alpha=0.7, col=factor(id))) +
      theme_void() +
      theme(legend.position = "none",
            plot.title = element_text(size = 30, face = "bold", hjust = 0.5)) +
      ggtitle("RUN TO THE HILLS") +
      scale_color_manual(values = pal180) + 
      transition_time(rel_time) +
      shadow_wake(wake_length = 0.1, alpha = FALSE) 

animate(p, height = 710, width =976)

anim_save(file="maps/day20_movement.gif")
