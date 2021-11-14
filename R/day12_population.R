pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, osmdata, geosphere, grid)

population <- read_rds("data/streams_rai1058.rds")

meta <- population$act_data

casas <- data.frame("casa" = c("2015 - 2017", "2017 - 2019", "2019 - 2021"),
                    "casa_min" = c(1,2,3),
                    "lon" = c(-70.55379487305301, -70.53620933573374, -70.53817271439917),
                    "lat" = c(-33.38054221982424, -33.382684125797304, -33.38891748078443 ))

casas <- SpatialPointsDataFrame(coords = casas[,3:4], data= casas, proj4string = CRS("+proj=longlat +datum=WGS84"))

inicios <- meta %>% select(id, start_longitude, start_latitude) %>% 
  mutate(borde = "inicio") %>% rename(lon = start_longitude,
                                      lat = start_latitude) %>% as_tibble()
fines <-   meta %>% select(id, end_latlng2, end_latlng1) %>% 
  mutate(borde = "final") %>% rename(lon = end_latlng2,
                                     lat = end_latlng1) %>% as_tibble()

inicios <- SpatialPointsDataFrame(coords = inicios[,2:3], data= inicios, proj4string = CRS("+proj=longlat +datum=WGS84"))
fines <- SpatialPointsDataFrame(coords = fines[,2:3], data= fines, proj4string = CRS("+proj=longlat +datum=WGS84"))

dist_ini <-   distm(casas, inicios, fun = distHaversine)
dist_fin <-   distm(casas, fines, fun = distHaversine)
dist_min <- ifelse(dist_ini < dist_fin, dist_ini, dist_fin)

meta$casa_min <- apply(dist_min, 2, function(x) which(min(x)==x))
meta$dist_min <- apply(dist_min, 2, min)

df_fh <- meta %>% select(id, dist_min, casa_min, type) %>% filter(type == "Run", dist_min <= 505)

streams <- population$streams %>% filter(id %in% df_fh$id) %>% rename_stream()
streams <- streams %>% left_join(df_fh, by = "id")

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

spatialstream <- streams %>% filter(!(id %in% c(4267535545, 765923072)))

spatialstream <- SpatialPoints(spatialstream[,c("lon","lat")],proj4string = CRS(prj_dd))

streams <- streams %>% left_join(casas@data[,1:2], by = "casa_min")

maptype = 'terrain'
map = ggmap::get_map(bbox(spatialstream), maptype = maptype, zoom=13)

ggmap(map) + 
  geom_bin_2d(streams, bins = 100,
              mapping = aes(lon, lat, fill=factor(casa), alpha=0.7)) +
  geom_point(data=casas@data, aes(lon,lat)) +
  facet_wrap(~casa, ncol = 2) +
  theme_void() +
  labs(tag = "Spatial signatures for \ndifferent homes through \nthe years", 
       gp = gpar(fontsize = 1, family = "Times", fontface = 2)) +
  theme(plot.tag.position = c(.75, .25),
        legend.position = "none",
        text = element_text(size = 30)) 

ggsave(file="maps/day12_population.png", units="in", width=13, height=9, dpi=300)
