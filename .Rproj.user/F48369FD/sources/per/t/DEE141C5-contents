library(tidyverse)
library(ggmap)
library(GPStream)
library(sp)

data <- read_rds("data/streams_rai600.RData")
ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

str_bbox <- data$streams %>% 
  filter(id == 2305090814) %>% 
  rename_stream()

str_sp <- SpatialPointsDataFrame(coords =  str_bbox[,c("lon","lat")], data=str_bbox, proj4string = CRS(ll_prj))

stream_green <- data$streams %>% 
  rename_stream() %>% 
  filter(lon >= bbox(str_sp)[1]-0.01,
         lon <= bbox(str_sp)[3]+0.01,
         lat >= bbox(str_sp)[2]-0.01,
         lat <= bbox(str_sp)[4]+0.02)

str_sp <- SpatialPointsDataFrame(coords =  stream_green[,c("lon","lat")], data=stream_green, proj4string = CRS(ll_prj))

stream_green$col <- runif(nrow(stream_green), 0, 10)

maptype = 'terrain'
map = ggmap::get_map(bbox(str_sp), maptype = maptype, zoom=14)

ggmap(map) +
  geom_density_2d_filled(data = stream_green, aes(lon,lat), alpha = 0.5) +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  scale_colour_gradient(low = "chartreuse4", high = "darkseagreen1", na.value = NA) +
  geom_jitter(data = stream_green, aes(lon,lat, col = col)) +
  theme_void() +
  theme(legend.position = "none") +
  annotate("text", label = "Sclerophyll Trails", x = -71.43, y = -32.533, size=12, fontface = 2, col="white")
  
ggsave("maps/day07_green.png", units="in", width=9, height=11.55, dpi=300)




