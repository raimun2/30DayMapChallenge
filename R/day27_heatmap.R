pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, geosphere, grid, gganimate, loscolores, RColorBrewer, cowplot)

heat <- read_rds("data/streams_rai1058.rds")$streams %>% 
  uniform_stream() %>% 
  drop_na(temp) %>% 
  filter(lat <= -33.3, lat >= -33.65,
         lon <= -70.4, lon >= -70.9) %>% 
  mutate(lon = round(lon, 3),
         lat = round(lat, 3)) %>% 
  group_by(lon, lat) %>% 
  summarize(max_temp = max(temp),
            min_temp = min(temp),
            mean_temp = mean(temp),
            range_temp = max_temp - min_temp)

paleta <- c(brewer.pal(9, "Blues")[9:1], heat.colors(15)[15:1])

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

spatialstream <- SpatialPoints(heat[,c("lon","lat")],proj4string = CRS(prj_dd))

maptype <- 'toner-2011'
map <- ggmap::get_stamenmap(bbox(spatialstream), maptype = maptype, zoom=13)

invert <- function(x) rgb(t(255-col2rgb(x))/255)    
m_inv <- as.raster(apply(map, 2, invert))

class(m_inv) <- class(map)
attr(m_inv, "bb") <- attr(map, "bb")

p <- ggmap(m_inv) + 
  geom_jitter(data = heat, aes(lon, lat, col=max_temp)) +
  scale_color_gradientn(colors=paleta, name = "Max Temperature (C°)") +
  theme_void() +
  ggtitle("Maximum temperatures while training in Santiago",
          subtitle = "Measured by smartwatch during training session") +
  theme(panel.background = element_rect(fill = "black"),
        legend.position = "bottom",
        plot.title = element_text(size=30, face = "bold", hjust = 0.5, color = "white"),
        plot.subtitle = element_text(size=22, face = "bold", hjust = 0.5, color = "white"),
        legend.title = element_text(face = "bold", color = "white"),
        legend.text = element_text(face = "bold", color = "white"))

ggdraw(p) + theme(panel.background = element_rect(fill = "black", colour = "black"))


ggsave("maps/day27_heatmap.png", units="in", width=13, height = 11.25, dpi=300)
