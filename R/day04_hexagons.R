library(tidyverse)
library(GPStream)

data <- read_rds("data/streams_rai600.RData")

streams <- 
  data$streams %>% 
  rename_stream() %>% 
  filter(lat > -34,
         lat < -33,
         lon > -71.2,
         lon < -70) 

streams$lon <- round(streams$lon, 3)
streams$lat <- round(streams$lat, 3)

str_un <- unique(streams[,c("lon","lat")])

ggplot(str_un, aes(lon,lat)) + 
  geom_density_2d_filled(n=200, h=0.01) +
  geom_hex(binwidth = c(.005, .005), aes(fill=NA), col = "white") +
  scale_fill_viridis_d(option="B", direction = 1) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black")) +
  coord_equal() +
  annotate("text", x = -70.7, y = -33.23, hjust = 0, size = 10, fontface =2, col="white",
           label = "Training coverage and  \ndensity in Santiago") +
  annotate("text", x = -70.3, y = -33.5, hjust = 0, size = 5, col="white", 
           label = "Author: @raimun2 \n#30DayMapChallenge - Day 4") 

ggsave("maps/day04_hexagons.png", units="in", width=13, height = 7.8, dpi=300)

