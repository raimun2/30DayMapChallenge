library(tidyverse)
library(ggmap)
library(GPStream)
library(sp)

data <- read_rds("data/streams_rai600.RData")

streams <- 
  data$streams %>% 
  rename_stream() %>% 
  filter(lat > -33.3797581,
         lat < -33.3399706,
         lon > -70.6140635,
         lon < -70.5493432) %>% 
  ele_correction()



prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

spatialstream <- SpatialPoints(streams[,c("lon","lat")],proj4string = CRS(prj_dd))

maptype = 'terrain'
map = suppressWarnings(suppressMessages(ggmap::get_map(bbox(spatialstream), maptype = maptype, zoom=15)))

ggmap(map) +
  geom_point(data=streams,aes(lon,lat, col=ele)) +
  theme_void() +
  theme(legend.position = "none") + 
  scale_colour_viridis_c(direction = -1) +
  annotate("text", x = -70.612, y = -33.344, hjust = 0, size = 10, fontface =2,
           label = "Visualizing trails in \nCerro Manquehue") +
  annotate("text", x = -70.612, y = -33.349, hjust = 0, size = 6, fontface =2,
           label = "Each point is a GPS reading \nfrom my trail running log \ncoloured by elevation")  +
  annotate("text", x = -70.565, y = -33.378, hjust = 0, size = 5, 
           label = "Author: @raimun2 \n#30DayMapChallenge - Day 1") 

ggsave("maps/day01_points.png", units="in", width=13, height=9.56, dpi=300)

