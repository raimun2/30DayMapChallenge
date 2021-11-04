library(tidyverse)
library(ggmap)
library(GPStream)
library(sf)
library(tmap)
library(sp)

data <- read_rds("data/streams_rai600.RData")

meta <- data$metadata

meta$dist_startend <- geosphere::distHaversine(cbind(meta$start_longitude,meta$start_latitude),
                                               cbind(meta$end_latlng2,meta$end_latlng1))

meta <- meta %>% filter(dist_startend < 200)

dat <- data$streams %>% rename_stream %>%  filter(id %in% meta$id)

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

poligonos <- dat %>%
  st_as_sf(coords = c("lon", "lat"), crs = CRS(prj_dd)) %>%
  group_by(id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

sf::sf_use_s2(FALSE)

poligonos$area <- st_area(poligonos) %>% as.numeric()

poligonos <- poligonos %>% arrange(desc(area))

poligonos$id <- round(poligonos$area/1000000,2)
poligonos$logarea <- log(poligonos$area)

top40 <- poligonos[1:43,]


png(filename = "maps/day03_polygons.png",width=13, height=9.56, units = "in", res = 300)

tm_shape(top40) +
  tm_polygons("logarea",
              palette = "BuPu",
              legend.show = FALSE) +
  tm_layout(frame = FALSE, 
            frame.lwd = NA, 
            panel.label.bg.color = NA,
            panel.label.size = 2,
            main.title = "Trail Running generated polygons \nand its covered areas (km2)", 
            main.title.position = "center",
            main.title.size = 3,
            main.title.fontface = "bold") +
  tm_facets(by = "id")

dev.off()
