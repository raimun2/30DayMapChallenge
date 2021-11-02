library(tidyverse)
library(ggmap)
library(GPStream)
library(sp)
library(elevatr)
library(raster)

data <- read_rds("data/streams_rai600.RData")

streams <- 
  data$streams %>% 
  filter(id == 2950140505) %>% 
  rename_stream() 

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

spatialstream <- SpatialPoints(streams[,c("lon","lat")],proj4string = CRS(prj_dd))

bboxexp <- bbox(spatialstream)
bboxexp[1] <- bboxexp[1]-0.05
bboxexp[2] <- bboxexp[2]-0.02
bboxexp[3] <- bboxexp[3]+0.05
bboxexp[4] <- bboxexp[4]+0.02

spatialstream@bbox <- bboxexp

elev = get_elev_raster(spatialstream, prj = prj_dd, z = 14)
 
curvas_nivel = rasterToContour(elev,nlevels = 100)

puntos_contorno = do.call(rbind, lapply(curvas_nivel$level,
                                        function (y) do.call(rbind,lapply(
                                          coordinates(curvas_nivel[curvas_nivel$level==y,])[[1]]  ,
                                          function(x) data.frame(level=y,x)))))

linea_contorno = do.call(rbind, lapply(curvas_nivel$level,
                                       function (y) do.call(rbind,lapply(
                                         coordinates(curvas_nivel[curvas_nivel$level==y,])[[1]]  ,
                                         function(x) nrow(data.frame(level=y,x))))))



colnames(puntos_contorno) = c("ele","lon","lat")
puntos_contorno$ele = as.numeric(as.character(puntos_contorno$ele))

puntos_contorno$linea = rep(1:nrow(linea_contorno),linea_contorno)



maptype = 'toner-lite'
map = suppressWarnings(suppressMessages(ggmap::get_map(bboxexp, maptype = maptype, zoom=6, source="stamen")))


ggmap(map) +
  geom_path(data=puntos_contorno,  aes(lon,lat, group=linea, col=ele)) +
  geom_path(data=streams,shape=".",col="yellow",aes(lon,lat), size = 2) +
  theme_void() +
  theme(legend.position = "none") + 
  scale_colour_viridis_c(option = "A", direction =  -1) +
  annotate("text", x = -70.37, y = -33.228, hjust = 0, size = 10, fontface =2,
           label = "Gradient ascent in \nCerro el Plomo") +
  annotate("text", x = -70.37, y = -33.34, hjust = 0, size = 5, 
           label = "Author: @raimun2 \n#30DayMapChallenge - Day 2") 

ggsave("maps/day03_lines.png", units="in", width=13, height=7.45, dpi=300)


