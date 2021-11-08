library(rgdal)
library(sp)
library(raster)
library(elevatr)
library(tidyverse)
library(ggmap)

andes_azul <- read.table("data/andesazul/andes_azul.csv", sep = ";", dec = ",")
colnames(andes_azul) <- c("Cumbre", "ele", "lat", "lon")
aar_sp <- SpatialPointsDataFrame(data=andes_azul[,4:3],  coords=andes_azul[,4:3], proj4string=CRS("+proj=longlat +datum=WGS84"))


rutas <- read_csv("data/andesazul/rutas.csv")
rutas$lon <- round(rutas$lon,4)
rutas$lat <- round(rutas$lat,4)
rutas <- unique(rutas)
rutas_sp <- SpatialPointsDataFrame(data=rutas,  coords=rutas[,1:2], proj4string=CRS("+proj=longlat +datum=WGS84"))

maptype = 'terrain'
map = ggmap::get_map(bbox(rutas_sp), maptype = maptype, zoom=12)

ggmap(map) +
  geom_rect(aes(xmin=-Inf,xmax= Inf,ymin=-Inf,ymax= Inf), fill="blue", alpha = 0.05) +
  geom_point(data=rutas, aes(lon,lat, col=1), size=1) +
  geom_point(data=andes_azul, aes(lon, lat), size = 5, col = "Darkblue") +
  theme_void()+
  theme(legend.position = "none") +
  annotate("text", x = -70.23, y = -33.31, hjust = 1, size = 10, fontface=2, col="white",
           label = "Matt's Andes\n Azul Round") 


ggsave("maps/day08_blue.png", units="in", width=13, height=12.8, dpi=300)

