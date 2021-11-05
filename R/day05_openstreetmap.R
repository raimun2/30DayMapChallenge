library(xml2)
library(osmdata)
library(sp)
library(Rcpp)
library(ggmap)
library(tidyverse)
library(XML)
library(ggrepel)

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs +alpha=-17"

latmin <- -33.65
latmax <- -33.30
lonmin <- -70.90
lonmax <- -70.40

sierra_ramon <- data.frame(lon = c(lonmin , lonmax), lat = c(latmin,latmax))
sierra_ramon <- SpatialPoints(sierra_ramon[,c("lon","lat")], proj4string = CRS(prj_dd))

query_osm <- bbox(sierra_ramon) %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "natural", value = "peak")

osmdata_xml(query_osm, filename="data/ramon.osm")

doc <- read_xml( "data/ramon.osm" )
nodes <- xml_find_all( doc, "//tag[@k='natural']/parent::node" )

df_osm <- data.frame( id  =     xml_attr( nodes, "id" )  %>% as.numeric(),
                      lat =     xml_attr( nodes, "lat" ) %>% as.numeric(),
                      lon =     xml_attr( nodes, "lon" ) %>% as.numeric(),
                      ele =     xml_find_first( nodes, ".//tag[@k='ele']") %>% xml_attr("v") %>% as.numeric(),
                      name =    xml_find_first( nodes, ".//tag[@k='name']") %>% xml_attr("v"),
                      amenity = xml_find_first( nodes, ".//tag[@k='natural']") %>% xml_attr("v"),
                      stringsAsFactors = FALSE) %>% 
  filter(lat <= latmax, lat >= latmin, lon >= lonmin, lon <= lonmax)

df_osm$ele <- elevatr::get_elev_point(locations = SpatialPointsDataFrame(data=df_osm,df_osm[,c("lon","lat")], 
                                                                         proj4string = CRS(prj_dd)), 
                                      units = "meters", src="aws", z=13)@data$elevation %>% round()

maptype = 'terrain'
map = ggmap::get_map(bbox(sierra_ramon), maptype = maptype, zoom = 12)

ggmap(map)  + 
  geom_point(data = df_osm, aes(lon, lat)) +
  geom_text_repel(data = df_osm[!is.na(df_osm$name),], aes(lon, lat, label=paste0(name,"\n(",ele,")"))) + 
  annotate(
    "label", x = -70.89 , y = -33.37,
    label = "A city \nsurrounded by \nmountains",
    size = 10, fontface = 2, hjust = 0, vjust = 0, color = "white",
    fill = "black", label.color = "black" 
  ) +
  theme_void() +
  labs(caption = "Author: @raimun2 \n#30DayMapChallenge - Day 5\nData: OpenStreetMap") 

ggsave("maps/day05_openstreetmap.png", units="in", width=13, height = 11.25, dpi=300)

