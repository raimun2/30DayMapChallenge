# peaks per commune
pacman::p_load(chilemapas, raster, rnaturalearth, tidyverse, xml2, XML)

base <- mapa_comunas
base$idcom <- 1:nrow(base)
# 
base_sppoly <- as_Spatial(base$geometry)
# 
# spdf_chile <- ne_countries(country = 'chile', scale = 110)
# spdf_chile_cont <- crop(base_sppoly, extent(spdf_chile))
# 
# base$geometry <- st_as_sfc(spdf_chile_cont)


doc <- read_xml( "data/peaks_chile.osm" )
nodes <- xml_find_all( doc, "//tag[@k='natural']/parent::node" )

peaks <- data.frame( id  =     xml_attr( nodes, "id" )  %>% as.numeric(),
                     lat =     xml_attr( nodes, "lat" ) %>% as.numeric(),
                     lon =     xml_attr( nodes, "lon" ) %>% as.numeric(),
                     ele =     xml_find_first( nodes, ".//tag[@k='ele']") %>% xml_attr("v") %>% as.numeric(),
                     name =    xml_find_first( nodes, ".//tag[@k='name']") %>% xml_attr("v"),
                     amenity = xml_find_first( nodes, ".//tag[@k='natural']") %>% xml_attr("v"),
                     stringsAsFactors = FALSE)

peaks_2000 <- peaks[peaks$ele >= 2000 & !is.na(peaks$ele),]

peaks_sp <- SpatialPointsDataFrame(coords = peaks_2000[,3:2], data=peaks_2000, proj4string =  CRS("+proj=longlat +ellps=GRS80 +no_defs"))



res <- over(peaks_sp, base_sppoly)
n_peaks <- table(res) %>% as_tibble()
colnames(n_peaks) <- c("idcom", "n_peaks")

base <- merge(base, n_peaks, by="idcom", all=TRUE)
base$n_peaks[is.na(base$n_peaks)] <- 0

base$n_peaks_r <- round(base$n_peaks/10) %>% factor()

ggplot(base) + 
  geom_sf(aes(fill = n_peaks_r, geometry = geometry), col = "black") +
  xlim(-101,-65) +
  scale_fill_viridis_d(name = "", option= "B", direction = -1) +
  theme_void(base_size = 13) +
  annotate("text", x = -100.612, y = -30.344, hjust = 0, size = 13, fontface =2,
           label = "N° of peaks over\n2000 masl in \neach Chilean \ndistrict") 


ggsave(file="maps/day26_choropleth.png", units="in", width=9, height=7, dpi=300, bg = "white")

