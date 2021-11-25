pacman::p_load(rgeos, tidyverse, raster, sp, osmdata, rnaturalearth, rnaturalearthhires, xml2, XML, ggthemes)

spdf_chile <- ne_countries(country = 'chile', scale = 110)
spdf_regiones <- ne_states(country = 'chile')

spdf_chile_cont <- crop(spdf_regiones, extent(spdf_chile))

lagos <- ne_download(category="physical", type="lakes", scale=10)
lagos <- crop(lagos, extent(spdf_chile_cont))

costas <- ne_download(category="physical", type="coastline", scale=50)
costas <- crop(costas, extent(spdf_chile_cont))

query_osm <- bbox(spdf_chile_cont) %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature(key = "natural", value = "peak")

#osmdata_xml(query_osm, filename="data/peaks_chile.osm")

doc <- read_xml( "data/peaks_chile.osm" )
nodes <- xml_find_all( doc, "//tag[@k='natural']/parent::node" )

peaks <- data.frame( id  =     xml_attr( nodes, "id" )  %>% as.numeric(),
                     lat =     xml_attr( nodes, "lat" ) %>% as.numeric(),
                     lon =     xml_attr( nodes, "lon" ) %>% as.numeric(),
                     ele =     xml_find_first( nodes, ".//tag[@k='ele']") %>% xml_attr("v") %>% as.numeric(),
                     name =    xml_find_first( nodes, ".//tag[@k='name']") %>% xml_attr("v"),
                     amenity = xml_find_first( nodes, ".//tag[@k='natural']") %>% xml_attr("v"),
                     stringsAsFactors = FALSE) 

peaks <- SpatialPointsDataFrame(coords = peaks[,3:2], data=peaks)
peaks <- crop(peaks, extent(spdf_chile_cont))

r <- raster(ncol=200, nrow=1000)
extent(r) <- extent(spdf_chile_cont)

r.polys <- rasterize(spdf_chile_cont, r, field = spdf_chile_cont@data$mapcolor9, fun = "sum", 
                     update = TRUE, updateValue = "NA")

raster_chile <- r.polys

chile_points <- as(r.polys,"SpatialPoints")

raster_costas <- r.polys
d_costas = gDistance(chile_points, costas, byid=TRUE)
d_min_costas = apply(d_costas,2,min)
raster_costas[!is.na(raster_costas[])] = d_min_costas

raster_lagos <- r.polys
d_lagos = gDistance(chile_points, lagos, byid=TRUE)
d_min_lagos = apply(d_lagos,2,min)
raster_lagos[!is.na(raster_lagos[])] = d_min_lagos

peaks$presencia <- 1

r.peaks <- rasterize(peaks, r, field = peaks@data$presencia, fun = "count", 
                     update = TRUE, updateValue = "NA")

r.peaks[is.na(r.polys[])] <- NA
peak_ras_points <- as(r.peaks,"SpatialPoints")

raster_nautico <- r.polys
d_min_agua <- ifelse(d_min_costas>d_min_lagos, d_min_lagos, d_min_costas)
raster_nautico[!is.na(raster_nautico[])] = d_min_agua

raster_peaks <- r.polys
d_peaks = gDistance(chile_points, peak_ras_points, byid=TRUE)
d_min_peaks = apply(d_peaks,2,min)
rm(d_peaks)
raster_peaks[!is.na(raster_peaks[])] = d_min_peaks

raster_limit <- r.polys
d_min_limit <- ifelse(d_min_agua>d_min_peaks, 2, 3)
raster_limit[!is.na(raster_limit[])] = d_min_limit

limit_spdf <- as(raster_limit, "SpatialPixelsDataFrame")
limit_df <- as.data.frame(limit_spdf)
colnames(limit_df) <- c("value", "x", "y")

ggplot() +  
  annotate("text", x = -100.612, y = -30.344, hjust = 0, size = 10, fontface =2,
           label = "Mountain or \nNautical Country?") +
  annotate("text", x = -100.612, y = -37.349, hjust = 0, size = 6, fontface =2,
           label = "Areas closest to peaks \nthan coastlines (lakes included) \nare defined as mountain zones, \notherwise as nautical zones") +
  geom_tile(data=limit_df, aes(x=x, y=y, fill=factor(value)), alpha=0.8) + 
  scale_fill_manual(values = c("coral4", "Darkblue")) +
  coord_equal() +
  theme_map() +
  theme(legend.position="none")


ggsave(file="maps/day22_boundaries.png", units="in", width=9, height=7, dpi=300, bg = "white")
