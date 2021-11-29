pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, geosphere, grid, gganimate, loscolores, RColorBrewer, cowplot)

NULL_STREAMS <- read_rds("data/streams_rai1058.rds")$streams %>% 
  uniform_stream() %>% 
  filter(lat <= -33.3, lat >= -33.55,
         lon <= -70.4, lon >= -70.65) 

prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

spatialstream <- SpatialPoints(NULL_STREAMS[,c("lon","lat")], proj4string = CRS(prj_dd))

maptype <- 'terrain'
map <- ggmap::get_stamenmap(bbox(spatialstream), maptype = maptype, zoom=13)

maptype2 <- 'toner-background'
map2 <- ggmap::get_stamenmap(bbox(spatialstream), maptype = maptype2, zoom=13)

rast_base <- raster(nrows= attributes(map[])$dim[1], ncols = attributes(map[])$dim[2], ext = extent(spatialstream))
sp_rast <- rasterize(spatialstream, rast_base, field=1)

mat_pts <- calc(sp_rast, function(x) is.na(x))
mat_pts <- as.matrix(mat_pts, ncol = attributes(map[])$dim[2])

m_inv <- as.raster(apply(map, 2, function(x) rgb(t(255-col2rgb(x))/255)))
mat_inv <- as.matrix(m_inv, ncol = attributes(map[])$dim[2])

m_ras <- as.raster(apply(map2, 2, function(x) rgb(t(col2rgb(x))/255)))
mat_ras <- as.matrix(m_ras, ncol = attributes(map[])$dim[2])

m_neg <- ifelse(mat_pts, mat_inv ,mat_ras)
ras_neg <- as.raster(m_neg)
class(ras_neg) <- class(map)
attr(ras_neg, "bb") <- attr(map, "bb")

p <- ggmap(ras_neg)  +  
  theme_void() +
  ggtitle("Negative space") +
  theme(plot.title = element_text(size=30, face = "bold", hjust = 0),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))
  
ggsave(plot = p, "maps/day29_null.png", units="in", width=13, height = 11.25, dpi=300)
