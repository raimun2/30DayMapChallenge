pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, ggridges)

glat =c(-33.58, -33.38)
glon = c(-70.55, -70.4)

spatial_ramon <- SpatialPoints(coords=cbind(glon, glat), proj4string=CRS("+proj=longlat +datum=WGS84"))

DEM_ramon <- get_elev_raster(spatial_ramon, z= 13)

DEM_ramon <- crop(DEM_ramon, extent(spatial_ramon))

n_ridges <- 100

DEM_sub <- DEM_ramon[1+0:(n_ridges-1)*round(nrow(DEM_ramon)/n_ridges), , drop=FALSE]

spdf <- as(DEM_sub, "SpatialPixelsDataFrame")
raster_df <- as.data.frame(spdf)
colnames(raster_df) <- c("ele", "lon", "lat")


ggplot(raster_df, aes(x = lon, y = lat, group = lat, height = ele)) + 
  geom_density_ridges(stat = "identity", scale = 20, fill=NA, color = "black") +
  coord_equal() + 
  theme_void() +
  ggtitle("Ridge profile of Sierra de Ramon")+
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5))

ggsave(file="maps/day21_elevation.png", units="in", width=9, height=13, dpi=300, bg = "white")

