pacman::p_load(rnaturalearth, sp, rnaturalearthhires, raster, tidyverse, sf, loscolores)

glaciers <- ne_download(scale = 10, type = 'glaciated_areas', category = 'physical')
oceans <- ne_download(scale = 10, type = 'ocean', category = 'physical')

clima <- getData('worldclim', var='bio', res=10)

bio <- clima$bio6

test_spdf <- as(bio, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

paletas <- loscolores(2, type = "d")
  
p<-ggplot() +  
  geom_polygon(data=oceans, aes(x=long, y=lat, group=group), 
              fill="darkblue", col="black") +
  geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=glaciers, aes(x=long, y=lat, group=group), 
               fill="white", col="white") +
  scale_fill_gradientn(colours=paletas) + 
  theme_void() + 
  theme(legend.position = "none") +
  coord_sf(crs = st_crs(4326), ndiscr = F)



ggsave(p, file="maps/day13_naturalearth.png", units="in", width=13, height=9, dpi=300)
