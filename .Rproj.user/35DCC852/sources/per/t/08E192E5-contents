library(tidyverse)
library(ggmap)
library(GPStream)
library(sp)

data <- read_rds("data/streams_rai600.RData")

meta <- data$metadata %>% 
  filter(!is.na(max_heartrate), distance > 30, total_elevation_gain > 1200)

meta$max_heartrate <- meta$max_heartrate %>% as.numeric()
meta$average_heartrate <- meta$average_heartrate %>% as.numeric()

stream_HR <- data$streams %>% 
  filter(id == meta[which(max(meta$average_heartrate)==meta$average_heartrate),"id"]) %>% 
  rename_stream() %>% 
  ele_correction()

stream_HR$HR <- ifelse(stream_HR$heartrate>160, 3, 0)

ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(stream_HR$lon,stream_HR$lat)),
                                    data = data.frame(cbind(stream_HR$lon,stream_HR$lat)),
                                    proj4string = sp::CRS(ll_prj))

raster_dem <- elevatr::get_elev_raster(locations = puntos, units = "meters",src="aws",z=11)

test_spdf <- as(raster_dem, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "lon", "lat")

test_df <- test_df %>% filter(lat <= max(stream_HR$lat)+0.01, lat >= min(stream_HR$lat)-0.01,
                              lon <= max(stream_HR$lon)+0.01, lon >= min(stream_HR$lon)-0.01)

ggplot() +
  geom_tile(data=test_df, aes(lon,lat, fill=value), alpha = 0.8) +
  geom_point(data=stream_HR, aes(lon,lat), col = "gray", size=5) +
  geom_point(data=stream_HR, aes(lon,lat, col = HR), size=stream_HR$HR) +
  scale_colour_gradient(low = "gray", high = "red4", na.value = NA) +
  scale_fill_gradient(low = "firebrick", high = "black", na.value = NA) +
  theme_void() +
  theme(legend.position = "none") +
  labs(caption = "Author: @raimun2 \n#30DayMapChallenge - Day 6") +
  annotate(
    "text", x = -70.53 , y = -33.37,
    label = "When your heart \npumps it all",
    size = 13, fontface = 2, hjust = 0, vjust = 0, color = "white"
  ) 

ggsave("maps/day06_red.png", units="in", width=13, height=9, dpi=300)


