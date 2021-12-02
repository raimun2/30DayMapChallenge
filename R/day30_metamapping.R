## metadatastreams
pacman::p_load(png, raster, ggmap, tidyverse)

maps <- list.files("maps", "*png")

map <- get_stamenmap(source = "stamen")

cols_mash <- 500
rows_mash <- 400

bbox <- attr(map, "bb")
bbox$ur.lon <- cols_mash/rows_mash * (bbox$ur.lat - bbox$ll.lat) +  bbox$ll.lon 
attr(map, "bb") <- bbox

colors <- NULL

for(i in 1:length(maps)){
  mapn <- png::readPNG(paste0("maps/",maps[i]))
  mapn <- as.raster(mapn[,,1:3])
  
  resamp <- mapn[round((1:rows_mash)*nrow(mapn)/rows_mash),round((1:cols_mash)*ncol(mapn)/cols_mash)]
  
  colors <- c(colors, as.vector(resamp))
}

colors <- unique(colors)

cols_n <- col2rgb(colors)

hsv <- t(rgb2hsv(cols_n)) %>% as.data.frame()

hsv <- hsv[order(hsv$h, hsv$v),]

colors_ordered <- rgb(hsv)

map_col <- as.raster(colors_ordered[1:333600], ncol=600)

class(map_col) <- class(map)
attr(map_col, "bb") <- attr(map, "bb")

basemap <- ggmap(map_col, extent = "device")

basemap +
  theme_void() +
  ggtitle("Color Map") +
  theme(plot.title = element_text(size=150, face = "bold", hjust = 0, 
                                  margin=margin(0,0,-150,0), color = "white"))

ggsave("maps/day30_meta.png", units="in", width=12, height = 11, dpi=300)


