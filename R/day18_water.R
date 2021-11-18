pacman::p_load(rayshader, raster, sf, sp, elevatr, tidyverse, ggmap, GPStream, marmap)

IPC <- read_rds("data/streams_rai600.RData")$streams %>% 
    filter(id == 1134046529) %>% 
  rename_stream()

glat =c(min(IPC$lat)-.3, max(IPC$lat)+.3)
glon = c(min(IPC$lon)-.3, max(IPC$lon)+.3)

spatialIPC <- SpatialPoints(coords=cbind(glon, glat), proj4string=CRS("+proj=longlat +datum=WGS84"))

DEMIPC <- get_elev_raster(spatialIPC, z= 10)

DEMIPC <- crop(DEMIPC, extent(spatialIPC))

DEM_mat <- raster_to_matrix(DEMIPC)


pal <- c(hcl.colors(95, palette = "blues", alpha = NULL, rev = FALSE, fixup = TRUE)[1:55], 
         hcl.colors(8, palette = "greens", alpha = NULL, rev = FALSE, fixup = TRUE))

filename_map = tempfile()

DEM_mat %>%
  height_shade(texture = pal) %>% 
  add_overlay(sphere_shade(DEM_mat, texture = "imhof2", 
                           zscale=4, colorintensity = 5), alphalayer=0.7) %>%  
  save_png(filename_map, filename = "maps/day18_water.png", 
           title_text = "Bathymetry of Easter Island",
           title_color = "white", title_offset = c(50, 50),
           title_size = 50)

