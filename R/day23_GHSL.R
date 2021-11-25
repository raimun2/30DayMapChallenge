pacman::p_load(rayshader, raster, sp, tidyverse, rgdal)

temp <- tempfile()
data_url <- "http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GPW4_GLOBE_R2015A/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k/V1-0/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.zip"
download.file(data_url, temp)
unzip(temp)
unlink(temp)

pop2015 <- raster("data/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif")

max_cval15 <- 162.73  

beginCluster() 

pop2015_corr <- reclassify(pop2015, rcl = c(max_cval15, Inf, max_cval15))
endCluster() 

pop15_mat <- raster_to_matrix(pop2015_corr)

n <- 10

pop15_mat_small <- pop15_mat[(1:(nrow(pop15_mat)/n))*n,(1:(ncol(pop15_mat)/n))*n]


filename_map = tempfile()

pop15_mat_small %>% 
    height_shade(texture = c("#000000",heat.colors(256))) %>% 
    add_overlay(sphere_shade(pop15_mat_small, texture = "desert", 
                             zscale=4, colorintensity = 5), alphalayer=0.5) %>%  
    save_png(filename_map, filename = "maps/day23_GHSL.png", 
             title_text = "The Global Human Settlements",
             title_color = "white", title_offset = c(50, 50),
             title_size = 50)

