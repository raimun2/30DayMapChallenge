pacman::p_load(mapview, cubeview, raster, tidyverse, htmlwidgets, webshot)

clima <- getData("worldclim", var='bio', res=10)

clima <- subset(clima, 1:3)

paletas <- read_rds("data/palettes.rds")

paletas <- paletas[4,-65]
rownames(paletas) <- NULL

m <- cubeview(clima, col.regions=paletas)

saveWidget(m, "maps/day14_newtool.html", selfcontained = TRUE)

