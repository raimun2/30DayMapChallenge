pacman::p_load(mapview, cubeview, raster, tidyverse, htmlwidgets, webshot)
pacman::p_load_gh("raimun2/loscolores")

clima <- getData("worldclim", var='bio', res=10)

clima <- subset(clima, 1:3)

paletas <- loscolores(5,type="d")
  
m <- cubeview(clima, col.regions=paletas)

saveWidget(m, "temp.html", selfcontained = FALSE)

webshot("temp.html", file = "Rplot.png",
        cliprect = "viewport")
