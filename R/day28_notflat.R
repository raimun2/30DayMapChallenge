pacman::p_load(tmap, sf, loscolores, rgeos, tidyverse, raster, sp, osmdata, rnaturalearth, rnaturalearthhires, xml2, XML, ggthemes, spdep)

spdf_regiones <- ne_states(country = 'chile')

extractCoords <- function(sp.df)
{
  results <- NULL
  for(j in 1:length(sp.df)){
    
    for(i in 1:length(sp.df@polygons[[j]]@Polygons))
    {
      coordji <- sp.df@polygons[[j]]@Polygons[[i]]@coords %>% as.data.frame()
      colnames(coordji) <- c("lon", "lat")
      coordji$poly <- i
      coordji$region <- j
      results <- rbind(results, coordji)
    }  
  }
  return(results)
}

proj_raimun2 <- function(spdf){
  
  coords <- extractCoords(spdf)
  
  coords.90 <- Rotation(coords[1:2], 100*pi/180) %>% data.frame()
  coords.90$polyid <- paste0(coords$region,"_",coords$poly)
  coords.90$region <- factor(coords$region)
  
  colnames(coords.90)[1:2] <- c("lon", "lat")
  
  coords.90 <- coords.90 %>% filter(lon < 70, lon > 29, lat < -50, lat > -70) %>% 
    mutate(lon = (lon - 49)*6, lat = lat - 15)
  
  ggplot(coords.90, aes(lon,lat)) + geom_point() + geom_point(data=data.frame(lon=0,lat=-90), col="red")
  
  sfpts <- st_as_sf(coords.90, coords=c("lon","lat"))
  
  xymp  <- st_sf(
    aggregate(
      sfpts,
      by = list(ID=sfpts$polyid),
      do_union=FALSE,
      FUN=function(vals){vals[1]}))
  
  xypoly = st_cast(xymp, 'POLYGON')
  
  st_crs(xypoly) <- CRS("+proj=longlat")
  
  clproj <- st_transform(xypoly, "+proj=lcc +lat_1=-85 +lat_2=-80 +lat_0=-75 +lon_0=0 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  
  
  return(clproj)
  
}

clproj <- proj_raimun2(spdf_regiones)

cl.bb <- clproj %>% st_bbox()

png(filename = "maps/day28_notflat.png",width=13, height=9.56, units = "in", res = 300)

tm_shape(clproj, bbox = cl.bb) + 
  tm_fill(col="region",legend.show = FALSE, palette = loscolores::loscolores(2,15,"d")) +
  tm_graticules( x = c(-180,-150,-125,-100,-75,-50,-25,0, 25,50,75,100,125,150), 
                 y = c(-70,-75,-80,-85,-89.9), 
                 labels.col = "white", col="grey90") +
  tm_layout(main.title = "Chilean conical projection", main.title.position = "center", main.title.size = 5) +
  tm_credits("created by @raimun2", position = c("center", "BOTTOM"), size=1)

dev.off()
