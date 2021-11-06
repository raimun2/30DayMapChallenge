#' Elevation correction point replace elevation points for coordinates of a Elevation Model Map and return a GPS stream dataframe with a new columns that shows the difference between original elevation and elevation model map elevation.
#' The algorithm replace each point of the original stream with coordinates of Digital Elevation Map, then calculate the difference with original elevation in a new variable named 'ele_DEM'.
#'
#'
#'
#'
#' @param data dataframe or tibble containing a GPS stream.
#' @param z zoom of Digital Elevation Model Map
#' @param replace binary variable. Default value = TRUE. Defines whether to create a new column or replace the altitude by 'ele_DEM'
#'
#' @return GPS stream dataframe with 'ele_DEM' variable.
#' @export
#'
#'
ele_correction_point <- function(data, z = 13, replace=TRUE){ #generar nueva columna o no
  # define map projection
  ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # generate spatial data frame with coordinates
  puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(data$lon,data$lat)),
                                  data = data.frame(cbind(data$lon,data$lat)),
                                  proj4string = sp::CRS(ll_prj))

  # retrieve elevation from Digital elevation Model (DEM)

  if (replace==TRUE){
    data$ele = elevatr::get_elev_point(locations = puntos, units = "meters", src="aws", z=z)@data$elevation
  } else {
    data$ele_DEM = elevatr::get_elev_point(locations = puntos, units = "meters", src="aws", z=z)@data$elevation
  }

  # return enhanced data
  return(data)
}

#rast <- try(raster::getData("alt", country = country, download = TRUE, mask = mask))


#' Elevation correction raster calculate the elevation correction from a online raster file.
#' The algorithm define a map projection, then generate spatial dataframe with coordinates and finally retrive the elevation correction from a digital elevation model.
#'
#' @param data dataframe or tibble containing a GPS stream.
#' @param z zoom of Digital Elevation Model Map
#' @param replace binary variable. Default value = TRUE. Defines whether to create a new column or replace the altitude by 'ele_DEM'
#'
#' @return GPS stream dataframe with new variable 'ele_DEM'.
#' @export
#'
ele_correction_raster <- function(data, z = 13, replace=TRUE){
  # define map projection
  ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # generate spatial data frame with coordinates
  puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(data$lon, data$lat)),
                                  data = data.frame(cbind(data$lon, data$lat)),
                                  proj4string = sp::CRS(ll_prj))

  # retrieve elevation from Digital elevation Model (DEM)
  raster <- elevatr::get_elev_raster(locations = puntos, units = "meters",src="aws",z=z)

  if (replace==TRUE){
    data$ele = raster::extract(raster, puntos)
  } else {
    data$ele_DEM = raster::extract(raster, puntos)
  }

  # return dataframe
  return(data)
}

#' Elevation correction with local raster, calculate the elevation from a local raster file and add it to a new column named 'ele_DEM'
#'
#' @param data dataframe or tibble containing a GPS stream
#' @param raster raster object with elevations
#' @param replace binary variable. Default value = TRUE. Defines whether to create a new column or replace the altitude by 'ele_DEM'
#' @return GPS stream dataframe with new variable 'ele_DEM'
#' @export
#'
ele_correction_raster_local <- function(data, raster, replace=TRUE){
  # define map projection
  ll_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # generate spatial data frame with coordinates
  puntos = sp::SpatialPointsDataFrame(coords = data.frame(cbind(data$lon,data$lat)),
                                      data = data.frame(cbind(data$lon,data$lat)),
                                      proj4string = sp::CRS(ll_prj))

  # retrieve elevation from Digital elevation Model (DEM)
  #raster <- elevatr::get_elev_raster(locations = puntos, units = "meters",src="aws",z=z)
  if (replace==TRUE){
    data$ele = raster::extract(raster, puntos)
  } else{
    data$ele_DEM = raster::extract(raster, puntos)
  }


  # return dataframe
  return(data)
}



#' Elevation correction return a stream dataframe  with correction of elevation from a stream data activity.
#' The algorithm use ele_correction_point, ele_correction_raster, ele_correction_raster_local functions to calculate the elevation correction.
#' Global function if datapoints < 200, retrieve each one, else retrieve raster
#' If local raster available, extract elevations directly
#'
#' @param data dataframe or tibble containing a GPS stream
#' @param z zoom of Digital Elevation Model Map
#' @param raster raster object with elevations.
#' @param replace binary variable. Default value = TRUE. Defines whether to create a new column or replace the altitude by 'ele_DEM'
#'
#' @return GPS stream dataframe with new variable 'ele_DEM'
#' @export
#'

ele_correction <- function(data, z = 13, raster=NULL, replace=TRUE){
  if(is.null(raster)){
      if(nrow(data)<200){
        ele_correction_point(data, z, replace)
      } else {
        ele_correction_raster(data, z, replace)
      }
  } else {
    ele_correction_raster_local(data, z, raster, replace)
  }
}
