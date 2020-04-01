#' Create raster template
#' @param ext An object of class extent or a vector contain xmin xmax ymin ymax
#' @param cellsize Output cell of raster, either a one or two element vector
#' @param offset A one or two element vector providing values for how raster
#' should be shifted in the x and y direction
#' @param crs CRS of output raster, defaulting to EPSG:4326
#' @return A raster with values from 1:ncell
#' @details A convenience function to allow easy creation of rasters with
#' arbitrary cell size, for populating with other values.
#' with other
#' @importFrom raster raster extent shift ncell values<-
#' @examples
#' e <- c(25, 27, -14, -12)
#' r <- raster_template(e, cellsize = 0.05)
#' r2 <- raster_template(raster::extent(r), cellsize = 0.1)
#' r3 <- raster_template(raster::extent(r), offset = 0.05, cellsize = 0.1)
#' par(mfrow = c(1, 3))
#' for(i in list(r, r2, r3)) raster::plot(i)
#' @export
raster_template <- function(ext, cellsize, offset = NULL,
                            crs = "+proj=longlat +datum=WGS84") {

  if(class(ext) == "Extent") {
    r <- raster(x = ext, res = cellsize, crs = crs)
  } else if(is.numeric(ext)) {
    if(length(ext) != 4) {
      stop("Need a vector with 4 coordinates, xmin, ymin, xmax, ymax")
    }
    r <- raster(x = extent(ext), res = cellsize, crs = crs)
  }
  values(r) <- 1:ncell(r)

  if(!is.null(offset)) {
    if(length(offset) == 2) {
      r <- shift(r, dx = offset[1], dy = offset[2])
    } else if(length(offset) == 1) {
      r <- shift(r, dx = offset, dy = offset)
    } else {
      stop("offset must be a one or two-element vector", call. = FALSE)
    }
  }
  return(r)
}

