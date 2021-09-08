#' @export
#'
#' @title Properly format a factor raster
#'
#' @details terra seems to have problems reading values from some factor raster
#' files. A factor raster made using the ArcGIS 'Polygon to Raster' tool will
#' only show a 'Count' field when loaded in terra, and even that seems to be
#' misleveled by 1 row when you look at the terra::cats() table for the raster.

factorizeRaster <- function(raster) {

  if (!terra::is.factor(raster))
    return(raster)

  # Determine factor levels
  levelsDf <- terra::cats(r)[[1]]
  levelsCol <- which(sapply(levelsDf, class) == "character")
  levels <- levelsDf[,levelsCol]

  # Map numeric factor values to their corresponding char values
  numericValues <- terra::values(r)[,1]
  factorValues <- levels[numericValues]

  # Fill a new factor raster with the character factor values
  factorRaster <- terra::rast(
    extent = terra::ext(r),
    crs = terra::crs(r),
    resolution = terra::res(r),
    vals = factorValues,
    names = colnames(levelsDf)[levelsCol]
  )

  return(factorRaster)

}
