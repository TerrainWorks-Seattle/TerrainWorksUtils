#' @export
#'
#' @title Fix a misfactored raster
#'
#' @description Creates a correctly-factored version of a misfactored
#' \code{SpatRast} object.
#'
#' @details terra seems to have issues loading some factor raster files. For
#' instance, a factor raster made using the ArcGIS 'Polygon to Raster' tool and
#' then loaded with \code{terra::rast()} will only show a 'Count' field instead
#' of the field specified in 'Polygon to Raster'. Additionally, the 'Count'
#' value assigned to each cell appears to be misleveled by 1 row when inspected
#' in the raster's \code{terra::cats()} table. The \code{fixFactorRaster()}
#' function attempts to take a faulty raster and map its values to the correct
#' field, as it still exists in the \code{terra::cats()} table.
#'
#' @param raster The faulty factor \code{SpatRast} object.
#'
#' @examples
#' \donttest{
#' library(TerrainWorksUtils)
#'
#' faultyRaster <- terra::rast("C:/Work/netmapdata/pack_forest/geounit.tif")
#' faultyRaster
#' terra::cats(faultyRaster)
#'
#' fixedRaster <- fixFactorRaster(faultyRaster)
#' fixedRaster
#' terra::cats(fixedRaster)
#' }

fixFactorRaster <- function(
  raster = NULL
) {

  if (!terra::is.factor(raster))
    return(raster)

  if (terra::nlyr(raster) != 1)
    stop("Can only fix single-band rasters")

  # Determine factor levels
  levelsDf <- terra::cats(raster)[[1]]
  levelsCol <- which(sapply(levelsDf, class) == "character")
  levels <- levelsDf[,levelsCol]

  # Map numeric factor values to their corresponding char values
  numericValues <- terra::values(raster)[,1]
  factorValues <- levels[numericValues]

  # Fill a new factor raster with the character factor values
  factorRaster <- terra::rast(
    extent = terra::ext(raster),
    crs = terra::crs(raster),
    resolution = terra::res(raster),
    vals = factorValues,
    names = colnames(levelsDf)[levelsCol]
  )

  return(factorRaster)

}
