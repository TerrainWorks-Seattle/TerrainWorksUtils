#' @export
#'
#' @title Extract raster values
#'
#' @description Extracts all raster variable values at specified locations.
#'
#' @param raster A \code{SpatRaster} object to extract values from.
#' @param points A \code{SpatVector} object of points.
#'
#' @return A \code{data.frame} of values.
#'
#' @examples
#' \donttest{
#' library(TerrainWorksUtils)
#'
#' points <- terra::vect("C:/Work/netmapdata/pack_forest/PF_trainingdata.shp")
#'
#' # Single-band rasters
#' continuousRaster <- terra::rast("C:/Work/netmapdata/pack_forest/pf_dtm3.flt")
#' factorRaster <- terra::rast("C:/Work/netmapdata/pack_forest/geo_unit.tif")
#' factorRaster <- fixFactorRaster(factorRaster)
#'
#' v1 <- extractRasterValues(continuousRaster, points)
#' v2 <- extractRasterValues(factorRaster, points)
#'
#' # Multi-band raster
#' alignedRasters <- alignRasters(continuousRaster, list(continuousRaster, factorRaster))
#' rasterStack <- c(alignedRasters[[1]], alignedRasters[[2]])
#'
#' v3 <- extractRasterValues(rasterStack, points)
#' }

extractRasterValues <- function(
  raster = NULL,
  points = NULL
) {

  # Validate parameters --------------------------------------------------------

  if (terra::nlyr(raster) == 0)
    stop("Cannot extract values from a non-single-layer raster")

  # Extract values -------------------------------------------------------------

  # Project the points into the same CRS as the raster
  projectedPoints <- terra::project(points, raster)

  # Define a data frame to store all layer values
  allValues <- data.frame(dummy = rep(NA, length(points)))
  removeDummy <- TRUE

  for (i in seq_len(terra::nlyr(raster))) {
    layer <- raster[[i]]

    if (terra::is.factor(layer)) {
      # Extract numeric factor value at each point
      values <- terra::extract(
        layer,
        projectedPoints,
        method = "simple",
        factor = TRUE
      )

      # Remove 'ID' column
      values <- values[,-1]

      # Store values in dataframe
      df <- data.frame(values)
      names(df) <- names(layer)

      # Add values to full dataset
      allValues <- cbind(allValues, df)
    } else {
      # Extract continuous value at each point
      values <- terra::extract(
        layer,
        projectedPoints,
        method = "simple"
      )

      # Remove 'ID' column
      values <- values[,-1]

      # Format values
      df <- data.frame(values)
      names(df) <- names(layer)
      allValues <- cbind(allValues, df)
    }

    # Remove the 'dummy' column as soon as a real value column is added
    if (removeDummy) {
      allValues[,1] <- NULL
      removeDummy <- FALSE
    }
  }

  # Return ---------------------------------------------------------------------

  return(allValues)

}
