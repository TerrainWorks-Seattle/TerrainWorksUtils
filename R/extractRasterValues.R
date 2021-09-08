#' @export
#'
#' @title Extract raster values
#'
#' @description Extracts all raster variable values at specified locations.
#'
#' @param raster A \code{SpatRaster} object to extract values from.
#' @param points A \code{SpatVector} object of points.
#' @param stringsAsFactors Whether to convert character values to factors.
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
#' rasterContinuous <- terra::rast("C:/Work/netmapdata/pack_forest/pf_dtm3.flt")
#' rasterFactor <- terra::rast("C:/Work/netmapdata/pack_forest/geo_unit.tif")
#'
#' v1 <- extractRasterValues(rasterContinuous, points)
#' v2 <- extractRasterValues(rasterFactor, points)
#'
#' # Multi-band raster
#' alignedRasters <- alignRasters(rasterContinuous, list(rasterContinuous, rasterFactor))
#' rasterStack <- c(alignedRasters[[1]], alignedRasters[[2]])
#'
#' v3 <- extractRasterValues(rasterStack, points)
#' }

extractRasterValues <- function(
  raster = NULL,
  points = NULL,
  stringsAsFactors = TRUE
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
        factor = FALSE
      )

      # Remove 'ID' column
      values <- values[,-1]

      # Map numeric factor values to their corresponding char values
      factorDf <- terra::cats(layer)[[1]]
      factorNamesCol <- which(sapply(factorDf, class) == "character")
      factorNames <- factorDf[,factorNamesCol]

      # Convert numeric factor value to character values
      indices <- values + 1
      values <- factorNames[indices]

      # Store values in dataframe
      df <- data.frame(values, stringsAsFactors = stringsAsFactors)
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
