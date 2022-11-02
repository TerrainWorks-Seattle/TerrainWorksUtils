#' @export
#' @title Create Analysis Region Mask
#'
#' @description Create a mask of the same size and shape as \code{raster} with
#' \code{NA} values everywhere outside of the analysis region. Cells included
#' in the anlaysis region are determined by the range of cell values covered
#' by \code{points}. First, the range of cell values which fall under
#' \code{points}  for each raster layer included in \code{maskVars} is
#' calculated, and only cells whose values fall within that range are included
#' in the analysis region.
#' The range can be expanded by a factor indicated by expansionFactor.
#'
#' @param raster SpatRaster with layers for each analysis variable
#' @param points SpatVector of points or polygons to be used for calculating
#' range of allowed values for the analysis region
#' @param maskVars character vector of names of raster layers to include
#' in calculating allowable ranges. If NULL, all layers will be included.
#' @param expansionFactor Factor to expand the range by. Default is 1, which
#' indicates no expansion. 0.5 will reduce the range by 50%. 2 will double the
#' range.
#'
#' @return SpatRaster with NA values everywhere outside analysis region
#' and 1 everywhere within the analysis region.
#'
createAnalysisRegionMask <- function(raster,
                                     points,
                                     maskVars = NULL,
                                     expansionFactor = 1) {
  varsRaster <- terra::subset(raster, maskVars)
  rangeMx <- extractRange(
    raster = varsRaster,
    extractionLocations = points,
    expansionFactor = expansionFactor
  )
  maskByRange(
    raster = raster,
    rangeMx = rangeMx
  )
}

#'
#' @export
#' @title Extract range of values from raster
#'
#' @description Finds the range of values for each layer of a raster which
#' fall under a set of points or polygons.
#'
#' @param raster A SpatRaster of explanatory variables
#' @param extractionLocations A SpatVector of points or polyogons to extract range from.
#' @param expansionFactor Factor to expand the range by. 1 indicates no expansion.
#' 0.5 will reduce the range by 50\%. 2 will double the range.
#'
#' @return A matrix that holds the min & max initiation limits of each raster
#' layer
#'
extractRange <- function(raster,
                         extractionLocations,
                         expansionFactor = 1) {

  if (is.null(raster)) {
    stop("Must provide a raster with at least 1 layer")
  }

  # Extract all variable values from initiation buffers
  values <- terra::extract(raster, extractionLocations)
  values$ID <- NULL

  # Find the min and max of the maximum variable values in each buffer
  minValues <- apply(values, 2, min, na.rm = TRUE)
  maxValues <- apply(values, 2, max, na.rm = TRUE)

  # Expand each initiation range
  range <- maxValues - minValues
  minValues <- minValues - (expansionFactor - 1) * (range / 2)
  maxValues <- maxValues + (expansionFactor - 1) * (range / 2)

  # Create a matrix that holds each variable's initiation range
  varCount <- terra::nlyr(raster)
  rangeMx <- matrix(c(minValues, maxValues), nrow = varCount)
  colnames(rangeMx) <- c("min", "max")
  rownames(rangeMx) <- names(minValues)

  return(rangeMx)
}

#' @title Mask a raster
#'
#' @description Create a raster mask based on an allowable range of
#' values
#'
#' @param raster A SpatRaster
#' @param rangeMx A matrix with one row for each layer of the raster to
#' use when calculating mask, and a column each for "min" and "max", indicating
#' min and max allowable values. Rownames must match layer names in raster.
#'
#' @return A SpatRaster mask: Values are NA where values fall outside the
#' range and 1 where values fall within the range.
#'
maskByRange <- function(raster,
                        rangeMx) {
  rep <- 1

  for (varName in rownames(rangeMx)) {
    varRaster <- raster[[varName]]

    # Get variable value limits
    minValue <- rangeMx[varName, "min"]
    maxValue <- rangeMx[varName, "max"]

    # NA-out cells with values outside variable initiation range
    varMask <- terra::app(varRaster, function(x) {
      ifelse(x < minValue | x > maxValue, NA, 1)
    })

    # Update the raster in the input raster stack
    if (rep == 1) {
      rangeRaster <- varMask
    } else {
      rangeRaster <- c(rangeRaster, varMask)
    }


    rep <- rep + 1
  }

  names(rangeRaster) = rownames(rangeMx)

  # Only include cells with ALL values within the initiation range
  all(rangeRaster)

}
