#' @export
#' @title Create Analysis Region Mask
#'
#' @description Create a mask of the same size and shape as \code{raster} with
#' \code{NA} values everywhere outside of the analysis region. Cells included
#' in the anlaysis region are determined by the range of cell values covered
#' by \code{points}. First, the range of cell values which fall under
#' \code{points}  for each raster layer included in \code{mask_vars} is
#' calculated, and only cells whose values fall within that range are included
#' in the analysis region.
#' The range can be expanded by a factor indicated by expansion_factor.
#'
#' @param raster SpatRaster with layers for each analysis variable
#' @param points SpatVector of points or polygons to be used for calculating
#' range of allowed values for the analysis region
#' @param mask_vars character vector of names of raster layers to include
#' in calculating allowable ranges. If NULL, all layers will be included.
#' @param expansion_factor Factor to expand the range by. Default is 1, which
#' indicates no expansion. 0.5 will reduce the range by 50%. 2 will double the
#' range.
#'
#' @return SpatRaster with NA values everywhere outside analysis region
#' and 1 everywhere within the analysis region.
#'
create_analysis_region_mask <- function(raster,
                                        points,
                                        mask_vars = NULL,
                                        expansion_factor = 1) {
  vars_raster <- terra::subset(raster, mask_vars)
  range_mx <- extract_range(
    raster = vars_raster,
    extraction_locations = points,
    expansion_factor = expansion_factor
  )
  mask_by_range(
    raster = raster,
    range_mx = range_mx
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
#' @param extraction_locations A SpatVector of points or polyogons to extract range from.
#' @param expansion_factor Factor to expand the range by. 1 indicates no expansion.
#' 0.5 will reduce the range by 50\%. 2 will double the range.
#'
#' @return A matrix that holds the min & max initiation limits of each raster
#' layer
#'
extract_range <- function(raster,
                          extraction_locations,
                          expansion_factor = 1) {

  if (is.null(raster)) {
    stop("Must provide a raster with at least 1 layer")
  }

  # Extract all variable values from initiation buffers
  values <- terra::extract(raster, extraction_locations)
  values$ID <- NULL

  # Find the min and max of the maximum variable values in each buffer
  min_values <- apply(values, 2, min, na.rm = TRUE)
  max_values <- apply(values, 2, max, na.rm = TRUE)

  # Expand each initiation range
  range <- max_values - min_values
  min_values <- min_values - (expansion_factor - 1) * (range / 2)
  max_values <- max_values + (expansion_factor - 1) * (range / 2)

  # Create a matrix that holds each variable's initiation range
  var_count <- terra::nlyr(raster)
  range_mx <- matrix(c(min_values, max_values), nrow = var_count)
  colnames(range_mx) <- c("min", "max")
  rownames(range_mx) <- names(min_values)

  return(range_mx)
}

#' @title Mask a raster
#'
#' @description Create a raster mask based on an allowable range of
#' values
#'
#' @param raster A SpatRaster
#' @param range_mx A matrix with one row for each layer of the raster to
#' use when calculating mask, and a column each for "min" and "max", indicating
#' min and max allowable values. Rownames must match layer names in raster.
#'
#' @return A SpatRaster mask: Values are NA where values fall outside the
#' range and 1 where values fall within the range.
#'
mask_by_range <- function(raster,
                          range_mx) {
  rep <- 1

  for (var_name in rownames(range_mx)) {
    var_raster <- raster[[var_name]]

    # Get variable value limits
    min_value <- range_mx[var_name, "min"]
    max_value <- range_mx[var_name, "max"]

    # NA-out cells with values outside variable initiation range
    var_mask <- terra::app(var_raster, function(x) {
      ifelse(x < min_value | x > max_value, NA, 1)
    })

    # Update the raster in the input raster stack
    if (rep == 1) {
      range_raster <- var_mask
    } else {
      range_raster <- c(range_raster, var_mask)
    }


    rep <- rep + 1
  }

  names(range_raster) = rownames(range_mx)

  # Only include cells with ALL values within the initiation range
  all(range_raster)

}
