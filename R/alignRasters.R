#' @export
#'
#' @title Align Rasters
#'
#' @description Aligns the grid and projection of multiple input rasters with a
#' reference raster. In input raster will be aligned if it doesn't match the
#' dimensions, resolution, extent, origin, or CRS projection of the reference
#' raster.
#'
#' @param referenceRaster A \code{SpatRaster} object to use as a guide for
#' alignment.
#' @param inputRasters A list of \code{SpatRaster} objects to be aligned.
#'
#' @return A list of \code{SpatRaster} objects that share the same grid and
#' projection as \code{referenceRaster}.

alignRasters <- function(referenceRaster, inputRasters) {

  alignedRasters <- list()

  # For each input raster
  for (i in seq_along(inputRasters)) {
    inputRaster <- inputRasters[[i]]

    # Reproject the input raster if it doesn't align with the reference raster
    if (
      terra::ext(inputRaster) != terra::ext(referenceRaster) ||
      !all(dim(inputRaster) == dim(referenceRaster)) ||
      !all(terra::res(inputRaster) == terra::res(referenceRaster)) ||
      !all(terra::origin(inputRaster) == terra::origin(referenceRaster)) ||
      terra::crs(inputRaster) != terra::crs(referenceRaster)
    ) {
      inputRaster <- terra::project(inputRaster, referenceRaster)
    }

    # Store the aligned input raster
    alignedRasters[[i]] <- inputRaster
  }

  return(alignedRasters)

}
