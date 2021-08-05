#' @export
#'
#' @title Align Rasters
#'
#' @description Aligns the grid and projection of multiple input rasters with a
#' reference raster. An input raster will be aligned if it doesn't match the
#' dimensions, resolution, extent, origin, or CRS projection of the reference
#' raster.
#'
#' @param referenceRaster A \code{SpatRaster} object to be aligned with.
#' @param inputRasters A list of \code{SpatRaster} objects to align with the
#' \code{referenceRaster}.
#'
#' @return A list of \code{SpatRaster} objects that share the same grid and
#' projection as \code{referenceRaster}.
#'
#' @examples
#' \donttest{
#' library(WetlandTools)
#'
#' referenceRaster <- terra::rast("C:/Work/netmapdata/Puyallup/grad_50.tif")
#'
#' inputRasters <- list(
#'   aligned = terra::rast("C:/Work/netmapdata/Puyallup/dev_50.tif"),
#'   unaligned = terra::rast("C:/Work/netmapdata/Puyallup/elev_puy.flt")
#' )
#'
#' alignedRasters <- alignRasters(referenceRaster, inputRasters)
#' }

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
