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
