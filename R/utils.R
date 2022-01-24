log_err <- function(...) {
  stop(sprintf("[%s] %s\n", Sys.time(), paste0(..., collapse = "")))
}

log_msg <-  function(...) {
  message(sprintf("[%s] %s\n", Sys.time(), paste0(..., collapse = "")))
}

log_obj <- function(obj) {
  message(sprintf("[%s] \n    %s\n",
                  Sys.time(),
                  paste0(capture.output(obj),
                         collapse = "\n    "))
  )
}

samplePolys <- function(polys,
                        sampleRate) {
  # Collect sample coordinates
  coords <- NULL
  for (i in seq_len(length(polys))) {
    poly <- polys[i]

    # Determine the number of samples to take from the polygon's area
    polyArea <- terra::expanse(poly, unit = "km")
    if (polyArea == 0) next
    sampleSize <- ceiling(polyArea * sampleRate)

    # Sample the polygon
    samplePoints <- terra::spatSample(poly, size = sampleSize)
    sampleCoords <- terra::crds(samplePoints)

    coords <- rbind(coords, sampleCoords)
  }

  return(coords)
}

#' @export
#'
#' @title Apply a set of "cats" to a factor raster
#'
#' @description Applies a given set of "cats" (factor level ID-name pairs) to a
#' factor raster. Any level names already shared between the given cats and the
#' raster will be numerically remapped to the given cats IDs.
#'
#' @details terra manages factor rasters by having each cell store an integer
#' ID (0, 1, 2, ...) which corresponds to a level name as found in
#' \code{terra::cats()}. However, if you load a raster with levels "A", "B", "C"
#' and another raster with levels "B", "C", "D", terra won't detect the
#' overlapping levels since B=1 and C=2 in the first raster and B=0 and C=1 in
#' the second. This is important when it comes to creating prediction models
#' that take factor rasters as input. Let's say that level "C" is a
#' high-probability indicator of event X. If you generate a model with raster 1
#' the model will learn to associate X with the value C=2. If you then run the
#' model with raster 2, the model will predict high-probability of X at cells
#' with D=2, which is numerically correct but the level names don't match. The
#' \code{setRasterLevels()} function solves this problem by ensuring that any
#' level names shared between two factor rasters will also share the same
#' integer ID.
#'
#' @param raster The factor \code{SpatRast} object to apply the given
#' \code{cats} to.
#' @param cats A \code{data.frame} that includes a "category" character column
#' (the output of \code{terra::cats()}).
#'
#' @examples
#' \donttest{
#' library(TerrainWorksUtils)
#'
#' r1 <- terra::rast("C:/Work/netmapdata/pack_forest/PF_landtype1.tif")
#' r2 <- terra::rast("C:/Work/netmapdata/pack_forest/PF_landtype2.tif")
#'
#' r1 <- TerrainWorksUtils::fixFactorRaster(r1)
#' r2 <- TerrainWorksUtils::fixFactorRaster(r2)
#'
#' terra::plot(r1)
#' terra::plot(r2)
#'
#' cats <- terra::cats(r1)[[1]]
#' r2New <- applyCats(r2, cats)
#'
#' terra::plot(r2New)
#' }

applyCats <- function(raster, cats) {

  # Store the original raster variable name for later
  varName <- names(raster)

  # Get the raster cats
  rasterCats <- terra::cats(raster)[[1]]

  # Find the common level names between the raster and the given cats
  sharedLevelNames <- intersect(cats$category, rasterCats$category)

  # Define shared-level ID translation matrix (from ID -> to ID)
  trans <- matrix(data <- rep(NA, 2 * length(sharedLevelNames)), ncol = 2)
  trans[,1] <- which(rasterCats$category %in% sharedLevelNames) - 1
  trans[,2] <- which(cats$category %in% sharedLevelNames) - 1

  # Re-classify input raster values according to the translation matrix
  raster <- terra::classify(raster, rcl = trans, othersNA = TRUE)

  # Set raster cats to those given
  terra::setCats(raster, layer = 1, value = cats$category)

  # Reset raster name back to its original
  names(raster) <- varName

  return(raster)
}
