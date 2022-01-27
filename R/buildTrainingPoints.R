#' Build training points from polygons
#'
#' @param regionPoly SpatVector polygon defining the region from which
#' points can be sampled
#' @param wetlandPolysFile Name of wetland polygons file
#' @param wetlandSampleRate Samples per km^2 of wetland
#' @param nonwetlandSampleRate Samples per km^2 of nonwetland
#' @param regionMrgin Width of region interior margin
#' @param wetlandTypes String: One or more of
#' \itemize{
#'   \item{Freshwater Forested/Shrub Wetland}
#'   \item{Freshwater Emergent Wetland}
#'   \item{Freshwater Pond}
#'   \item{Estuarine and Marine Wetland}
#'   \item{Riverine}
#'   \item{Lake}
#'   \item{Estuarine and Marine Deepwater}
#'   \item{Other}
#' }
#' @param trainingPointsFile Name of training points file
#'
build_training_points <- function(regionPolygon,
                                  wetlandPolys,
                                  wetlandSmpleRate,
                                  nonwetlandSampleRate,
                                  regionMargin,
                                  trainingPointsFile,
                                  wetlandTypes) {

  # Validate parameters --------------------------------------------------------

  if (class(regionPoly) != "SpatVector") stop("regionPoly must be SpatVector")
  if (length(regionPoly) != 1) stop("regionPoly must include only one polygon")

  if (class(wetlandPolys) != "SpatVector") stop("wetlandPolys must be SpatVector")

  if (!(is.numeric(wetlandSampleRate) && length(wetlandSampleRate) == 1)) {
    stop("Wetland sample rate must be a single numeric value")
  }

  if (!(is.numeric(nonwetlandSampleRate) && length(nonwetlandSampleRate) == 1)) {
    stop("Non-wetland sample rate must be a single numeric value")
  }

  # Prepare the region ---------------------------------------------------------


  # Shrink region by applying an interior margin. This ensures that training
  # points will not be sampled near the region's edges
  if (regionMargin != 0) {
    regionPoly <- terra::buffer(regionPoly, width = -abs(regionMargin))
  }

  # Sample wetlands ------------------------------------------------------------

  wetlandPolys <- wetlandPolys[wetlandPolys$WETLAND_TY %in% wetlandTypes]
  if (length(wetlandPolys) == 0) {
    stop("No wetlands to sample")
  }

  # Crop the wetland polygons to the region
  wetlandPolys <- terra::project(wetlandPolys, regionPoly)
  wetlandPolys <- terra::crop(wetlandPolys, regionPoly)

  # Sample wetland regions
  wetlandCoords <- samplePolys(wetlandPolys, wetlandSampleRate)

  # Sample non-wetlands --------------------------------------------------------

  # Determine non-wetland polygon(s) by subtracting wetland polygons from the
  # whole region
  nonwetlandPolys <- terra::erase(regionPoly, wetlandPolys)

  # Sample non-wetland regions
  nonwetlandCoords <- samplePolys(nonwetlandPolys, nonwetlandSampleRate)

  # Combine sample points ------------------------------------------------------

  trainingCoords <- rbind(wetlandCoords, nonwetlandCoords)
  trainingAtts <- data.frame(class = factor(c(rep("WET", nrow(wetlandCoords)), rep("UPL", nrow(nonwetlandCoords)))))
  trainingPoints <- terra::vect(trainingCoords, atts = trainingAtts, crs = terra::crs(regionPoly))

  # terra::polys(regionPoly, lty = 2)
  # terra::polys(wetlandPolys, border = "cyan")
  # terra::points(trainingPoints, col = c(rep("blue", nrow(wetlandCoords)), rep("red", nrow(nonwetlandCoords))))

  return(trainingPoints)
}
