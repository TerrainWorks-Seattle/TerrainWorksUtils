createTrainingDataFromPolygons <- function(polygons,
                                           predictorsRaster,
                                           analysisRegionMask) {
  # sample points from within polygons
  # sample points from outside polygons
  #
}

#' @title Create Training Data from Points
#'
#' @description Given an input dataset and set of positive points, generate
#' randomly sampled negative points and extract predictor values for positive
#' and negative points.
#'
#' @return a data.frame with values for positive and negative points
#'
#' @param positivePoints SpatVector with locations of all points with positive
#' class
#' @param predictorsRaster SpatRaster with a layer for each predictor variable
#' @param analysisRegionMask SpatRaster with non-NA values everywhere that
#' points can be sampled from. All locations that should be excluded from
#' sampling should be NA. If NULL, all cells which are non-NA for all layers
#' of the predictorsRaster will be used.
#' @param bufferRadius minimum possible distance between a positive and negative
#' point
#' @param negativeProportion Proportion of negative points to be generated
#' compared to number of positive points
#' @param extractionMethod Method to use for selecting negative points from
#' within randomly sampled buffers, of the form \code{c(method, layer)}. \code{
#' method} can be either "max" or "min", and \code{layer} is the name of one
#' of the layers in \code{predictorsRaster}.
#' If not NA, a buffer is drawn around all negative points, and a point is
#' selected from within the buffer according to extractionMethod. For example:
#' \code{c("max", "grad_15")} indicates to use the cell with the maximum
#' value for the layer "grad_15" within the buffer.
#'
#' @export
createTrainingDataFromPoints <- function(positivePoints,
                                         predictorsRaster,
                                         analysisRegionMask = NULL,
                                         bufferRadius = 15,
                                         negativeProportion = 1,
                                         extractionMethod = "centroid",
                                         extractionLayer = NULL) {
  allPoints <- sampleNegativePoints(
    positivePoints = positivePoints,
    analysisRegion = analysisRegionMask,
    buffer = TRUE,
    bufferRadius = bufferRadius,
    negativeProportion = negativeProportion
  )

  extractValues(
    raster = predictorsRaster,
    points = allPoints,
    extractionMethod = extractionMethod,
    extractionLayer = extractionLayer,
    xy = FALSE
  )
}

#' @export
#' @title Sample negative points
#'
#' @description randomly sample points from an analysis area to create a
#' dataset of negative points given a set of positive points
#'
#' @param positivePoints SpatVector with locations of all points with positive
#' class
#' @param analysisRegionMask SpatRaster with non-NA values everywhere that
#' points can be sampled from. All locations that should be excluded from
#' sampling should be NA. If NULL, all cells which are non-NA for all layers
#' of the predictorsRaster will be used.
#' @param buffer Return buffer around points?
#' @param bufferRadius minimum possible distance between a positive and negative
#' point
#' @param negativeProportion Proportion of negative points to be generated
#' compared to number of positive points
#'
#' @return SpatVector with positive and negative points, wtih a field
#' \code{"class"} indicating whether each point is positive or negative
sampleNegativePoints <- function(positivePoints,
                                 analysisRegion,
                                 buffer = TRUE,
                                 bufferRadius = 15,
                                 negativeProportion = 1) {

  # Check params
  if (terra::geomtype(positivePoints) != "points") {
    stop("positivePoints must be points!")
  }
  if (class(analysisRegion) != "SpatRaster") {
    stop("analysisRegion must be raster!")
  }
  if (crs(positivePoints) != crs(analysisRegion)) {
    stop("positivePoints and analysisRegion crs does not match!")
  }


  positivePoints$class <- "positive"
  # Buffer positive points
  positiveBuffers <- terra::buffer(positivePoints,
    width = bufferRadius * 2
  )

  # remove remove positive buffers from analysisRegion
  negativeRegion <- terra::deepcopy(analysisRegion)
  positiveCellIndices <- terra::extract(negativeRegion,
    positiveBuffers,
    cells = TRUE
  )$cell
  negativeRegion[positiveCellIndices] <- NA

  # Determine how many to generate
  negativeBuffersCount <- ceiling(length(positivePoints) * negativeProportion)

  # Create negativePoints
  negativePoints <- samplePoints(
    count = negativeBuffersCount,
    region = negativeRegion,
    buffer = buffer,
    radius = bufferRadius
  )

  negativePoints$class <- "negative"

  if (buffer) {
    return(rbind(positiveBuffers, negativePoints))
  } else {
    return(rbind(positivePoints, negativePoints))
  }
}


#' @export
#'
#' @title Sample Points from a region
#'
#' @description Generates a SpatVector of points in a given
#' area.
#'
#' @details This extends \code{terra::spatSample()} to generate the correct
#' number of sampled points when the sample raster has a lot of NAs.
#'
#' @param count  The number of to sample
#' @param region A SpatRaster with NA cells anywhere a sampled point
#'               cannot be located
#' @param buffer Should points be returned with a buffer?
#' @param radius The radius of each buffer (ignored if buffer = FALSE)
#'
#' @return A SpatVector of buffers.
samplePoints <- function(count,
                         region,
                         buffer = TRUE,
                         radius = 15) {
  if (class(count) != "numeric") stop("count must be a number")
  if (class(region) != "SpatRaster") stop("region must be a raster")
  # NOTE: terra::spatSample() sometimes generates less than the requested
  # number of points if the sample raster has a lot of NAs. This is remedied
  # by repeatedly requesting a larger and larger number of points until
  # enough have been generated, then subsetting those for the correct amount.

  currentRequest <- count
  hasGeneratedEnough <- FALSE

  while (!hasGeneratedEnough) {
    # Sample points anywhere that fits initiation conditions but recorded no landslides

    # suppress warning about fewer cell returned than requested
    samplePoints <- withCallingHandlers(
      {
        terra::spatSample(
          region,
          size = currentRequest,
          na.rm = TRUE,
          as.points = TRUE,
          warn = FALSE
        )
      },
      warning = function(w) {
        if (conditionMessage(w) == "[spatSample] fewer cells returned than requested") {
          invokeRestart("muffleWarning")
        }
      }
    )

    # Test if enough non-initiation points have been generated
    if (length(samplePoints) >= count) {
      # If so, subset and exit loop
      samplePoints <- samplePoints[seq_len(count)]
      hasGeneratedEnough <- TRUE
    } else {
      # If not, double the next request, up to 10 times
      if (log2(currentRequest / count) < 10) {
        currentRequest <- currentRequest * 2
      } else {
        stop(
          "Cannot generate enough points. ",
          "Try a larger analysisRegion or setting a smaller bufferRadius."
        )
      }
    }
  }

  # Create a buffer around each non-initiation point
  if (isTRUE(buffer) & radius > 0) {
    return(terra::buffer(samplePoints, width = radius))
  } else {
    return(samplePoints)
  }
}

#' @title Extract buffer values
#'
#' @export
#'
#' @description Extracts values for all layers of a raster for a set of points
#' or polygons. If
#'
#' @param raster A SpatRaster of explanatory variables with a layer for
#' each variable to extract.
#' @param points SpatVector of points to use for extraction. If
#' polygons, the point chosen for extraction will be determined via the
#' extractionMethod parameter.
#' @param extractionMethod Method to use for extracting values from each point:
#'  "all", "centroid", "max", or "min". Ignored if extractionPoints is not
#'  polygon
#' @param extractionLayer Layer to use for extracting value. Ignored if
#' extractionMethod = "centroid". Ignored if extractionMethod is "all" or
#' "centroid" or if extractionPoints is not polygon.
#' @param xy Return coordinates of cells?
#' @param na.rm Remove any cells with NA values?
#'
#' @return A dataframe of extracted raster values with an additional "class"
#' column.
extractValues <- function(raster,
                          points,
                          extractionMethod = "all",
                          extractionLayer = NULL,
                          xy = TRUE,
                          na.rm = TRUE) {

  # Check parameters
  if (class(raster) != "SpatRaster") stop("raster must be a raster!")
  if (class(points) != "SpatVector") stop("points must be points or polygon")
  if (!extractionMethod %in% c("all", "centroid", "max", "min")) {
    stop("extractionMethod must be 'all', 'centroid', 'max' or 'min'")
  }
  if (extractionMethod %in% c("max", "min")) {
    if (is.null("extractionLayer")) {
      stop(
        "extractionLayer must be specified when extractionMethod is ",
        extractionMethod
      )
    }
    if (!extractionLayer %in% names(raster)) {
      stop("could not find ", extractionLayer, " in raster")
    }
  }

  if (extractionMethod == "all" | terra::geomtype(points) == "points") {
    values <- terra::extract(raster, points, xy = xy)
  } else {

    # Subset if a different buffer extraction method was requested
    if (extractionMethod == "centroid") {

      # Find center points of polygons
      centerPoints <- terra::centroids(points)

      # Extract values from cells containing center points
      values <- terra::extract(raster, centerPoints, xy = xy)
    } else {

      # First extract all values from polygons
      values <- terra::extract(raster, points, xy = xy)

      # Formula to group entries by buffer and return requested variable value
      formula <- as.formula(paste(extractionLayer, "~", "ID"))

      # For each buffer, keep the entry with the "fun" variable value
      values <- merge(
        stats::aggregate(formula, extractionMethod, data = values, na.rm = na.rm),
        values,
        sort = FALSE
      )
      # If any ties (eg multiple cells have max value) choose whichever
      # happens to be listed frist.
      values <- values[!duplicated(values$ID), ]
    }
  }


  # Add any other variables from points back into extraction values
  for (var in setdiff(names(points), names(values))) {
    values[[var]] <- points[[var]][[var]]
  }

  # Remove the "ID" column
  values$ID <- NULL


  if (na.rm) {
    # Filter out entries with NA values
    values <- na.omit(values)
  }

  return(values)
}

