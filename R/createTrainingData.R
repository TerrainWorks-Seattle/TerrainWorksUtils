createTrainingDataFromPolygons <- function(
  polygons,
  predictorsRaster,
  analysisRegionMask
){
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
createTrainingDataFromPoints <- function(
  positivePoints,
  predictorsRaster,
  analysisRegionMask = NULL,
  bufferRadius = 15,
  negativeProportion = 1,
  extractionMethod = "center",
  extractionLayer = NULL
) {

  # Buffer positive points
  positiveBuffers <- terra::buffer(positivePoints,
                                   width = bufferRadius * 2)

  # remove remove positive buffers from analysisRegion
  negativeRegion <- terra::copy(analysisRegionMask)
  positiveCellIndices <- terra::extract(negativeRegion,
                                    positiveBuffers,
                                    cells = TRUE)$cell
  negativeRegion[positiveCellIndices] <- NA

  # Determine how many to generate
  negativeBuffersCount <- ceiling(length(positivePoints) * negativeProportion)

  # Create negativeBuffers
  negativeBuffers <- samplePoints(
    count = negativeBuffersCount,
    region = negativeRegion,
    buffer = TRUE,
    radius = bufferRadius
  )

  trainingData <- extractBufferValues(
    raster = predictorsRaster,

    extractionMethod = extractionMethod,
    extractionLayer = extractionLayer
  )

  # Remove coordinates from data
  coordsCols <- names(trainingData) %in% c("x", "y")
  trainingData <- trainingData[,!coordsCols]

  trainingData

}

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
sampleNegativePoints <- function(
  positivePoints,
  analysisRegion,
  buffer = TRUE,
  bufferRadius = 15,
  negativeProportion = 1
) {

  # Buffer positive points
  positiveBuffers <- terra::buffer(positivePoints,
                                   width = bufferRadius * 2)

  # remove remove positive buffers from analysisRegion
  negativeRegion <- terra::copy(analysisRegion)
  positiveCellIndices <- terra::extract(negativeRegion,
                                        positiveBuffers,
                                        cells = TRUE)$cell
  negativeRegion[positiveCellIndices] <- NA

  # Determine how many to generate
  negativeBuffersCount <- ceiling(length(positivePoints) * negativeProportion)

  # Create negativeBuffers
  negativeBuffers <- samplePoints(
    count = negativeBuffersCount,
    region = negativeRegion,
    buffer = TRUE,
    radius = bufferRadius
  )

  negativeBuffers$class <- "negative"
  positiveBuffers$class <- "positive"

  # TODO: Fails because crs doesn't match. WHY doesn't crs match??????
  allBuffers <- rbind(negativeBuffers, positiveBuffers)

  allBuffers
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

  # NOTE: terra::spatSample() sometimes generates less than the requested
  # number of points if the sample raster has a lot of NAs. This is remedied
  # by repeatedly requesting a larger and larger number of points until
  # enough have been generated, then subsetting those for the correct amount.

  currentRequest <- count
  hasGeneratedEnough <- FALSE

  while (!hasGeneratedEnough) {
    # Sample points anywhere that fits initiation conditions but recorded no landslides
    samplePoints <- terra::spatSample(
      region,
      size = currentRequest,
      na.rm = TRUE,
      as.points = TRUE,
      warn = FALSE
    )

    # Test if enough non-initiation points have been generated
    if (length(samplePoints) >= count) {
      # If so, subset and exit loop
      samplePoints <- samplePoints[seq_len(count)]
      hasGeneratedEnough <- TRUE
    } else {
      # If not, double the next request
      currentRequest <- currentRequest * 2
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
#' @description Extracts variable raster values within initiation and
#' non-initiation buffers.
#'
#' @param raster           A SpatRaster of explanatory variables
#' @param positiveBuffers      A SpatVector of buffers for each positive
#' @param negativeBuffers   A SpatVector of buffers for each negative
#' @param extractionMethod Method to use for extracting values from each buffer:
#'  "all", "center", "max", or "min"
#' @param extractionLayer Layer to use for extracting value. Ignored if
#' extractionMethod = "center"
#'
#' @return A dataframe of extracted raster values with an additional "class"
#' column.
extractBufferValues <- function(
  raster,
  positiveBuffers,
  negativeBuffers,
  extractionMethod = "all",
  extractionLayer = NULL) {

  # By default, extract all values from initiation and non-initiation buffers
  positiveValues <- terra::extract(raster, positiveBuffers, xy = TRUE)
  negativeValues <- terra::extract(raster, negativeBuffers, xy = TRUE)

  # Subset if a different buffer extraction method was requested
  if (extractionMethod == "center") {

    # Find center points of buffers
    positiveCenters <- terra::centroids(positiveBuffers)
    negativeCenters <- terra::centroids(negativeBuffers)

    # Get center point coordinates
    positiveCoords <- terra::geom(positiveCenters)[,c("x","y")]
    negativeCoords <- terra::geom(negativeCenters)[,c("x","y")]

    # Extract values from cells containing center points
    positiveValues <- terra::extract(raster, positiveCoords, xy = TRUE)
    negativeValues <- terra::extract(raster, negativeCoords, xy = TRUE)

  } else if (extractionMethod == "max") {

    positiveValues <- aggregateBufferValues(positiveValues, extractionLayer, max)
    negativeValues <- aggregateBufferValues(negativeValues,  extractionLayer, max)

  } else if (extractionMethod == "min") {

    positiveValues <- aggregateBufferValues(positiveValues, extractionLayer, max)
    negativeValues <- aggregateBufferValues(negativeValues,  extractionLayer, max)

  }

  # Assign a classification value to each entry
  positiveValues$class <- rep("positive", nrow(positiveValues))
  negativeValues$class <- rep("negative", nrow(negativeValues))

  # Combine positive entries into a single dataset
  dataset <- rbind(positiveValues, negativeValues)

  # Remove the "ID" column
  dataset$ID <- NULL

  # TODO: Remove cells that fall within positive AND negative buffers

  # Factor the classification variable values
  dataset$class <- factor(dataset$class)

  # Filter out entries with NA values
  dataset <- na.omit(dataset)

  return(dataset)

}


#' @title Aggregate buffer values
#'
#' @description Groups values by buffer and--for each buffer--keeps the entry
#' with the "fun" (min/max) variable value.
#'
#' @param values  Values extracted from buffers, including an "ID" column
#' @param varName Name pattern of the variable to aggregate by
#' @param fun     A function to aggregate buffer values by: max, min
#'
#' @return A dataframe of buffer values.

aggregateBufferValues <- function(values, varName, fun) {

  # Determine which variable to aggregate by based on provided name pattern
  valuesVarName <- names(values)[grepl(varName, names(values))][1]

  # Formula to group entries by buffer and return requested variable value
  fm <- as.formula(paste(valuesVarName, "~", "ID"))

  # For each buffer, keep the entry with the "fun" variable value
  aggregatedValues <- merge(
    aggregate(fm, max, data = values),
    values
  )

  return(aggregatedValues)

}
