#' @export
#'
#' @title Calculate ROC Statistics
#'
#' @description Calculates ROC statistics from a set of wetland-classified data
#' and a wetland probability raster. ROC statistics include: ROC, area under ROC
#' curve (AUC), precision, accuracy.
#'
#' @param wetProbRaster A \code{SpatRaster} object where each cell value
#' represents the probability of a wetland being there.
#' @param classifiedPoints A \code{??} object of points which are classified as
#' either "wetland" or "non-wetland" with the class names \code{wetlandClass}
#' and \code{nonWetlandClass}.
#' @param wetlandClass Name of the class indicating wetlands.
#' @param nonWetlandClass Name of the class indicating non-wetlands.
#'
#' @return A list of ROC statistic objects and values.
#'
#' @examples
#' \donttest{
#' library(TerrainWorksUtils)
#'
#' wetProbRaster <- terra::rast("C:/Work/netmapdata/Puyallup/puy_prob.tif")
#'
#' allPoints <- terra::vect("C:/Work/netmapdata/Puyallup/wetlandPnts.shp")
#' classifiedPoints <- allPoints[, "NEWCLASS"]
#'
#' rocStats <- calcRocStats(wetProbRaster, classifiedPoints, "WET", "UPL")
#' }

calcRocStats <- function(
  wetProbRaster = NULL,
  classifiedPoints = NULL,
  wetlandClass = NULL,
  nonWetlandClass = NULL
) {

  # Validate parameters --------------------------------------------------------

  # Get test point values ------------------------------------------------------

  # Determine each point's wetland probability value
  pointValues <- terra::extract(wetProbRaster, classifiedPoints, method = "simple")

  # Include point classification values
  pointValues["class"] <- terra::values(classifiedPoints)

  # Remove point "ID" column
  pointValues <- pointValues[,-1]

  # Remove points with NA values
  pointValues <- stats::na.omit(pointValues)

  # Remove points that aren't classified as either "wetland" or "non-wetland"
  wetlandPointIndices <- pointValues$class == wetlandClass
  nonWetlandPointIndices <- pointValues$class == nonWetlandClass
  pointValues <- pointValues[wetlandPointIndices | nonWetlandPointIndices,]

  # Convert point class values (strings) to factors
  pointValues$class <- factor(pointValues$class)

  names(pointValues)[1] <- "wetProb"

  # Calculate ROC statistics ---------------------------------------------------

  # Consolidate points' wetland probabilities and classifications
  prediction <- ROCR::prediction(
    predictions = pointValues$wetProb,
    labels = pointValues$class,
    label.ordering = c(wetlandClass, nonWetlandClass)
  )

  # Calculate the ROC curve
  roc <- ROCR::performance(prediction, measure = "tpr", x.measure = "fpr")

  # Calculate the area under the ROC curve (AUC)
  auc <- ROCR::performance(prediction, measure = "auc")

  # Calculate the precision
  precision <- ROCR::performance(prediction, measure = "prec", x.measure = "rec")

  # Calculate the accuracy
  accuracy <- ROCR::performance(prediction, measure = "acc")

  idx <- which.max(methods::slot(precision, "y.values")[[1]])
  prbe <- methods::slot(precision, "y.values")[[1]][idx]
  precisionCutoff <- methods::slot(precision, "x.values")[[1]][idx]

  idx <- which.max(methods::slot(accuracy, "y.values")[[1]])
  maxAccuracy <- methods::slot(accuracy, "y.values")[[1]][idx]
  accuracyCutoff <- methods::slot(accuracy, "x.values")[[1]][idx]

  # Return ---------------------------------------------------------------------

  # Store ROC stats in a list
  rocStats <- list(
    roc = roc,
    auc = auc,
    precision = precision,
    accuracy = accuracy,
    prbe = prbe,
    precisionCutoff = precisionCutoff,
    maxAccuracy = maxAccuracy,
    accuracyCutoff = accuracyCutoff
  )

  return(rocStats)

}
