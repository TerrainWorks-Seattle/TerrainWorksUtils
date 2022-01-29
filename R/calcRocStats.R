#' @export
#'
#' @title Calculate ROC Statistics
#'
#' @description Calculates ROC statistics for a set of classes and their
#' probabilities data
#' and a wetland probability raster. ROC statistics include: ROC, area under ROC
#' curve (AUC), precision, accuracy.
#'
#' @param classes A character vector of classifications.
#' @param probs A numeric vector of probability values corresponding to the
#' elements of \code{classes}.
#' @param posClass Name of the positive class.
#' @param negClass Name of the negative class.
#'
#' @return A list of ROC statistic objects and values.
#'
#' @examples
#' \donttest{
#' library(TerrainWorksUtils)
#'
#' # Generate test data with a bit of class probability overlap
#' classes <- c(rep("WET", 40), rep("UPL", 60))
#' probs <- c(runif(40, 0.4, 1.0), runif(60, 0.0, 0.6))
#'
#' rocStats <- calcRocStats(classes, probs, "WET", "UPL")
#' }
#'
calcRocStats <- function(classes = NULL,
                         probs = NULL,
                         posClass = NULL,
                         negClass = NULL) {

  # Validate parameters --------------------------------------------------------

  # Calculate ROC statistics ---------------------------------------------------

  # Consolidate points' wetland probabilities and classifications
  prediction <- ROCR::prediction(
    predictions = probs,
    labels = classes,
    label.ordering = c(negClass, posClass)
  )

  # Calculate the ROC curve
  roc <- ROCR::performance(prediction, measure = "tpr", x.measure = "fpr")

  # Calculate the area under the ROC curve (AUC)
  auc <- ROCR::performance(prediction, measure = "auc")

  # Calculate the max precision and its cutoff point
  precision <- ROCR::performance(prediction, measure = "prec", x.measure = "rec")
  maxPrecisionIndex <- which.max(methods::slot(precision, "y.values")[[1]])
  prbe <- methods::slot(precision, "y.values")[[1]][maxPrecisionIndex]
  maxPrecisionCutoff <- methods::slot(precision, "x.values")[[1]][maxPrecisionIndex]

  # Calculate the max accuracy and its cutoff point
  accuracy <- ROCR::performance(prediction, measure = "acc")
  maxAccuracyIndex <- which.max(methods::slot(accuracy, "y.values")[[1]])
  maxAccuracy <- methods::slot(accuracy, "y.values")[[1]][maxAccuracyIndex]
  maxAccuracyCutoff <- methods::slot(accuracy, "x.values")[[1]][maxAccuracyIndex]

  # Return ---------------------------------------------------------------------

  # Store ROC stats in a list
  rocStats <- list(
    roc = roc,
    precision = precision,
    accuracy = accuracy,
    prbe = prbe,
    maxPrecisionCutoff = maxPrecisionCutoff,
    maxAccuracy = maxAccuracy,
    maxAccuracyCutoff = maxAccuracyCutoff,
    auc = auc
  )

  return(rocStats)
}
