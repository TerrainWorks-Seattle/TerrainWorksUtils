
#' Generate a success rate curve
#'
#' This code takes a data frame of predicted landslide initiation values and calculates a success rate curve.
#'
#' This calculation is done by ordering all observations in ascending order and getting cumulative sums for both the probabilities and the area.
#' These sums are then divided by the total values for probability and area, leaving us with cumulative proportion values.
#'
#' This function support binning, which allow multiple success rate curves to be combined.
#'
#' The difference between the success rate curve and the prediction rate curve is in the data that is used to generate it.
#' Success rate curves are generated using study areas.
#' Prediction rate curves are generated on entire prediction areas - or new data.
#'
#' @param data
#' @param bins
#'
#' @return
#' @export
#'
#' @examples
success_rate_curve <- function(data,
                               prob_col = "prob.pos",
                               bins = NULL) {

  if (is.null(data$prob_col)){
    stop(" ")
  }

  # order the data by probability
  proportions <- as.data.table(data)
  setorder(proportions, cols = prob_col)

  # calculate the cumulative sums of area and probabilities
  proportions$cumul_prob <- cumsum(proportions$prob.pos)
  prob_sum <- max(proportions$prob.cumsum)
  proportions$cumul_area <- seq(1, length(proportions$x))
  area_sum <- max(proportions$area.cumsum)

  setattr(proportions, "sorted", "prob.pos")

  binned_prop <- proportions[J(bins$probability), roll = "nearest"]
  cumul_prop$area <- cumul_prop$area + binned_prop$area.cumsum
  cumul_prop$prob <- cumul_prop$prob + binned_prop$prob.cumsum


  return(data)
}
