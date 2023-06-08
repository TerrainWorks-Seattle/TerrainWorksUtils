
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
#'
#' @param data A data frame (ideally, a tibble) with probabilities, in this case, of landslide initiation.
#' @param plot Change this value to FALSE if you do not want to produce the success rate curve plot.
#' @param prob_col The name of the column which contains the probabilities.
#' @param bins A vector with breaks for binning the cumulative probability sums. This is useful when combining multiple success rate curves, which might occur if a larger basin is divided into smaller sub-basin DEMs. If you do not want the curve to be binned, set this argument to NULL.
#'
#' @return A tibble with the success rate curve.
#' @export
#' @importFrom data.table data.table
srcurve <- function(data,
                    plot = TRUE,
                    prob_col = "prob.pos",
                    bins = seq(0, 1, length = 20)) {

  # Check parameters ----------------------------------------------

  if (is.null(data)) {
    stop("No data supplied.")
  }
  if (!(prob_col %in% names(data))) {
    stop("prob_col must exist in data or an alternate column name must be specified.")
  }
  if (!is.logical(plot)) {
    stop("Plot variable must be TRUE/FALSE.")
  }

  # Calculate proportions -----------------------------------------

  # order the data by probability
  if (!is_tibble(data)) {
    data <- as_tibble(data)
  }
  props <- data[c(prob_col)]
  names(props) <- c("prob")
  props <- arrange(props, prob)

  # calculate the cumulative sums of area and probabilities
  props$prob_cumul <- cumsum(props$prob)
  props$area_cumul <- seq(1, length(props[[1]]))

  prob_sum <- max(props$prob_cumul)
  area_sum <- max(props$area_cumul)

  props$prob_prop <- props$prob_cumul / prob_sum
  props$area_prop <- props$area_cumul / area_sum

  props_table <- as.data.table(props)
  setattr(props_table, "sorted", "prob")

  to_return <- props

  # Bin data ------------------------------------------------------

  if (!is.null(bins) && is.vector(bins)) {
    props_binned <- props_table[data.table(bins), roll = "nearest"]
    to_return <- as_tibble(props_binned)
  }

  # Plot curve ----------------------------------------------------

  p <- ggplot(to_return, aes(x = area_prop, y = prob_prop)) +
         geom_line() +
         geom_point() +
         labs(title = "Success rate curve",
              x = "Proportion of area",
              y = "Proportion of predicted landslides")

  if (plot) {
    print(p)
  }

  return(to_return)
}

#' Combine success rate curves
#'
#' This function combines success rate curves into one curve. This is a weighted average, since the curves could represent different amounts of data.
#' The curves should be in the same format as produced by the srcurve() function provided in this package and all must be the same length.
#'
#' @param ... Any number of success rate curves to be combined.
#' @param plot Change to FALSE if you do not want to produce a plot showing the curves.
#'
#' @return A tibble with the combined success rate curve.
#' @export
srcurve_combine <- function(...,
                            plot = TRUE) {

  # Check parameters ----------------------------------------------

  curves <- list(...)
  if (length(curves) == 0) {
    stop("No data provided.")
  }
  first <- curves[[1]]
  for (cur in curves) {
    if (!all(c("prob_cumul", "area_cumul") %in% names(cur))) {
      stop("All curves must have columns \"prob_cumul\" and \"area_cumul\".")
    }
    if (nrow(cur) != nrow(first)) {
      stop("All curves must be the same length.")
    }
  }

  # Combine curves ----------------------------------------------

  sum_curve <- tibble(
    prob_cumul = 0,
    area_cumul = 0,
    .rows = nrow(first)
  )

  for (cur in samp_list) {
    sum_curve[["prob_cumul"]] <- sum_curve[["prob_cumul"]] + cur[["prob_cumul"]]
    sum_curve[["area_cumul"]] <- sum_curve[["area_cumul"]] + cur[["area_cumul"]]
  }

  prob_sum <- max(sum_curve$prob_cumul)
  area_sum <- max(sum_curve$area_cumul)

  sum_curve$prob_prop <- sum_curve$prob_cumul / prob_sum
  sum_curve$area_prop <- sum_curve$area_cumul / area_sum

  print(sum_curve)

  # Plot curve ----------------------------------------------------

  p <- ggplot(sum_curve, aes(x = area_prop, y = prob_prop)) +
        geom_line() +
        geom_point() +
        labs(title = "Success rate curve",
             x = "Proportion of area",
             y = "Proportion of predicted landslides")

  if (plot) {
    print(p)
  }

}
