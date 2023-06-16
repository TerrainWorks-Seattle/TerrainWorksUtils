
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
#' @return A \code{tibble} with the success rate curve.
#' @export
#' @importFrom data.table data.table
srcurve <- function(data,
                    plot = TRUE,
                    prob_col = "prob.pos",
                    bins = NULL) {

  # Check parameters ----------------------------------------------

  if (is.null(data)) {
    stop("No data supplied.")
  }
  if (!(prob_col %in% names(data))) {
    stop("prob_col must exist in data or an alternate column name must be specified.")
  }
  if (!is.numeric(data[[prob_col]])) {
    stop("Data must be numeric.")
  }
  if (!is.logical(plot)) {
    stop("Plot variable must be TRUE/FALSE.")
  }
  if (!is.null(bins) && length(bins) > nrow(data)) {
    stop("Number of bins is greater than the data provided.")
  }
  if (nrow(data) > 100000 && is.null(bins)) {
    warning("You have a lot of data - consider using bins to improve runtime.")
  }

  # Calculate proportions -----------------------------------------

  # get the data ready
  props <- as.data.table(data[c(prob_col)])
  names(props) <- c("prob")

  # sort by probability and combine cells with equal probabilities
  setorder(props, cols = "prob")
  props <- props[, .N, by = prob]

  # calculate the cumulative sums of area and probabilities
  props$prob_cumul <- cumsum(props$prob * props$N)
  props$area_cumul <- cumsum(props$N)

  prob_sum <- max(props$prob_cumul)
  area_sum <- max(props$area_cumul)

  props$prob_prop <- props$prob_cumul / prob_sum
  props$area_prop <- props$area_cumul / area_sum

  setattr(props, "sorted", "prob")

  to_return <- as_tibble(props)

  # Bin data ------------------------------------------------------

  if (!is.null(bins) && is.vector(bins) && length(bins) <= nrow(props)) {
    props_binned <- props[data.table(bins), roll = "nearest"]
    to_return <- as_tibble(props_binned)
  }

  # Plot curve ----------------------------------------------------

  to_plot <- to_return
  if (to_plot[1, "prob_prop"] != 0 && to_plot[1, "area_prop"] != 0) {
    to_plot <- add_row(to_plot, prob_prop = 0, area_prop = 0, .before = 1)
  }

  p <- ggplot(to_plot, aes(x = area_prop, y = prob_prop)) +
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
#' Calling this function with one curve returns the same curve with nothing changed.
#'
#' @param ... Any number of success rate curves to be combined.
#' @param plot Change to FALSE if you do not want to produce a plot showing the curves.
#'
#' @return A \code{tibble} with the combined success rate curve.
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
  if (!is.logical(plot)) {
    stop("Plot variable must be TRUE/FALSE.")
  }

  # Combine curves ----------------------------------------------

  if (length(curves) == 1) {
    sum_curve <- curves[[1]]
  } else {
    sum_curve <- tibble(
      prob = first[["prob"]],
      prob_cumul = 0,
      area_cumul = 0,
      .rows = nrow(first)
    )

    for (cur in curves) {
      sum_curve[["prob_cumul"]] <- sum_curve[["prob_cumul"]] + cur[["prob_cumul"]]
      sum_curve[["area_cumul"]] <- sum_curve[["area_cumul"]] + cur[["area_cumul"]]
    }

    prob_sum <- max(sum_curve$prob_cumul)
    area_sum <- max(sum_curve$area_cumul)

    sum_curve$prob_prop <- sum_curve$prob_cumul / prob_sum
    sum_curve$area_prop <- sum_curve$area_cumul / area_sum
  }


  # Plot curve ----------------------------------------------------

  to_plot <- sum_curve
  if (to_plot[1, "prob_prop"] != 0 && to_plot[1, "area_prop"] != 0) {
    to_plot <- add_row(to_plot, prob_prop = 0, area_prop = 0, .before = 1)
  }

  p <- ggplot(to_plot, aes(x = area_prop, y = prob_prop)) +
        geom_line() +
        # geom_point() +
        labs(title = "Success rate curve",
             x = "Proportion of area",
             y = "Proportion of predicted landslides")

  for (cur in curves) {
    p <- p + geom_line(mapping = aes(x = area_prop, y = prob_prop),
                       data = cur,
                       color = "gray")
  }

  if (plot) {
    print(p)
  }

  return(sum_curve)

}


#' Area under the success rate curve
#'
#' Calculates the area under the success rate curve as a single value.
#' This is a useful quantification of the success rate curve metric for easily comparing models and integrating this with a modeling ecosystem.
#'
#' @param curve The success rate curve. It should have a column "area_prop" and a column "prob_prop"
#' @param integral_type How the integral is calculated. Can be left, right, or trap.
#'
#' @return The area under the curve.
#' @export
#'
srcurve_auc <- function(curve,
                        integral_type = "trap") {

  # Check parameters ----------------------------------------------------------

  if (is.null(curve)) {
    stop("No data provided.")
  }
  if (!all(c("area_prop", "prob_prop") %in% names(curve))) {
    stop("Curve must be a success rate curve with area and probability proportions.")
  }
  if (!(integral_type %in% c("left", "right", "trap"))) {
    stop("Integral type must be \"left\", \"right\", or \"trap\"")
  }

  # Calculate area under the curve --------------------------------------------

  auc_sum <- 0

  x <- curve[["area_prop"]]
  y <- curve[["prob_prop"]]

  for (i in 1:(nrow(curve) - 1)) {
    dx <- (x[i + 1] - x[i])
    if (integral_type == "left") {
      auc_sum <- auc_sum + (y[i] * dx)
    } else if (integral_type == "right") {
      auc_sum <- auc_sum + (y[i + 1] * dx)
    } else if (integral_type == "trap") {
      auc_sum <- auc_sum + (mean(c(y[i], y[i + 1])) * dx)
    }
  }

  return(auc_sum)
}

