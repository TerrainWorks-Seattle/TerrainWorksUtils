
#' Calculate proportions
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
#' @param data A data frame (ideally, a tibble) with probabilities, in this case, of landslide initiation. Must be coercible to a data table.
#' @param plot Change this value to FALSE if you do not want to produce the success rate curve plot.
#' @param prob_col The name of the column which contains the probabilities.
#' @param bins A vector with breaks for binning the cumulative probability sums. This is useful when combining multiple success rate curves, which might occur if a larger basin is divided into smaller sub-basin DEMs. If you do not want the curve to be binned, set this argument to NULL.
#'
#' @return A \code{tibble} with the success rate curve.
#' @export
#' @importFrom data.table data.table
calculate_proportions <- function(data,
                                  plot = FALSE,
                                  prob_col = "prob.pos",
                                  bins = NULL) {

  # Check parameters ----------------------------------------------

  if (is.null(data) | !is.data.frame(data)) {
    stop("Must supply a data frame.")
  }
  if (!(prob_col %in% names(data))) {
    msg <- paste0("Can't find column `", prob_col, "` in `data`.")
    stop(msg)
  }
  if (!is.logical(plot)) {
    stop("Must specify a logical value for `plot`.")
  }
  if (nrow(data) > 100000 & is.null(bins)) {
    warning("You have a lot of data - consider using bins to improve runtime.")
  }

  # Calculate proportions -----------------------------------------

  # get the data ready
  props <- as.data.table(data[c(prob_col)])
  names(props) <- c("prob")

  # sort by probability and combine cells with equal probabilities
  setorder(props, -"prob")
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

  if (!is.null(bins)) {

    if (!is.vector(bins)) {
      stop(paste0("`bins` must be a vector or list."))
    }

    # prepare for join
    bins <- data.table(bins)
    setkey(bins, bins)
    setkey(props, prob)
    setorder(props, "prob")

    # find the nearest values from proportions within 0.01
    props_binned <- props[bins, roll = -.01]
    setorder(props_binned, -"prob")

    # fill in missing values and values outside the range observed
    setnafill(props_binned, type = "locf")
    setnafill(props_binned, type= "const", fill = 0)

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

#' Combine area and probability proportions
#'
#' This function combines success rate curves into one curve. This is a weighted average, since the curves could represent different amounts of data.
#' The curves should be in the same format as produced by the calculate_proportions() function provided in this package and all must be the same length.
#'
#' Calling this function with one curve returns the same curve with nothing changed.
#'
#' @param ... Any number of success rate curves to be combined.
#' @param plot Change to FALSE if you do not want to produce a plot showing the curves.
#'
#' @return A \code{tibble} with the combined success rate curve.
#' @export
combine_proportions <- function(...,
                                plot = FALSE) {

  # Check parameters ----------------------------------------------

  curves <- list(...)
  # print(curves)

  if (length(curves) == 0 | is.null(curves)) {
    stop("Must provide one or more data frames.")
  } else if (is.null(curves[[1]])) {
    stop("Must provide one or more data frames.")
  } else if (length(curves) == 1 & !is.data.frame(curves[[1]])) {
    curves <- curves[[1]]
  }

  # print(curves)
  # print("now working with: ")
  # print(curves)
  # DO THESE CHECKS IN THE FOR LOOP BELOW

  # first <- curves[[1]]
  # for (cur in curves) {
  #   if (!all(c("prob_cumul", "area_cumul") %in% names(cur))) {
  #     stop("All curves must have columns \"prob_cumul\" and \"area_cumul\".")
  #   }
  #   if (nrow(cur) != nrow(first)) {
  #     stop("All curves must be the same length.")
  #   }
  # }
  if (!is.logical(plot)) {
    stop("Must specify a logical value for `plot`.")
  }

  # Combine curves ----------------------------------------------

  # if (is.list(curves[1])) {
  #   curves <- curves[1]
  # }

  if (length(curves) == 1) {
    if (is.null(curves[[1]][["prob_cumul"]])) {
      stop("Can't find `prob_cumul` in data frame.")
    }
    if (is.null(curves[[1]][["area_cumul"]])) {
      stop("Can't find `area_cumul` in data frame.")
    }

    sum_curve <- curves[[1]]
  } else {
    sum_curve <- tibble(
      prob = curves[[1]][["prob"]],
      prob_cumul = 0,
      area_cumul = 0,
      .rows = nrow(curves[[1]])
    )

    for (cur in curves) {
      # print(cur)
      if (is.null(cur[["prob_cumul"]])) {
        stop("Can't find `prob_cumul` in data frame.")
      }
      if (is.null(cur[["area_cumul"]])) {
        stop("Can't find `area_cumul` in data frame.")
      }
      if (nrow(sum_curve) != nrow(cur)) {
        msg <- paste0("Can't combine curves of different lengths.\n",
                      "x First curve is length ", nrow(sum_curve), "\n",
                      "x Second curve is length ", nrow(cur), "\n")
        stop(msg)
      }

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
        labs(title = "Success rate curve",
             x = "Proportion of area",
             y = "Proportion of predicted landslides")

  for (cur in curves) {
    p <- p + geom_line(mapping = aes(x = area_prop, y = prob_prop),
                       data = cur,
                       color = "gray")
  }

  p <- p + geom_line() + geom_point(size = 1)

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
success_rate_auc <- function(curve,
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

# both landslides and dem must be data frames with prob.pos columns
#
# it is assumed that the landslides data frame has one row for each landslide


#' Success Rate Curve
#'
#' This function builds a success rate curve using landslide data and a list of the DEMs. A prediction rate curve can be produced if the landslides given have not been included in the model used for predicted probabilities (test data).
#' It is up to the user to ensure that all relevant DEMs are included in the list - the extent of the study area will impact the curve.
#'
#' The DEM list should be filenames that lead to .csv files with predictions for each DEM. The .csv files should be in a format similar to what is produced in a accompanying function, with a probability output column from a model.
#'
#'
#'
#' @param landslides A data frame of every landslide initiation point, with the predicted probability column.
#' @param dem_list A list of .csv files with predicted probabilities for each DEM.
#' @param plot Whether to produce a default plot of the success rate curve and the modeled probability proportions.
#' @param prob_col The name of the predicted probability column. Default is "prob.pos"
#' @param bins
#'
#' @return A data frame with the success rate curve and the modeled probability curve.
#' @export
success_rate_curve <- function(landslides,
                     dem_list,
                     plot = TRUE,
                     prob_col = "prob.pos",
                     bins = NULL,
                     quiet = TRUE) {


  #### OR ####

  # output should be:

  #  x | y | prob | observed_prop | modeled_prop | area_prop
  #  ----------------------------------------------------------
  #    |   |      |               |              |
  #
  # column descriptions:
  #
  #   prob: has corresponding probabilities in descending order. highest probabilities come first, to align this with the literature.
  #   observed_landslides: has the cumulative proportion of observed landslides that can be found at each probability (and higher)
  #   modeled_landslides: has the cumulative proportion of modeled landslides that can be found at each probability (integrated probability)
  #   area_prop: the proportion of total area that can be found at each probability (and higher)
  #

  # create SR curve with landslides --- area_prop is useless
  # create SR curve with modeled probability --- prob_prop is modeled_landslides and area_prop is area_prop
  #         - do this for all dems that contain the landslide initiation points.
  # join datasets by probability (nearest)

  # this method would rely on the user to correctly identify which dem's should be included.
  # perhaps i will write a wrapper function that will do this for us, given an list of dems as a text file (like Dan tends to do)

  # -------------- PSEUDOCODE -------------------

  # b = seq(0, 1, length = 100)
  # curve_obs <- calculate_proportionslandslides, bins = b)
  # output$observed_prop <- curve_obs$area_prop

  # for (every dem) {
  #   curve_dem_list <- append(curve_dem_list, calculate_proportionsdem, bins = b))
  # }
  # curve_dem_all <- combine_proportions(curve_dem_list)

  # output$modeled_prop <- curve_dem_all$prob_prop
  # output$area_prop <- curve_dem_all$area_prop

  # return output

  b = seq(1, 0, length = 4000)

  output <- tibble(
    prob = b,
    observed_prop = 0,
    modeled_prop = 0,
    area_prop = 0,
    .rows = nrow(b)
  )

  tic()
  curve_obs <- calculate_proportions(landslides, bins = b)
  output$observed_prop <- curve_obs$area_prop
  t <- toc(quiet = TRUE)

  if (!quiet) {

    print(paste0("Done with observed landslide curve: ", t$callback_msg))
  }
  # curve_dems <- list()

  # need to figure this out!!!!
  # x <- lapply(calculate_proportions(tibble(read.csv(dem)), bins = b))

  # print("Reading csv predictions. ")
  # dem_tibbles <- lapply(lapply(dem_list, read.csv), tibble)
  # print(dem_tibbles)
  # print("Calculating proportions on predictions")

  if (!is.data.frame(dem_list[[1]])) {
    dem_tibbles <- lapply(lapply(dems, read.csv), tibble)
  }


  tic()
  curve_dems <- lapply(dem_list, calculate_proportions, bins = b)
  t <- toc(quiet = TRUE)

  if (!quiet) {
      print(paste0("Done with modeled landslide curve: ", t$callback_msg))

  }




  # for (dem in dem_list[2:length(dem_list)]) {
  #   dem_pred <- tibble(read.csv(dem))
  #   curve_dems <- append(curve_dems, calculate_proportions(dem_pred, bins = b))
  #   # calculate_proportions(dem_pred, bins = b)
  # }
  curve_comb <- combine_proportions(curve_dems, plot = TRUE)

  output$modeled_prop <- curve_comb$prob_prop
  output$area_prop <- curve_comb$area_prop

  return(output)

}


