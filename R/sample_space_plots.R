
#' @title Plot parameters by sample points
#'
#' @description Generates histogram plot comparing the distribution of various
#' parameters across positive and negative classes of data. This function takes
#' training_data as a data frame.
#'
#' @param training_data A data frame with the data to be plotted. Must have at
#' least a column called "class" of type <chr> that contains a value "positive"
#' or "negative" for each row.
#' @param parameters A list of names of the parameters to plot. These must
#' correspond to columns in training_data
#'
#' @export
#'
# TODO: edit to work with training data as points in addition to data frame.
# TODO: edit y-axis to not cut anything off.
plot_metric_distribution <- function(training_data,
                                     parameters = NULL,
                                     plot_type = "hist") {

  if (!(plot_type %in% c("hist", "den"))) {
    stop("Plot type must be either \"hist\" (histogram) or \"den\" (density)")
  }

  # separate out the positive and negative data
  positive_data <- training_data[which(training_data$class == "positive"), ]
  negative_data <- training_data[which(training_data$class == "negative"), ]

  if (is.null(parameters)) {
    parameters = names(training_data)
  }

  # define visual aspects of the plot
  num_parameters = length(parameters)
  par(mfrow = c(ceiling(num_parameters / 3), 3))
  blk <- rgb(0, 0, 0, max = 255, alpha = 80)
  red <- rgb(227, 60, 57, max = 255, alpha = 80)

  if (plot_type == "hist") {
    for (p in parameters) {
      # use consistent buckets for the histograms
      buckets = seq(min(training_data[, p], na.rm = TRUE),
                    max(training_data[, p], na.rm = TRUE),
                    length.out = 20)

      if (plot_type == "hist") {
        h_pos <- hist(positive_data[, p], breaks = buckets, plot = FALSE)
        h_neg <- hist(negative_data[, p], breaks = buckets, plot = FALSE)
        plot(h_neg, xlab = p,
             main = paste0("Distribution of ", p),
             col = red)
        plot(h_pos, col = blk,
             add = TRUE)
      }
    }
    } else {
      for (p in parameters) {
        d_pos <- density(positive_data[, p], n = 1024)
        d_neg <- density(negative_data[, p], n = 1024)
        plot(d_pos,
             main = paste0("Distribution of ", p),
             xlab = p,
             col = c1)
        polygon(d_pos, col = c1)
        polygon(d_neg, col = c2)
      }
  }
}


