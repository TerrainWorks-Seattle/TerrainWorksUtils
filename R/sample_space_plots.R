
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
#' @return
#' @export
#'
#' @examples
#'
#' TODO: edit to work with training data as points in addition to data frame.
#' TODO: edit y-axis to not cut anything off.
sample_point_plots <- function(training_data,
                               parameters) {

  # separate out the positive and negative data
  positive_data <- training_data[which(training_data$class == "positive"), ]
  negative_data <- training_data[which(training_data$class == "negative"), ]

  # define visual aspects of the plot
  num_parameters = length(parameters)
  par(mfrow = c(ceiling(num_parameters / 3), 3))
  c1 <- rgb(0, 0, 0, max = 255, alpha = 80)
  c2 <- rgb(227, 60, 57, max = 255, alpha = 80)

  for (p in parameters) {
    # use consistent buckets for the histograms
    buckets = seq(min(training_data[, p], na.rm = TRUE),
                  max(training_data[, p], na.rm = TRUE),
                  length.out = 20)

    h_pos <- hist(positive_data[, p], breaks = buckets, plot = FALSE)
    h_neg <- hist(negative_data[, p], breaks = buckets, plot = FALSE)
    plot(h_neg, xlab = p,
         main = paste0("Distribution of ", p),
         col = c2)
    plot(h_pos, col = c1,
         add = TRUE)
  }

}

sample_space_plots <- function(positive_region,
                               negative_region) {


}
