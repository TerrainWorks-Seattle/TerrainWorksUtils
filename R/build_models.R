


#' @title Build Random Forest Model
#'
#' @param training_data A dataframe containing columns with model inputs and a
#' column called "class" which describes each row as "positive" or "negative"
#'
#' @return A random forest model.
#' @export
#'
#' @examples
build_rf_model <- function(data = NULL,
                           plot_model = FALSE) {

  if (is.null(data$class)) {
    stop("Error: training data must contain a \"class\" column.")
  }

  # data should not have NaN values
  for (col in names(data)) {
    if (any(is.nan(data[, col]))) {
      stop(paste0("Error: data contains NaN values in column ",col))
    }
  }

  model <- randomForest(formula = as.factor(class)~.,
                        data = data)

  if (plot_model) {
    plot(model)
  }

}
