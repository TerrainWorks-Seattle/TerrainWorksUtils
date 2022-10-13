

#' @title Build a Random Forest model using cross-validation.
#'
#' @description This function acts as a wrapper for the train() function from
#' the \code{caret} package for building a classification Random Forest model
#' using the \code{randomForest} package.
#'
#' The train function will compare models for a few values of tuning parameters.
#' For the randomForest model, the tuning parameter is \code{mtry}, which is the
#' number of variables randomly sampled as candidates at each split. For this
#' purpose, we use specificity, sensitivity, and area under the ROC curve (ROC)
#' as evaluating parameters.
#'
#' \code{caret} generates a report which is printed, in addition to a plot
#' of the model performance across the tuning parameters and a report of the
#' time it took to generate the model.
#'
#' @param data The dataset to be used in model-building. Columns must contain
#' the predictor variables and a column called "class" that labels each row as
#' positive or negative.
#' @param seed A number setting the random seed to allow reproducible results.
#' Default is 123.
#' @param ctrl_method Can be either \code{"cv"} for cross-validation without
#' repeats or \code{"repeatedcv"}.
#' @param k The number of folds, or divisions of the data. Default is 5, which
#' produces a 80/20 ratio between training data and testing data for each
#' repetition.
#' @param repeats Number of times to repeat cross validation. The folds change
#' with each repeat. If \code{ctrl_method} is \code{"cv"}, this parameter is
#' ignored.
#'
#'
#' @return Nothing
#' @export
#'
build_k_fold_rf_model <- function(data = NULL,
                               seed = 123,
                               ctrl_method = "repeatedcv",
                               k = 5,
                               repeats = 3) {

  set.seed(seed)

  #set control parameters.
  ctrl <- trainControl(method = ctrl_method,
                       number = k,
                       repeats = repeats,
                       # use AUC, specificity and sensitivity as metrics.
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)

  # note: use parameter tuneLength to increase the range of tuning parameters
  # tried. can't be more than (number of predictors - 1).
  time <- system.time(model <- train(form = as.factor(class) ~ .,
                                     data = data,
                                     trControl = ctrl,
                                     method = "rf",
                                     metric = "ROC"))

  print(model)
  print("Time spent generating the model:")
  print(time)
  plot(model)
}



#' @title Build Random Forest Model
#'
#' @param data A dataframe containing columns with model inputs and a
#' column called "class" which describes each row as "positive" or "negative"
#' @param plot_model Should a plot of the model be produced.
#'
#' @return A random forest model.
#' @export
#'
build_rf_model <- function(data = NULL,
                           plot_model = FALSE) {

  if (is.null(data$class)) {
    stop("Error: training data must contain a \"class\" column.")
  }

    for (col in names(data)) {
    if (any(is.nan(data[, col]))) {
      stop(paste0("Error: data contains NaN values in column ", col,
                  " and na_action is not specified."))
    }
  }

  model <- randomForest(formula = as.factor(class) ~ .,
                        data = data,
                        na.action = na_action)

  if (plot_model) {
    plot(model)
  }

  return(model)

}


