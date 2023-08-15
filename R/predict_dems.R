
#' Predict multiple DEMs
#'
#' This function uses mapply() and predict_and_save to predict a full list of DEMs.
#' The predicting directory must contain a folder/file structure that follows the basins in the basin list. Each sub-directory should contain gradient, mean_curv, and pca files.
#'
#' Output is written to files in the same file structure.
#'
#' @param model A trained model to be used for prediction. Must have function $predict_newdata and $state$train_task$feature_names, as provided by mlr3 models.
#' @param pred_dir A directory where the basin input data can be found.
#' @param basin_list A list of basins and sub-basins that correspond to the file structure in pred_dir.
#' @param out_dir An folder to write output data. If this is not specified, a new folder called \code{"predicting_output/"} will be created in the same parent directory as the predicting input directory.
#' @param scale_vals A tibble containing values to center and scale the data. These values should match what was used for the training data. It is the users responsibility to make sure they are transforming the data correctly according to their workflow.
#' @param output The type of file to write the predictions to. The default is a .csv file. Multiple types can be specified.
#'
#' @return Nothing
#' @export
predict_multiple_dems <- function(model,
                                  pred_dir,
                                  basin_list,
                                  ...,
                                  out_dir = NULL) {
  params <- list(...)

  if (is.null(out_dir)) {
    out_dir <- paste0(dirname(pred_dir), "/predicting_output/")
  }

  mapply(predict_and_save,
                paste0(pred_dir, basin_list),
                paste0(out_dir, basin_list),
                MoreArgs = list(...,
                                model = model,
                                transform = make_quadratic_features))

}


#' Predict and save
#'
#' This function takes a folder with elevation derivative files for a basin and a model and saves the predictions to a .csv file.
#' The csv has columns "x" and "y" with coordinates, "prob.pos" with the model output, and all features used in the calculation of the model.
#'
#' An implementation note: Since we are using a classification model to derive the probability of classification into the positive class, we must change the rasters into a data frame, and use the $predict_newdata function. If we were interested in classification, we would be able use the mlr3spatiotemp::predict_spatial() function with raster inputs and raster outputs.
#'
#'
#' @param dir_in A directory which contains the necessary elevation derivative files for prediction.
#' @param file_out The name of the csv file to write results.
#' @param model A trained model. Must have function $predict_newdata and $state$train_task$feature_names. These are both provided by most mlr3 learners.
#' @param scale_vals The centering and scaling values used in training the model. The same values should be used when predicting data. The function expects a tibble with one column per feature and two values per feature. For example, \code{tibble(gradient = c(center_value, scale_value), mean_curv = c(center_value, scale_value))}.
#' @param transform A function that calculates any transformed columns needed for the model. For example, this package includes a function called "make_quadratic_features" which calculateds squared and interaction terms for all numeric features.
#'
#' @return A logical value.
#' @export
#'
#' @importFrom tictoc tic toc
#' @importFrom dplyr rename
predict_and_save <- function(dir_in,
                             file_out,
                             model,
                             ...,
                             scale_vals = NULL,
                             transform = function(x) x,
                             output = "tif",
                             write_covars = TRUE) {
  params <- list(...)
  # print(params)

  tic()
  # read files into a raster
  topo_files <- c(paste0("GRADIENT,", dir_in, "/gradient.flt"),
                  paste0("MEAN CURVATURE,", dir_in, "/mean_curv.flt"))
  # print(topo_files)
  topo_rast <- elev_deriv(rasters = topo_files)
  pca_file <- list.files(paste0(dir_in), "^pca.{1,8}\\.flt", full.names = TRUE)
  pca_rast <- contributing_area(raster = pca_file)

  # combine into one raster
  rasters <- c(topo_rast, pca_rast)
  names(rasters) <- c("gradient", "mean_curv", "pca")

  # convert to a data frame
  input_data <- as_tibble(as.data.frame(rasters, xy = TRUE))


  input_data$pca <- log(input_data$pca)
  input_data <- rename(input_data, ln_pca_k1_48 = pca) # comment this out when i change the names used for training the model.

  input_data <- transform(input_data)

  # print(scale_vals)

  # center and scale the data
  if (!is.null(scale_vals)) {
    for (col in names(scale_vals)) {
      if (col %in% names(input_data)) {
        input_data[[col]] <- ((input_data[[col]] - scale_vals[[col]][1]) / scale_vals[[col]][2])
      }
    }
  }

  # predict the data
  input_data$row_ids <- seq(1, length(input_data[[1]]))
  predictions <- as.data.table(model$predict_newdata(input_data))
  predictions <- full_join(as_tibble(predictions), input_data,
                           by = "row_ids",
                           multiple = "error")

  # assemble data for writing
  if (write_covars) {
    to_select <- c( "x", "y", "prob.pos", model$state$train_task$feature_names)
  } else {
    to_select <- c( "x", "y", "prob.pos")
  }
  # to_select <- c( "x", "y", "row_ids", "prob.pos", model$state$train_task$feature_names)
  predictions <- predictions[to_select]
  dir.create((file_out), showWarnings = FALSE, recursive = TRUE)

  if (output == "tif") {                            # write to a tif file
    # if (!str_ends(file_out, fixed(".tif"))) {
    #   file_out <- ".tif"
    # }
    predictions_rast <- terra::rast(predictions, type = "xyz")
    # print(names(predictions_rast))
    if (write_covars) {
      file_out <- paste0(file_out, "/", names(predictions_rast), ".tif")
    } else {
      file_out <- paste0(file_out, "/predictions.tif")
    }
    # print(file_out)
    terra::writeRaster(predictions_rast,
                       file_out,
                       filetype = "GTiff",
                       overwrite = params$overwrite)
  } else if (output == "csv") {                     # write to a csv file
    # if (!str_ends(file_out, fixed(".csv"))) {
    #   file_out <- ".csv"
    # }
    # print(file_out)
    write_csv(predictions,
              paste0(file_out, "predictions.csv"))
  } else {
    stop(paste0("`output` must be a supported filetype.",
                "\nx currently `csv` and `tif` are supported."))
  }

  print(paste0("Wrote file ", file_out))

  toc(quiet = FALSE)
  # print(predictions)
  return(TRUE)

}

#' Make quadratic features
#'
#' This helper function adds quadratic features to add for training the model. TerrainWorks found that adding these features could improve model performance.
#' Squared terms and interaction terms are added to the original data frame using using standardized column names.
#'
#' TODO: add handling for non-numeric features.
#'
#' @param data A data frame with feature columnns.
#' @param ignore A list of column names to ignore. Default is ("x", "y", and "row_ids").
#'
#' @return data
#' @export
#'
#' @importFrom combinat combn
make_quadratic_features <- function(data,
                                    ignore = c("x", "y", "row_ids")) {

  # find combinations of features
  use_these <- names(data)[!names(data) %in% ignore]
  combos <- combn(use_these, 2)

  # add squared terms
  for (col in use_these) {
    data[[paste0(col, "_sq")]] <- data[[col]] ^ 2
  }

  # add interaction terms
  for (col in 1:ncol(combos)) {
    combo <- combos[, col]
    data[[paste0(combo[[1]], "_by_", combo[[2]])]] <-
      data[[combo[[1]]]] * data[[combo[[2]]]]
  }

  return(data)
}
