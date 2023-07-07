
# ncol of scale_vals must equal length(elev_fnames). expects a tibble with column names that match the input variable names.
# if not, tries to infer based on order.

# model must have the function $predict_newdata(). otherwise, no requirements.

# input: predicting_data/
#     is a folder that contains folders of basin and sub-basins with elevation derivatives
#     already calculated.

# output: predicted_dems/
#     is a folder that contains the exact same structure of basins and sub-basins but
#     with the predicted probabilities saved as .csv files (and maybe also as .flt files
#     for easy raster reading/mapping)
#
#     this output will be used as input for model assessment (like the
#     success rate curve function)

predict_multiple_dems <- function(model,
                                  pred_dir,
                                  basin_list,
                                  out_dir = NULL,
                                  scale_vals = NULL,
                                  output = c("csv")) {

  # this function will use the model to predict a list of dems.
  # each dem should have the associated elevation derivative files within the folder.
  # the list input should be the name of the folder with the complete filename (i.e., Umpqua/basin1/)
  # for an example, see /code/sandbox/data/predicting_input.

  # elev fnames should identify all of the filenames necessary.
  # we call on elev_deriv and contributing_area functions here to read the files and input them into rasters.

  # for (dem in dem_list) {
  #     dem_rasts <- c(elev_deriv(gradient, mean_curv), contributing_area(pca))
  #     dem_df <- as.data.frame(dem_rasts, xy = TRUE)
  #
  #     dem_df <- center_and_scale(dem_df)
  #
  #     dem_pred <- predict_new_data(model, dem_df)
  #
  #     save_as(write_to_csv(dem_pred))
  # }

  # C/P from PFA_initiation_model.qmd
  # removed the success rate curve part, **started** to edit the code to adjust to parameters.

  if (is.null(out_dir)) {
    out_dir <- paste0(dirname(pred_dir), "/predicting_output/")
  }

  mapply(predict_and_save,
         paste0(pred_dir),
         paste0(out_dir, basin_list, "/predictions.csv"),
         MoreArgs = list(model = model,
                         scale_vals = scale_vals,
                         transform = make_quadratic_features))

  # predict_and_save(paste0(pred_dir, basin_list),
  #                  file_out = paste0(out_dir, basin_list, "/predictions.csv"),
  #                  model = model,
  #                  scale_vals = scale_vals,
  #                  transform = make_quadratic_features)

}


#' Predict and save
#'
#' This function takes a folder with elevation derivative files for a basin and a model and saves the predictions to a .csv file.
#' The csv has columns "x" and "y" with coordinates, "prob.pos" with the model output, and all features used in the calculation of the model.
#'
#' TODO: add compression option
#'
#' @param dir_in A directory which contains the necessary elevation derivative files for prediction.
#' @param file_out The name of the csv file to write results.
#' @param model A trained model. Must have function $predict_newdata and $state$train_task$feature_names. These are both provided by most mlr3 learners.
#' @param scale_vals The centering and scaling values used in training the model. The same values should be used when predicting data. The function expects a tibble with one column per feature and two values per feature. For example, \code{tibble(gradient = c(center_value, scale_value), mean_curv = c(center_value, scale_value))}.
#' @param transform A function that calculates any transformed columns needed for the model. For example, this package includes a function called "make_quadratic_features" which calculateds squared and interaction terms for all numeric features.
#'
#' @return
#' @export
#'
#' @examples
predict_and_save <- function(dir_in,
                             file_out,
                             model,
                             scale_vals = NULL,
                             transform = function(x) x) {
  tic()
  # read files into a raster
  topo_files <- c(paste0("GRADIENT,", dir_in, "/gradient.flt"),
                  paste0("MEAN CURVATURE,", dir_in, "/mean_curv.flt"))
  topo_rast <- elev_deriv(rasters = topo_files)
  pca_file <- list.files(paste0(dir_in), "^pca_6_[0-9]{1,3}\\.flt", full.names = TRUE)
  pca_rast <- contributing_area(raster = pca_file)

  # combine into one raster
  rasters <- c(topo_rast, pca_rast)
  names(rasters) <- c("gradient", "mean_curv", "pca")

  # convert to a data frame
  input_data <- as_tibble(as.data.frame(rasters, xy = TRUE))


  input_data$pca <- log(input_data$pca)
  input_data <- rename(input_data, ln_pca_k1_48 = pca) # comment this out when i change the names used for training the model.

  input_data <- transform(input_data)

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

  # write to a file
  to_select <- c("row_ids", "x", "y", "prob.pos", model$state$train_task$feature_names)
  dir.create(dirname(dirname(file_out)), showWarnings = FALSE)
  dir.create(dirname(file_out), showWarnings = FALSE)

  if (!str_ends(file_out, fixed(".csv"))) {
    file_out <- ".csv"
  }
  # print(file_out)
  write_csv(select(predictions, to_select),
            file_out)
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
