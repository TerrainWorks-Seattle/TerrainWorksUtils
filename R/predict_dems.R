
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
                                  basin_list,
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

  mapply()

  predict_and_save(paste0(pred_dir, basin_list),
                   file_out = paste0(pred_dir, basin_list, "predictions.csv"),
                   model = ls_model,
                   scale_vals = cs_vals,
                   transform = make_quadratic_features)

}

# for use with lapply(basin_list, predict_and_save)

# takes the dem location, the model,
predict_and_save <- function(dir_in,
                             file_out,
                             model,
                             scale_vals = NULL,
                             transform = function(x) x) {

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
  input_data <- rename(input_data, ln_pca_k1_48 = pca)

  input_data <- transform(input_data)

  # print(input_data)

  # # check that transform() added all necessary features
  # if (!all(names(input_data) %in% model$task$feature_names)) {
  #   message <- paste0("Some features are missing: ", {{names(input_data)}})
  #   stop("Some input features are missing: ")
  # }

  # print(scale_vals)
  # print(names(input_data))

  # center and scale the data
  if (!is.null(scale_vals)) {
    for (col in names(scale_vals)) {
      if (col %in% names(input_data)) {
        # print(col)
        # print(scale_vals[1, col])
        # print(scale_vals[2, col])
        input_data[[col]] <- ((input_data[[col]] - scale_vals[[col]][1]) / scale_vals[[col]][2])
        # input_data[col] <- (input_data[col] - scale_vals[1, col]) / scale_vals[2, col]
      }
    }
  }

  # print(input_data)

  # predict the data
  input_data$row_ids <- seq(1, length(input_data[[1]]))

  # print(input_data$gradient)f

  predictions <- as.data.table(model$predict_newdata(input_data))
  predictions <- full_join(as_tibble(predictions), input_data,
                           by = "row_ids",
                           multiple = "error")

  # write to a file
  to_select <- c("row_ids", "x", "y", "prob.pos", ls_model$state$train_task$feature_names)
  dir.create(dirname(file_out), showWarnings = FALSE)
  write_csv(select(predictions, to_select),
            file_out)

  # print(predictions)
  return(predictions)

}

#' Make quadratic features
#'
#' This helper function adds quadratic features to add for training the model. TerrainWorks found that adding these features could improve model performance.
#' Squared terms and interaction terms are added to the original data frame using using standardized column names.
#'
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
