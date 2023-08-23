
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
                                  files_in,
                                  output_dir,
                                  out_label,
                                  ...) {
  params <- list(...)

  # if (is.null(output_dir)) {
  #   output_dir <- paste0(dirname(files_in[1, 1]), "/predicting_output/")
  # }

  # print(as_tibble(t(files_in)))
  # print(paste0(output_dir, "predictions_", (gsub("\\D", "", files_in$gradient))))

  r <- mapply(predict_and_save,
              files_in,
              paste0(output_dir, out_label),
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
#' @importFrom purrr negate
predict_and_save <- function(model,
                             files_in,
                             file_out,
                             ...,
                             mask_range = NULL,
                             scale_vals = NULL,
                             ln_pca = TRUE,
                             transform = function(x) x,
                             file_type = "tif",
                             write_covars = TRUE,
                             quiet = FALSE) {
  params <- list(...)

  # time keeper
  tic()

  r <- lapply(files_in, function(x) {return(terra::rast(x))})
  rasters <- terra::rast(r)

  # remove any trailing numbers, if needed
  names(rasters) <- gsub("[^A-Za-z]*$","", names(rasters))

  # apply the analysis region mask
  if (!is.null(mask_range)) {
    mask <- mask_by_range(rasters,
                          mask_range)
    rasters <- terra::mask(rasters, mask)
  }

  # convert to a data frame
  input_data <- as_tibble(as.data.frame(rasters, xy = TRUE))

  #transform data as needed
  if (ln_pca & !is.null(input_data$pca)){
    input_data$ln_pca <- log(input_data$pca)
  }
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

  # assemble data for writing
  if (write_covars) {
    to_select <- c( "x", "y", "prob.pos", model$state$train_task$feature_names)
  } else {
    to_select <- c( "x", "y", "prob.pos")
  }
  predictions <- predictions[to_select]
  dir.create((file_out), showWarnings = FALSE, recursive = TRUE)

  # write files
  if (file_type == "tif") {                            # write to a tif file
    predictions_rast <- terra::rast(predictions, type = "xyz")
    if (write_covars) {
      file_out <- paste0(file_out, "", names(predictions_rast), ".tif")
    } else {
      file_out <- paste0(file_out, "predictions.tif")
    }
    terra::writeRaster(predictions_rast,
                       file_out,
                       filetype = "GTiff",
                       overwrite = params$overwrite)
  } else if (file_type == "csv") {                     # write to a csv file
    file_out <- paste0(file_out, "predictions.csv")
    write_csv(predictions, file_out)
  } else {
    stop(paste0("`file_type` must be a supported filetype.",
                "\nx currently `csv` and `tif` are supported."))
  }

  if(!quiet) {
    print(paste0("Wrote file ", file_out))
  }

  toc(quiet = quiet)
  return(TRUE)

}

#' Make quadratic features
#'
#' This helper function adds quadratic features to add for training the model.
#' TerrainWorks found that adding these features could improve model performance.
#' Squared terms and interaction terms are added to the original data frame
#' using standardized column names.
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
make_quadratic_features <- function(.data,
                                    ignore = c("x", "y", "row_ids")) {

  # check for non numeric columns
  nonnum <- names(select_if(.data, negate(is.numeric)))
  if (!all(nonnum %in% ignore)) {
    cols <-  nonnum[!(nonnum %in% ignore)]

    msg <- paste0("column(s) `", paste0(cols, collapse = "`, `"),
                  "` are non-numeric and are being ignored.")
    ignore <- nonnum
    warning(msg)
  }

  # find combinations of features
  use_these <- names(.data)[!names(.data) %in% ignore]
  combos <- combn(use_these, 2)

  # add squared terms
  for (col in use_these) {
    .data[[paste0(col, "_sq")]] <- .data[[col]] ^ 2
  }

  # add interaction terms
  for (col in 1:ncol(combos)) {
    combo <- combos[, col]
    .data[[paste0(combo[[1]], "_by_", combo[[2]])]] <-
      .data[[combo[[1]]]] * .data[[combo[[2]]]]
  }

  return(.data)
}

#' Flt to Tif file conversion
#'
#' This is a helpful tool for converting between flt (ArcGIS raster files) and
#' GTiff files. The FORTRAN executables included in the TerrainWorks R packages
#' currently use .flt files, but .tif files are generally a better format for
#' storage, since they support compression.
#'
#' This function offers an option to delete the .flt files, once it has been
#' converted. This option is included for space efficiency, if working with
#' large amounts of data.
#'
#' @param path Path of the file to convert
#' @param new_path The path to of where to write the file. The directory must
#'                 exist already. The default is a file of the same name as the
#'                 original file.
#' @param rm_old Whether to delete the .flt (and associated) files.
#' @param quiet Whether to report the time spent reading + writing files.
#'
#' @export
flt_to_tif <- function(path,
                       new_path = paste0(gsub("\\..*", "", path), ".tif"),
                       rm_old = FALSE,
                       quiet = TRUE) {
  tic()

  # read in raster
  raster <- terra::rast(path)
  terra::writeRaster(raster,
                     new_path,
                     filetype = "GTiff",
                     overwrite = TRUE)

  # delete files associated with the old .flt file
  if (rm_old) {
    delete <- list.files(dirname(path),
                         paste0(gsub("\\..*", "", basename(path)), "\\."),
                         full.names = TRUE)
    delete <- delete[!endsWith(delete, suffix = ".tif")]
    file.remove(delete)
  }

  toc(quiet = quiet)
}

#' Tif to flt file conversion
#'
#' This is a helpful tool for converting between Flt (ArcGIS raster files) and
#' GTiff files. The FORTRAN executables included in the TerrainWorks R packages
#' currently use .flt files, but .tif files are generally a better format for
#' storage, since they support compression.
#'
#' This function offers an option to delete the .tif file, once it has been
#' converted. This option is included for space efficiency, if working with
#' large amounts of data.
#'
#' @param path Path of the file to convert
#' @param new_path The path to of where to write the file. The directory must
#'                 exist already. The default is a file of the same name as the
#'                 original file.
#' @param rm_old Whether to delete the .tif (and associated) files.
#' @param quiet Whether to report the time spent reading + writing files.
#'
#' @export
tif_to_flt <- function(path,
                       new_path = paste0(gsub("\\..*", "", path), ".hdr"),
                       rm_old = FALSE,
                       quiet = TRUE) {
  tic()

  # read in raster
  raster <- terra::rast(path)
  terra::writeRaster(raster,
                     new_path,
                     filetype = "Ehdr",
                     overwrite = TRUE)

  # delete files associated with the old .flt file
  if (rm_old) {
    delete <- list.files(dirname(path),
                         gsub("\\..*", "", basename(path)),
                         full.names = TRUE)
    delete <- delete[!endsWith(delete, suffix = ".tif")]
    file.remove(delete)
  }

  toc(quiet = quiet)
}
