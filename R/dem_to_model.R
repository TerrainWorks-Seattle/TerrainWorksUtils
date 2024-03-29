

#' @title Dem to model
#'
#' @description Take a DEM and all additional parameters needed to build the
#' model. All parameters have default values that are based on a current
#' best-guess based on current literature or understanding of physical processes.
#' The purpose of this script is to make it easy to evaluate the sensitivity
#' of the model to various parameters.
#'
#' @param dems File path(s) for the DEM of the study area.
#' @param initiation_points File path(s) for the initiation points in the study area
#' @param output_dir File path(s) for a directory where output files will be
#'        created.
#' @param elev_derivates The elevation derivatives to calculate and include in
#'        model generation. Current supported options are grad (gradient), prof
#'        (profile curvature), plan (planar curvature), norm (normal slope
#'        curvature), tan (tangential curvature), and mean (mean curvature).
#' @param pca_durations The storm duration(s) in hours for the partial
#'        contributing area calculation.
#' @param pca_conductivity The hydraulic conductivity used in the contributing
#'        area calculation.
#' @param length_scale The length scale in m over which to calculate the
#'        elevation derivatives.
#' @param use_analysis_mask Whether to use an analysis mask or not. This is an
#'        optional step which limits the selection of negative data points to
#'        areas on the DEM where the specific elevation derivatives fall within
#'        the range observed in the initiation points.
#' @param analysis_mask_derivs Which elevation derivatives to use for
#'        generating the analysis mask.
#' @param analysis_mask_expansion The expansion of the range to use for
#'        generating the analysis mask.
#' @param neg_region_buffer The radius in m around the initiation points that
#'        should be considered the negative region. We assume that any landslide
#'        within this range would have been observed.
#' @param pos_region_buffer The radius in m around the initiation points that
#'        should be considered part of the landslide initiation.
#' @param neg_sampling_proportion The proportion of negative to positive points
#'        to be sampled from the negative region.
#' @param preprocess_norm Should training data be normalized before the model
#'        is trained
#' @param preprocess_center Should training data be centered before the model
#'        is trained
#'
#' @return Returns the information needed to evaluate the model. For this, we
#' could probably look at just ROC, AUC.
#' @export
#'
dem_to_model <- function(dems,
                         initiation_points,
                         output_dir,
                         elev_derivatives = c("plan", "norm", "mean", "tan"),
                         pca_durations = c(5, 24),
                         pca_conductivity = 1,
                         length_scale = 15,
                         use_analysis_mask = FALSE,
                         analysis_mask_derivs = NULL,
                         analysis_mask_expansion = 1,
                         neg_region_buffer = 150,
                         pos_region_buffer = 11,
                         neg_sampling_proportion = 1,
                         preprocess_norm = TRUE,
                         preprocess_center = TRUE,
                         plot_probability_raster = FALSE) {

  # ---------------- Error checking input values ---------------------- #

  # if (!file.exists(dem)) {
  #   stop("Must provide a valid DEM file.")
  # }

  # if (!file.exists(initiation_points)) {
  #   stop("Must provide a valid file with initiation points.")
  # }

  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist or was not specified.")
  }

  if (!all(elev_derivatives %in%
           c("grad", "plan", "mean", "tan", "prof", "norm"))) {
    stop("Unsupported derivative. Must be grad, plan, mean, tan, prof, or norm.")
  }

  if ("mean" %in% elev_derivatives & !("norm" %in% elev_derivatives &
                                       "tan" %in% elev_derivatives)) {
    stop("Must have both normal and tangential curvatures to calculate mean curvature.")
  }

  if (length_scale <= 0) {
    stop("Length scale must be larger than 0.")
  }
#
#   dem_input <- terra::rast(dem)
#   dem_width = dim(dem_input)[1] * terra::res(dem_input)[1]
#   dem_length = dim(dem_input)[2] * terra::res(dem_input)[2]

  if (length_scale > 100) {
    warning("Using too large of a length scale may lead to bad results.")
  }

  if (!is.logical(use_analysis_mask)) {
    stop("Indicate analysis mask usage with TRUE or FALSE.")
  }

  if (use_analysis_mask & !is.null(analysis_mask_derivs)  &
      !all(analysis_mask_derivs %in% elev_derivatives)) {
    stop("Only calculated derivatives can be used for building the analysis mask.")
  }

  if (pos_region_buffer <= 0) {
    stop("Positive region buffer must be greater than 0.")
  }

  if (neg_region_buffer <= pos_region_buffer) {
    stop("Negative region buffer length must be greater than the positive region buffer")
  }

  # if (neg_region_buffer > dem_width | neg_region_buffer > dem_length) {
  #   warning("Negative region buffer is larger than the study area.")
  # }

  if (neg_sampling_proportion < 0) {
    stop("Negative sampling proportion must be positive.")
  }

  if (!is.logical(preprocess_norm) | !is.logical(preprocess_center)) {
    stop("Indicate preprocessing steps with TRUE or FALSE.")
  }

  # ------------------------------------------------------------------- #
  # ---------------- Calculating DEM derivatives ---------------------- #

  print("Calculating elevation derivatives...")

  n <- 1
  all_train_data <- data.frame()
  all_data <- data.frame()

  for (dem in dems) {

    if (!file.exists(dem)) {
      stop("Must provide a valid DEM file.")
    }

    if (!file.exists(initiation_points[n])) {
      stop("Must provide a valid file with initiation points.")
    }

    output_subdir = output_dir[n]
    if (!dir.exists(output_subdir)) {
      dir.create(output_subdir)
    }


    to_calculate <- list()
    names <- list()
    missing = FALSE

    if ("grad" %in% elev_derivatives) {
      if (file.exists(paste0(output_subdir,"grad_", length_scale, ".flt"))) {
        missing = missing | FALSE
      } else {
        missing = TRUE
      }
      to_calculate <- append(to_calculate, paste0("GRADIENT,",output_subdir,
                                                  "grad_", length_scale))
      names <- append(names, "grad")
    }
    if ("plan" %in% elev_derivatives) {
      if (file.exists(paste0(output_subdir,"plan_", length_scale, ".flt"))) {
        missing = missing | FALSE
      } else {
        missing = TRUE
      }
      to_calculate <- append(to_calculate, paste0("PLAN CURVATURE,",output_subdir,
                                                  "plan_", length_scale))
      names <- append(names, "plan")
    }
    if ("prof" %in% elev_derivatives) {
      if (file.exists(paste0(output_subdir,"prof_", length_scale, ".flt"))) {
        missing = missing | FALSE
      } else {
        missing = TRUE
      }
      to_calculate <- append(to_calculate, paste0("PROFILE CURVATURE,",output_subdir,
                                                  "prof_", length_scale))
      names <- append(names, "prof")
    }
    if ("norm" %in% elev_derivatives) {
      if (file.exists(paste0(output_subdir,"norm_", length_scale, ".flt"))) {
        missing = missing | FALSE
      } else {
        missing = TRUE
      }
      to_calculate <- append(to_calculate, paste0("NORMAL SLOPE CURVATURE,",output_subdir,
                                                  "norm_", length_scale))
      names <- append(names, "norm")
    }
    if ("tan" %in% elev_derivatives) {
      if (file.exists(paste0(output_subdir,"tan_", length_scale, ".flt"))) {
        missing = missing | FALSE
      } else {
        missing = TRUE
      }
      to_calculate <- append(to_calculate, paste0("TANGENTIAL CURVATURE,",output_subdir,
                                                  "tan_", length_scale))
      names <- append(names, "tan")

    }

    if (missing) {
      print("need to recalculate metrics")
      vars_raster <- elev_deriv(rasters = to_calculate,
                                length_scale = length_scale,
                                dem = dem,
                                scratch_dir = output_subdir)
    } else {
      vars_raster <- elev_deriv(rasters = to_calculate,
                                length_scale = length_scale,
                                scratch_dir = output_subdir)
    }



    names(vars_raster) <- names

    for (dur in pca_durations) {
      pca_loc <- (paste0(output_subdir,"pca_k", pca_conductivity,"_d", dur, ".flt"))

      if (file.exists(pca_loc)) {
        vars_raster <- c(vars_raster, terra::rast(pca_loc))
      } else {
        vars_raster <- c(vars_raster, contributing_area(raster = pca_loc,
                                        dem = dem,
                                        length_scale = length_scale,
                                        k = pca_conductivity,
                                        d = dur,
                                        scratch_dir = output_subdir))
      }
    }

    if ("mean" %in% elev_derivatives) {
      mean <- (vars_raster$norm + vars_raster$tan) / 2
      names(mean) <- "mean"
      vars_raster = c(vars_raster, mean)
    }


    # ------------------------------------------------------------------- #
    # ------------------- Creating training data ------------------------ #

    init_points <- vect(initiation_points[n])

    print("Generating training data...")

    if (use_analysis_mask) {
      analysis_mask <- create_analysis_region_mask(
                          raster = vars_raster,
                          points = init_points,
                          mask_vars = analysis_mask_derivs,
                          expansion_factor = analysis_mask_expansion)

      training_data <- create_training_data_with_buffer(
                          positive_points = init_points,
                          predictors_raster = vars_raster,
                          pos_buffer = pos_region_buffer,
                          neg_buffer = neg_region_buffer,
                          analysis_region_mask = analysis_mask,
                          negative_proportion = neg_sampling_proportion,
                          extraction_method = "centroid",
                          extraction_layer = NULL,
                          rseed = 123)
    } else {

      training_data <- create_training_data_with_buffer(
                          positive_points = init_points,
                          predictors_raster = vars_raster,
                          pos_buffer = pos_region_buffer,
                          neg_buffer = neg_region_buffer,
                          analysis_region_mask = NULL,
                          negative_proportion = neg_sampling_proportion,
                          extraction_method = "centroid",
                          extraction_layer = NULL,
                          rseed = 123)
    }

    training_data <- training_data[, c(names(vars_raster), "class")]
    training_data$region <- n

    all_train_data <- rbind(all_train_data, training_data)
    all_data <- rbind(all_data, as.data.frame(vars_raster, xy = TRUE))

    n <- n + 1
  }

  # ------------------------------------------------------------------- #
  # ------------------------- Build model ----------------------------- #

  print("Building model...")

  if (!preprocess_norm & !preprocess_center) {
    preproc_steps = NULL
  } else if (preprocess_center & !preprocess_norm) {
    preproc_steps <- c("center")
  } else if (preprocess_norm & !preprocess_center) {
    preproc_steps <- c("scale")
  } else if (preprocess_norm & preprocess_center) {
    preproc_steps <- c("center", "scale")
  }

  model <- build_k_fold_rf_model(data = subset(all_train_data, select = -c(region)),
                                 seed = 123,
                                 ctrl_method = "repeatedcv",
                                 folds = 5,
                                 repeats = 3,
                                 preprocess = preproc_steps)


}

#' Make probability raster
#'
#' @param model A model object that can be called with the predict function.
#' @param data The full dataset to be predicted.
#' @param plot Whether to produce a plot of the prediction raster.
#'
#' @return The predicted raster
#' @export
make_prob_raster <- function(model,
                             data,
                             plot = FALSE) {

  # Predict landslide initiation probabilities
  pred <- predict(model,
                  newdata = data,
                  type  = "prob")

  # Only return the probability of landslide initiation (positive class)
  pred <- cbind(data$x, data$y, pred$positive)
  colnames(pred) <- c("x", "y", "prob")

  return(rast(pred, type = "xyz"))

}

#' @title Generate success curve
#'
#' @description Produces a success curve for a give prediction raster. The
#' success curve divides the predicted raster into 10 probability regions, then
#' finds the proportion of total inititation points in each probability region.
#'
#' @param predicted_raster A raster of the study area with predicted
#' probabilities. This must have a layer called "positive", indicating the
#' probability of an initiation point, with values ranging from 0 to 1.
#' @param initiation_points A vector object with initiation points.
#' @param step_size A parameter indicating how big each probability region range
#' should be.
#' @param plot If FALSE, the plot will not be produced.
#'
#' @return The values associated with the success curve.
#' @export
generate_success_curve <- function(predicted_raster,
                                   initiation_points,
                                   step_size = 0.1,
                                   plot = TRUE) {

  init_regions <- extract(predicted_raster, initiation_points)

  # Create bins and convert to raster
  breaks = seq(0, 1, step_size)
  init_regions$binned <- cut(init_regions$prob, breaks = breaks)

  # Find the frequencies of each region
  freqs <- as.data.frame(table(init_regions$binned))
  freqs$cumul <- (cumsum(freqs$Freq) / sum(freqs$Freq))

  xs <- seq(1, length(breaks) - 1)
  ys <- rep(0, length(breaks) - 1)
  ys <- freqs$cumul
  success_curve <- cbind(xs / max(xs), ys)
  colnames(success_curve) <- c("region_cutoff", "init_proportion")

  # Plot the success_curve
  if (!silence_plot) {
    plot(success_curve,
       pch = 20,
       xlim = c(0, 1),
       xlab = "Probability region",
       ylim = c(0, 1),
       ylab = "Proportion of initation points",
       type = "b",
       col = "blue",
       )
  }

  return(success_curve)
}

