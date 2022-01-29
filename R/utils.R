

log_err <- function(...) {
  stop(sprintf("[%s] %s\n", Sys.time(), paste0(..., collapse = "")))
}

log_msg <- function(...) {
  message(sprintf("[%s] %s\n", Sys.time(), paste0(..., collapse = "")))
}

log_obj <- function(obj) {
  message(sprintf(
    "[%s] \n    %s\n",
    Sys.time(),
    paste0(utils::capture.output(obj),
      collapse = "\n    "
    )
  ))
}

#' @export
#'
#' @import terra
#' @import methods
#' @import randomForest
#' @import ROCR
#' @import stats
#'
#' @title Align Rasters
#'
#' @description Aligns multiple rasters with a single reference raster. A raster
#' will be aligned if it doesn't match the dimensions, resolution, extent,
#' origin, or CRS projection of the reference raster.
#'
#' @details Projecting a raster requires that its cell values be estimated in
#' accordance with its new projection. Continuous variable rasters will use
#' bilinear interpolation while categorical (factor) rasters will use
#' nearest-neighbor sampling.
#'
#' @param referenceRaster A \code{SpatRaster} object to be aligned with.
#' @param inputRasters A list of \code{SpatRaster} objects to align with the
#' \code{referenceRaster}.
#'
#' @return A list of \code{SpatRaster} objects that share the same grid and
#' projection as \code{referenceRaster}.
#'
alignRasters <- function(referenceRaster = NULL, inputRasters = NULL) {

  # Validate parameters --------------------------------------------------------

  if (!("SpatRaster" %in% class(referenceRaster))) {
    stop("Argument 'referenceRaster' must be a 'SpatRaster' object.")
  }

  if (!("list" %in% class(inputRasters))) {
    stop("Argument 'inputRasters' must be a list")
  }

  # Align rasters --------------------------------------------------------------

  alignedRasters <- list()

  # For each input raster
  for (i in seq_along(inputRasters)) {
    inputRaster <- inputRasters[[i]]

    if (!("SpatRaster" %in% class(inputRaster))) {
      stop("inputRaster[[", i, "]] must be a 'SpatRaster' object.")
    }

    # Compare raster extents
    tryCatch(
      {
        extentMatch <- terra::ext(inputRaster) == terra::ext(referenceRaster)
      },
      error = function(err) {
        message("Error comparing extent of inputRaster[[", i, "]] with referenceRaster:")
        stop(err)
      }
    )

    # Compare raster dimensions
    tryCatch(
      {
        dimensionMatch <- all(dim(inputRaster) == dim(referenceRaster))
      },
      error = function(err) {
        message("Error comparing dimensions of inputRaster[[", i, "]] with referenceRaster:")
        stop(err)
      }
    )

    # Compare raster resolutions
    tryCatch(
      {
        resolutionMatch <- all(terra::res(inputRaster) == terra::res(referenceRaster))
      },
      error = function(err) {
        message("Error comparing resolutions of inputRaster[[", i, "]] with referenceRaster:")
        stop(err)
      }
    )

    # Compare raster resolutions
    tryCatch(
      {
        originMatch <- all(terra::origin(inputRaster) == terra::origin(referenceRaster))
      },
      error = function(err) {
        message("Error comparing origins of inputRaster[[", i, "]] with referenceRaster:")
        stop(err)
      }
    )

    # Compare raster coordinate reference systems
    tryCatch(
      {
        crsMatch <- terra::crs(inputRaster) == terra::crs(referenceRaster)
      },
      error = function(err) {
        message("Error comparing coordinate reference systems of inputRaster[[", i, "]] with referenceRaster:")
        stop(err)
      }
    )

    # Reproject the input raster if it doesn't align with the reference raster
    if (!extentMatch || !dimensionMatch || !resolutionMatch || !originMatch || !crsMatch) {
      log_msg("Aligning input raster", i)
      tryCatch(
        {
          # Determine what estimation method to use based on variable type (continuous/categorical)
          estimationMethod <- ifelse(terra::is.factor(inputRaster), "near", "bilinear")
          inputRaster <- terra::project(inputRaster, referenceRaster, method = estimationMethod)
        },
        error = function(err) {
          message("Error trying to project inputRaster[[", i, "]] onto referenceRaster:")
          stop(err)
        }
      )
    }
    log_msg("Raster ", i, " aligned.")

    # Store the aligned input raster
    alignedRasters[[i]] <- inputRaster
  }

  # Return ---------------------------------------------------------------------

  return(alignedRasters)
}


#' @export
#'
#' @title Extract raster values
#'
#' @description Extracts all raster variable values at specified locations.
#'
#' @param raster A \code{SpatRaster} object to extract values from.
#' @param points A \code{SpatVector} object of points.
#'
#' @return A \code{data.frame} of values.
#'
#'
#'
##'
extractRasterValues <- function(raster = NULL,
                                points = NULL) {

  # Validate parameters --------------------------------------------------------

  if (terra::nlyr(raster) == 0) {
    stop("Cannot extract values from a non-single-layer raster")
  }

  # Extract values -------------------------------------------------------------

  # Project the points into the same CRS as the raster
  projectedPoints <- terra::project(points, raster)

  # Define a data frame to store all layer values
  allValues <- data.frame(dummy = rep(NA, length(points)))
  removeDummy <- TRUE

  for (i in seq_len(terra::nlyr(raster))) {
    layer <- raster[[i]]

    if (terra::is.factor(layer)) {
      # Extract numeric factor value at each point
      values <- terra::extract(
        layer,
        projectedPoints,
        method = "simple",
        factor = TRUE
      )

      # Remove 'ID' column
      values <- values[, -1]

      # Store values in dataframe
      df <- data.frame(values)
      names(df) <- names(layer)

      # Add values to full dataset
      allValues <- cbind(allValues, df)
    } else {
      # Extract continuous value at each point
      values <- terra::extract(
        layer,
        projectedPoints,
        method = "simple"
      )

      # Remove 'ID' column
      values <- values[, -1]

      # Format values
      df <- data.frame(values)
      names(df) <- names(layer)
      allValues <- cbind(allValues, df)
    }

    # Remove the 'dummy' column as soon as a real value column is added
    if (removeDummy) {
      allValues[, 1] <- NULL
      removeDummy <- FALSE
    }
  }

  # Return ---------------------------------------------------------------------

  return(allValues)
}


#' @export
#'
#' @title Sample points from polygons
#'
#' @description Given a set of polygons, randomly sample points
#' from within polygons.
#'
#' @param polygons SpatVector object containing polygons
#' @param sampleRate Number of points per kilometer to sample
#'
#' @return SpatVector of sampled points
sampleFromPolygons <- function(polygons,
                               sampleRate) {

  # Collect sample coordinates
  pointsList <- lapply(seq_along(polygons), function(i) {
    poly <- polygons[i]
    # Determine the number of samples to take from the polygon's area
    polyArea <- terra::expanse(poly, unit = "km")
    if (polyArea == 0) next
    sampleSize <- ceiling(polyArea * sampleRate)

    # Sample the polygon
    samplePoints <- terra::spatSample(poly, size = sampleSize)
  })

  terra::vect(pointsList)
}

#' @export
#'
#' @title Apply a set of "cats" to a factor raster
#'
#' @description Applies a given set of "cats" (factor level ID-name pairs) to a
#' factor raster. Any level names already shared between the given cats and the
#' raster will be numerically remapped to the given cats IDs.
#'
#' @details terra manages factor rasters by having each cell store an integer
#' ID (0, 1, 2, ...) which corresponds to a level name as found in
#' \code{terra::cats()}. However, if you load a raster with levels "A", "B", "C"
#' and another raster with levels "B", "C", "D", terra won't detect the
#' overlapping levels since B=1 and C=2 in the first raster and B=0 and C=1 in
#' the second. This is important when it comes to creating prediction models
#' that take factor rasters as input. Let's say that level "C" is a
#' high-probability indicator of event X. If you generate a model with raster 1
#' the model will learn to associate X with the value C=2. If you then run the
#' model with raster 2, the model will predict high-probability of X at cells
#' with D=2, which is numerically correct but the level names don't match. The
#' \code{setRasterLevels()} function solves this problem by ensuring that any
#' level names shared between two factor rasters will also share the same
#' integer ID.
#'
#' @param raster The factor \code{SpatRast} object to apply the given
#' \code{cats} to.
#' @param cats A \code{data.frame} that includes a "category" character column
#' (the output of \code{terra::cats()}).
#'S
#'
applyCats <- function(raster, cats) {

  # Store the original raster variable name for later
  varName <- names(raster)

  # Get the raster cats
  rasterCats <- terra::cats(raster)[[1]]

  # Find the common level names between the raster and the given cats
  sharedLevelNames <- intersect(cats$category, rasterCats$category)

  # Define shared-level ID translation matrix (from ID -> to ID)
  trans <- matrix(data <- rep(NA, 2 * length(sharedLevelNames)), ncol = 2)
  trans[, 1] <- which(rasterCats$category %in% sharedLevelNames) - 1
  trans[, 2] <- which(cats$category %in% sharedLevelNames) - 1

  # Re-classify input raster values according to the translation matrix
  raster <- terra::classify(raster, rcl = trans, othersNA = TRUE)

  # Set raster cats to those given
  terra::setCats(raster, layer = 1, value = cats$category)

  # Reset raster name back to its original
  names(raster) <- varName

  return(raster)
}

#' @export
#'
#' @title Fix a misfactored raster
#'
#' @description Creates a correctly-factored version of a misfactored
#' \code{SpatRast} object.
#'
#' @details terra seems to have issues loading some factor raster files. For
#' instance, a factor raster made using the ArcGIS 'Polygon to Raster' tool and
#' then loaded with \code{terra::rast()} will only show a 'Count' field instead
#' of the field specified in 'Polygon to Raster'. Additionally, the 'Count'
#' value assigned to each cell appears to be misleveled by 1 row when inspected
#' in the raster's \code{terra::cats()} table. The \code{fixFactorRaster()}
#' function attempts to take a faulty raster and map its values to the correct
#' field, as it still exists in the \code{terra::cats()} table.
#'
#' @param raster The faulty factor \code{SpatRast} object.
#'
#'
fixFactorRaster <- function(raster = NULL) {
  if (!terra::is.factor(raster)) {
    return(raster)
  }

  if (terra::nlyr(raster) != 1) {
    stop("Can only fix single-band rasters")
  }

  # Determine factor levels
  levelsDf <- terra::cats(raster)[[1]]
  levelsCol <- which(sapply(levelsDf, class) == "character")
  levels <- levelsDf[, levelsCol]

  # Map numeric factor values to their corresponding char values
  numericValues <- terra::values(raster)[, 1]
  factorValues <- levels[numericValues]

  # Fill a new factor raster with the character factor values
  factorRaster <- terra::rast(
    extent = terra::ext(raster),
    crs = terra::crs(raster),
    resolution = terra::res(raster),
    vals = factorValues,
    names = colnames(levelsDf)[levelsCol]
  )

  return(factorRaster)
}
