#' @title Create Training Data from Polygons
#'
#' @export
#' @description Given an input dataset and set of polygons indicating
#' a class, sample points from inside and outside polygons and extract
#' predictor values for positive and negative points.
#'
#' @param polygons SpatVector of polygons indicating all locations belonging
#' to the class you wish to predict
#' @param predictors_raster SpatRaster with a layer for each predictor variable
#' @param analysis_region polygon or raster indicating the extent from which
#' points can be sampled. Only regions covered by non-NA cells will be included
#' if analysis_region is a raster.
#' @param sample_rate Samples per km^2
#' @param region_margin width in meters of margin to draw around polygon edges
#' which will not be used for sampling.
#' @param polygon_class string to use for class attribute for points sampled
#' from inside polygons
#' @param nonpolygon_class string to use for class attribute for points
#' sampled outside polygons
create_training_data_from_polygons <- function(polygons,
                                           predictors_raster,
                                           analysis_region,
                                           sample_rate = 0.5,
                                           region_margin = 50,
                                           polygon_class = "positive",
                                           nonpolygon_class = "negative") {
  training_points <- create_training_points_from_polygons(
    polygons = polygons,
    analysis_region = analysis_region,
    sample_rate = sample_rate,
    region_margin = region_margin,
    polygon_class = polygon_class,
    nonpolygon_class = nonpolygon_class
  )

  extract_values(
    raster = predictors_raster,
    points = training_points,
    xy = FALSE
  )
}
#------------------------------------------------------------------------------------------
#' @title Create Training Data from Points
#'
#' @description Given an input dataset and set of positive points, generate
#' randomly sampled negative points and extract predictor values for positive
#' and negative points.
#'
#' @return a data.frame with values for positive and negative points
#'
#' @param positive_points SpatVector with locations of all points with positive
#' class
#' @param predictors_raster SpatRaster with a layer for each predictor variable
#' @param analysis_region_mask SpatRaster with non-NA values everywhere that
#' points can be sampled from. All locations that should be excluded from
#' sampling should be NA. If NULL, all cells which are non-NA for all layers
#' of the predictors_raster will be used.
#' @param buffer_radius minimum possible distance between a positive and negative
#' point
#' @param negative_proportion Proportion of negative points to be generated
#' compared to number of positive points
#' @param extraction_method Method to use for extracting values from each point:
#'  "all", "centroid", "max", or "min". Ignored if extractionPoints is not
#'  polygon
#' @param extraction_layer Layer to use for extracting value. Ignored if
#' extraction_method = "centroid". Ignored if extraction_method is "all" or
#' "centroid" or if extractionPoints is not polygon.
#' @param rseed Optional integer to seed the random sampling. This allows exact
#' "random"  results to be reproduced multiple times. If no number is given, a
#' random number will be chosen as a seed.
#'
#' @export
create_training_data_from_points <- function(positive_points,
                                             predictors_raster,
                                             analysis_region_mask = NULL,
                                             buffer_radius = 15,
                                             negative_proportion = 1,
                                             extraction_method = "centroid",
                                             extraction_layer = NULL,
                                             rseed = NULL) {
  training_points <- sample_negative_points(
    positive_points = positive_points,
    analysis_region = analysis_region_mask,
    buffer = TRUE,
    buffer_radius = buffer_radius,
    negative_proportion = negative_proportion,
    rseed = rseed
  )

  extract_values(
    raster = predictors_raster,
    points = training_points,
    extraction_method = extraction_method,
    extraction_layer = extraction_layer,
    xy = FALSE
  )
}

#--------------------------------------------------------------------------------
#' @title Create training data from a negative buffer region
#'
#' @description Given an input dataset and set of positive points, generate
#' randomly sampled negative points and extract predictor values for positive
#' and negative points. This sampling method is designed for a data set of
#' positive points that is not considered a full inventory, such as the DOGAMI
#' landslide inventory. First, we build a presumed negative region, with enough
#' proximity to recorded landslide initiation points that we assume if a
#' landslide had occurred there, it would have been recorded.
#'
#' @param positive_points SpatVector with locations of all points with positive
#' class; that is landslide initiation points
#' @param predictors_raster SpatRaster with a layer for each predictor variable
#' @param pos_buffer The radius in meters around a positive point that is
#' considered within the landslide initiation zone; negative sample points 
#' are excluded from within this buffer
#' @param neg_buffer The radius around a positive point within which we are
#' confident that no landslides were observed; negative (nonlandslide) points
#' are sampled from the area between the pos_buffer and neg_buffer radius.
#' @param analysis_region_mask SpatRaster with data values everywhere that
#' points can be sampled from. All locations that should be excluded from
#' sampling should be NA. If NULL, all cells which are non-NA for all layers
#' of the predictors_raster will be used.
#' @param negative_proportion Proportion of negative points to be generated
#' compared to number of positive points
#' @param extraction_method Method to use for extracting values from each point:
#'  "all", "centroid", "max", or "min". Ignored if extractionPoints is not
#'  polygon
#' @param extraction_layer Layer to use for extracting value. Ignored if
#' extraction_method = "centroid". Ignored if extraction_method is "all" or
#' "centroid" or if extractionPoints is not polygon.
#' @param rseed Optional integer to seed the random sampling. This allows exact
#' "random"  results to be reproduced multiple times. If no number is given, a
#' random number will be chosen as a seed.
#'
#' @return a data.frame with values for positive and negative points
#' @export
#'
create_training_data_with_buffer <- function(positive_points,
                                             predictors_raster,
                                             pos_buffer = 15,
                                             neg_buffer = 50,
                                             analysis_region_mask = NULL,
                                             negative_proportion = 1,
                                             extraction_method = "centroid",
                                             extraction_layer = NULL,
                                             rseed = NULL) {

  neg_region <- make_neg_region(positive_points,
                                predictors_raster,
                                pos_buffer,
                                neg_buffer,
                                return_raster = TRUE)

  if (!is.null(analysis_region_mask) &
      class(analysis_region_mask) == "SpatRaster") {
    region_mask <- mask(neg_region, analysis_region_mask)
  } else {
    region_mask <- neg_region
  }

  create_training_data_from_points(positive_points,
                                   predictors_raster,
                                   analysis_region_mask = region_mask,
                                   buffer_radius = 1,
                                   negative_proportion = negative_proportion,
                                   extraction_method = "centroid",
                                   extraction_layer = NULL,
                                   rseed = rseed)

}

#----------------------------------------------------------------------------
#' @title Sample training points from a set of polygons
#'
#' @param polygons SpatVector of polygons indicating all locations belonging
#' to the class you wish to predict
#' @param analysis_region polygon or raster indicating the extent from which
#' points can be sampled. Only regions covered by non-NA cells will be included
#' if analysis_region is a raster.
#' @param sample_rate Samples per km^2
#' @param ratio Ratio of number of positive to negative samples
#' @param region_margin width in meters of margin to draw around polygon edges
#' which will not be used for sampling.
#' @param polygon_class label for points sampled from within polygons
#' @param nonpolygon_class label for points sampled outside polygons
#' @export
#'
create_training_points_from_polygons <- function(polygons,
                                             analysis_region,
                                             sample_rate = 0.5,
                                             ratio = 1,
                                             region_margin = 50,
                                             polygon_class = "positive",
                                             nonpolygon_class = "negative") {
  if (class(polygons) != "SpatVector") stop("polygons must be SpatVector")

  if (!is.numeric(sample_rate) | length(sample_rate) != 1) {
    stop("sample_rate must be a single numeric value")
  }  
  
  # Make sure analysis_region is polygon
  if (class(analysis_region) == "SpatRaster") {
    analysis_region <- as.polygons(analysis_region > -Inf)
  }

  # Prepare the region ---------------------------------------------------------

  # Shrink region by applying an interior margin. This ensures that training
  # points will not be sampled near the region's edges
  if (region_margin != 0) {
    regionPoly <- terra::buffer(analysis_region, width = -abs(region_margin))
  }

  # Sample from polygons ------------------------------------------------------------

  # Crop the wetland polygons to the region
  positive_polygons <- terra::project(polygons, analysis_region)
  positive_polygons <- terra::crop(positive_polygons, analysis_region)

  # Sample wetland regions
  positive_points <- sample_from_polygons(positive_polygons, sample_rate)

  # Sample non-wetlands --------------------------------------------------------
  negative_region <- terra::erase(analysis_region, positive_polygons)

  # Sample non-wetland regions
  negative_points <- sample_from_polygons(negative_region, sample_rate*ratio)


  # Combine sample points ------------------------------------------------------

  positive_points$class <- polygon_class
  negative_points$class <- nonpolygon_class

  training_points <- rbind(positive_points, negative_points)
  training_points
}

#----------------------------------------------------------------------------
#' @title Sample negative points
#'
#' @description randomly sample points from an analysis area to create a
#' dataset of negative points given a set of positive points
#'
#' @param positive_points SpatVector with locations of all points with positive
#' class
#' @param analysis_region SpatRaster with data values everywhere that
#' points can be sampled from. All locations that should be excluded from
#' sampling should be NA. If NULL, all cells which contain data for all layers
#' of the predictors_raster will be used.
#' @param buffer Return buffer around points?
#' @param buffer_radius minimum possible distance between a positive and negative
#' point
#' @param negative_proportion Proportion of negative points to be generated
#' compared to number of positive points
#' @param rseed Optional integer to seed the random sampling. This allows exact
#' "random"  results to be reproduced multiple times. If no number is given, a
#' random number will be chosen as a seed.
#'
#' @return SpatVector with positive and negative points, with a field
#' \code{"class"} indicating whether each point is positive or negative
#' @export
#' 
sample_negative_points <- function(positive_points,
                                   analysis_region,
                                   buffer = TRUE,
                                   buffer_radius = 15,
                                   negative_proportion = 1,
                                   rseed = NULL) {

  # Check params
  if (terra::geomtype(positive_points) != "points") {
    stop("positive_points must be points!")
  }
  if (class(analysis_region) != "SpatRaster") {
    stop("analysis_region must be raster!")
  }
  if (terra::crs(positive_points, proj = TRUE) !=
    terra::crs(analysis_region, proj = TRUE)) {
    stop("positive_points and analysis_region crs does not match!")
  }

  positive_points$class <- "positive"
  # Buffer positive points
  positive_buffers <- terra::buffer(positive_points,
    width = buffer_radius)

  # remove positive buffers from analysis_region
  negative_region <- terra::deepcopy(analysis_region)
  positive_cell_indices <- terra::extract(negative_region,
    positive_buffers,
    cells = TRUE
  )$cell
  negative_region[positive_cell_indices] <- NA

  # Determine how many to generate
  negative_buffers_count <- ceiling(length(positive_points) * negative_proportion)

  # Create negative_points
  negative_points <- sample_points(
    count = negative_buffers_count,
    region = negative_region,
    buffer = buffer,
    radius = buffer_radius,
    rseed = rseed
  )

  negative_points$class <- "negative"

  if (buffer) {
    terra::geomtype(positive_points)
    terra::geomtype(negative_points)
    return(rbind(terra::buffer(positive_points, width = buffer_radius),
                 negative_points))
  } else {
    return(rbind(positive_points,
                 negative_points))
  }
}

#-----------------------------------------------------------------------
#' @title Sample Points from a region
#'
#' @description Generates a SpatVector of points in a given
#' area.
#'
#' @details This extends \code{terra::spatSample()} to generate the correct
#' number of sampled points when the sample raster has a lot of NAs.
#'
#' @param count  The number of points to sample
#' @param region A SpatRaster with NA cells anywhere a sampled point
#'               cannot be located
#' @param buffer Should points be returned with a buffer?
#' @param radius The radius of each buffer (ignored if buffer = FALSE)
#' @param rseed Optional integer to seed the random sampling. This allows exact
#' "random"  results to be reproduced multiple times. If no number is given, a
#' random number will be chosen as a seed.
#'
#' @return A SpatVector of buffers.
#' @export
#' 
sample_points <- function(count,
                          region,
                          buffer = TRUE,
                          radius = 15,
                          rseed = NULL) {
  if (class(count) != "numeric") stop("count must be a number")
  if (class(region) != "SpatRaster") stop("region must be a raster")
  # NOTE: terra::spatSample() sometimes generates less than the requested
  # number of points if the sample raster has a lot of NAs. This is remedied
  # by repeatedly requesting a larger and larger number of points until
  # enough have been generated, then subsetting those for the correct amount.

  current_request <- count
  has_generated_enough <- FALSE

  if (!is.null(rseed)) {
    set.seed(rseed)
  }

  while (!has_generated_enough) {
    # Sample points anywhere that fits initiation conditions but recorded no landslides

    # suppress warning about fewer cell returned than requested
    sample_points <- withCallingHandlers(
      {
        terra::spatSample(
          region,
          size = current_request,
          na.rm = TRUE,
          as.points = TRUE,
          warn = FALSE
        )
      },
      warning = function(w) {
        if (conditionMessage(w) == "[spatSample] fewer cells returned than requested") {
          invokeRestart("muffleWarning")
        }
      }
    )

    # Test if enough non-initiation points have been generated
    if (length(sample_points) >= count) {
      # If so, subset and exit loop
      sample_points <- sample_points[seq_len(count)]
      has_generated_enough <- TRUE
    } else {
      # If not, double the next request, up to 10 times
      if (log2(current_request / count) < 10) {
        current_request <- current_request * 2
      } else {
        stop(
          "Cannot generate enough points. ",
          "Try a larger analysis_region or setting a smaller buffer_radius."
        )
      }
    }
  }

  # Create a buffer around each non-initiation point
  if (isTRUE(buffer) & radius > 0) {
    return(terra::buffer(sample_points, width = radius))
  } else {
    return(sample_points)
  }
}

#--------------------------------------------------------------------------------------
#' @title Extract buffer values
#'
#' @description Extracts values for all layers of a raster for a set of points
#' or polygons. If
#'
#' @param raster A SpatRaster of explanatory variables with a layer for
#' each variable to extract.
#' @param points SpatVector of points to use for extraction. If
#' polygons, the point chosen for extraction will be determined via the
#' extraction_method parameter.
#' @param extraction_method Method to use for extracting values from each point:
#'  "all", "centroid", "max", or "min". Ignored if extractionPoints is not
#'  polygon
#' @param extraction_layer Layer to use for extracting value. Ignored if
#' extraction_method = "centroid". Ignored if extraction_method is "all" or
#' "centroid" or if extractionPoints is not polygon.
#' @param return_type \code{"table"} or \code{"points"}. If \code{"table"}
#' (default) a data.table with values is returned. if \code{"points"}, a
#' \code{SpatVector} of points is returned, with training values as attributes
#' for each point.
#' @param na.rm Remove any cells with NA values?
#' @param ... Additional arguments passed onto \code{terra::extrac}
#'
#' @return A dataframe of extracted raster values with an additional "class"
#' column.
#' @export
#' 
extract_values <- function(raster,
                          points,
                          extraction_method = "all",
                          extraction_layer = NULL,
                          return_type = "table",
                          na.rm = TRUE,
                          ...) {

  # Check parameters
  if (class(raster) != "SpatRaster") stop("raster must be a raster!")
  if (class(points) != "SpatVector") stop("points must be points or polygon")
  if (!extraction_method %in% c("all", "centroid", "max", "min")) {
    stop("extraction_method must be 'all', 'centroid', 'max' or 'min'")
  }
  if (extraction_method %in% c("max", "min")) {
    if (is.null("extraction_layer")) {
      stop(
        "extraction_layer must be specified when extraction_method is ",
        extraction_method
      )
    }
    if (!extraction_layer %in% names(raster)) {
      stop("could not find ", extraction_layer, " in raster")
    }
  }

  if (extraction_method == "all" | terra::geomtype(points) == "points") {
    values <- terra::extract(raster, points, ...)
  } else {

    # Subset if a different buffer extraction method was requested
    if (extraction_method == "centroid") {

      # Find center points of polygons
      centerPoints <- terra::centroids(points)

      # Extract values from cells containing center points
      values <- terra::extract(raster, centerPoints, ...)
    } else {

      # First extract all values from polygons
      values <- terra::extract(raster, points, ...)

      # Formula to group entries by buffer and return requested variable value
      formula <- as.formula(paste(extraction_layer, "~", "ID"))

      # For each buffer, keep the entry with the "fun" variable value
      values <- merge(
        stats::aggregate(formula, extraction_method, data = values, na.rm = na.rm),
        values,
        sort = FALSE
      )
      # If any ties (eg multiple cells have max value) choose whichever
      # happens to be listed frist.
      values <- values[!duplicated(values$ID), ]
    }
  }

  if (na.rm) {
    na_rows <- apply(values, 1, function(x) sum(is.na(x)) > 1)
  }

  if (return_type == "table") {
    # Add any other variables from points back into extraction values
    for (var in setdiff(names(points), names(values))) {
      values[[var]] <- points[[var]][[var]]
    }

    # Remove the "ID" column
    values$ID <- NULL

    if (na.rm) {
      # Filter out entries with NA values in raster
      values <- values[!na_rows, ]
    }

    return(values)
  } else {

    # Remove the "ID" column
    values$ID <- NULL

    # add new variables from extraction values into points
    for (var in names(values)) {
      if (var %in% names(points)) {
        var_new <- paste0(var, ".1")
      } else {
        var_new <- var
      }

      points[, var_new] <- values[, var]
    }

    if (na.rm) {
      points <- terra::subset(points, !na_rows)
    }

    return(points)
  }
}

#----------------------------------------------------------------------------
#' @title Make a negative region raster
#'
#' @description Creates a negative region with which to sample from that exists
#' between an inner buffer and an outer buffer around given initiation points.
#' This method will be used to sample a landslide database that is not
#' considered a full inventory, i.e., we cannot assume all locations that are
#' not marked as landslide initiation points did not have landslide.
#'
#' @param initiation_points A SpatVector that contains the initiation points.
#' @param ref_raster SpatRaster used as a spatial reference; typically a DEM.
#' @param inner_buffer_rad The radius in meters of the inner buffer. This is
#' the area around the initiation point that should be considered "positive",
#' that is, within the landslide initiation zone.
#' @param outer_buffer_rad The radius in meters of the outer buffer. This is
#' the area around the landslide that can be considered "negative", that is
#' where no landslides were observed.
#' @return SpatVector with polygons outlining the presumed negative region.
#' @export
make_neg_region <- function(positive_points,
                            ref_raster,
                            inner_buffer,
                            outer_buffer,
                            return_raster = TRUE) {

  # create a positive region around the positive points
  pos_region <- terra::buffer(positive_points,
                              width = inner_buffer)
  pos_region <- terra::aggregate(pos_region)

  # create a negative region
  neg_buffer <- terra::buffer(positive_points,
                              width = outer_buffer)
  neg_buffer <- terra::aggregate(neg_buffer)
  neg_region <- terra::symdif(neg_buffer, pos_region)

  # return the correct type
  if (isFALSE(return_raster)) {
    return(neg_region)
  } else {
    rast_extent = rast(ref_raster)
    return(rasterize(neg_region, rast_extent))
  }

}
