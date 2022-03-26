#' @title Calculate surface metrics
#'
#' @description Calculate surface metrics from a DEM. These are the same
#' metrics found in the
#' \href{https://github.com/TerrainWorks-Seattle/ForestedWetlands}{ForestedWetlands}
#' GIS toolbox. They can be calculated
#' here independent of ArcGIS. See the DEMutilities vignette for examples.
#'  Currently only supported for Windows. If you
#' would like support for other operating systems please contact the package
#' maintainers or submit an issue on
#' \href{https://github.com/TerrainWorks-Seattle/TerrainWorksUtils}{github}.
#'
#' @param metrics character vector of metrics to calculate. See details section
#' for options and descriptions of each metric.
#' @param DEM_path Path to DEM (flt or tif)
#' @param output_dir Directory to write output rasters
#' @param length_scale Length in meters over which to calculate slope metrics.
#' Choose a number greater than the resolution of the raster to smooth output.
#' @param output_suffix suffix to tag onto the end of output files. Default
#' is "_<length_scale>", so if you choose 15 for length_scale, the gradient
#' output raster will be named grad_15.tif
#' @param dev_resample Customizations for dev calculation
#' @param dev_interval Customizations for dev calculation
#' @param pca_hours Customizations for pca calculation (not currently
#' supported)
#' @param pca_conductivity Customizations for pca calculation (not currently
#' supported)
#'
#' @details Currently supported output metrics include:
#' \describe{
#'   \item{grad}{Gradient: Slope steepness measured along fall-line}
#'   \item{plan}{plan curvature: curvature along contour line
#'   (negative indicates convergent topography, positive indicates
#'   divergent topography)}
#'   \item{prof}{profile curvature: A measure of changes in slope along the
#'   fall line (direction of maximum slope). Negative values indicate
#'   decreasing slope; positive values indicate increasing slope.}
#'   \item{bcon}{b-contour}
#'   \item{dev}{Local deviation from mean elevation: an indicator of
#'   locally high or low topography. Defined as (z-zmean)/sd, where z is
#'   elevation at a point (DEM cell), zmean is mean elevation over a
#'   circle with diameter equal to the specified length scale, and sd is
#'   standard deviation of elevation within that circle.}
#'   \item{twi}{Topographic Wetness Index}
#' }
#'
#' @export
#'
calculate_surface_metrics <- function(metrics = c("grad", "plan"),
                                      DEM_path,
                                      output_dir = getwd(),
                                      length_scale = 15,
                                      output_suffix = paste0("_", length_scale),
                                      dev_resample = 1,
                                      dev_interval = 1,
                                      pca_hours = 48,
                                      pca_conductivity = 1) {
  if (!all(metrics %in% c(
    "grad",
    "plan",
    "prof",
    "bcon",
    "dev",
    "twi",
    "pca"
  ))) {
    stop("Invalid surface metrics specified")
  }

  executable_path <- get_executable_path()

  # TODO: add "overwrite" parameter, and if FALSE, don't re-calculate
  # existing grids

  suppressWarnings(
    makeGrids_inputFile_path <-
      normalizePath(file.path(output_dir, "input_makeGrids.txt"))
  )

  # DEM utilities currently only support .flt files.
  # Convert non-flt DEM (eg tiff) to .flt

  if (!grepl("\\.flt$", DEM_path)) {
    # Strip file extension from DEM_path, if it has one
    DEM_path_base <- gsub("\\.\\w+$", "", DEM_path)


    if (file.exists(paste0(DEM_path_base, ".flt"))) {
      # Add .flt extension if .flt file exists
      DEM_path <- paste0(DEM_path_base, ".flt")
    } else {
      # Create raster object from DEM
      dem <- terra::rast(DEM_path)

      # Create .flt filename
      DEM_path <- paste0(DEM_path_base, ".flt")

      # Save .flt file
      terra::writeRaster(dem, DEM_path)

      # Convert GDAL BIL flt header file to ESRI FLT header so it can
      # be read by ArcGIS and match expectation from DEM utilities scripts
      convert_hdr(paste0(DEM_path_base, ".hdr"))
    }
  }

  # Convert input length to meters
  dem <- terra::rast(DEM_path)
  DEM_units <- terra::linearUnits(dem)
  adjusted_length <- length_scale / DEM_units

  # --- makeGrids ---

  write_input_file_MakeGrids(
    DEM_path = DEM_path,
    length_scale = adjusted_length,
    output_dir = output_dir,
    grad = "grad" %in% metrics,
    plan = "plan" %in% metrics,
    prof = "prof" %in% metrics,
    bcon = "bcon" %in% metrics,
    filename = makeGrids_inputFile_path,
    output_file_extension = output_suffix
  )

  makeGrids <- file.path(executable_path, "MakeGrids.exe")
  command <- paste(makeGrids, makeGrids_inputFile_path, sep = " ")
  # Need wd to be scratch dir because that is where files are written
  wd <- getwd()
  setwd(output_dir)
  output <- system(command,
    wait = TRUE
  )
  setwd(wd)
  if (output != 0) {
    warning("Problem calculating partial contributing area: error ", output)
  }

  # --- localRelief (DEV) ---

  if ("dev" %in% metrics) {
    localRelief_inputFile_path <-
      normalizePath(file.path(output_dir, "input_localRelief.txt"))

    write_input_file_localRelief(
      DEM_path = DEM_path,
      length_scale = adjusted_length,
      output_dir = output_dir,
      resample = dev_resample,
      interval = dev_interval,
      filename = localRelief_inputFile_path,
      output_file_extension = output_suffix
    )

    localRelief <- file.path(executable_path, "LocalRelief.exe")
    command <- paste(localRelief, localRelief_inputFile_path, sep = " ")
    setwd(output_dir)
    output <- system(command,
      wait = TRUE
    )
    setwd(wd)
    if (output != 0) {
      warning("Problem calculating partial contributing area: error ", output)
    }
  }

  # --- topographic wetness index ---

  if ("twi" %in% metrics) {
    # First make sure all relevant input files are present
    #
    missing_metrics <- c()
    grad_path <- file.path(
      output_dir,
      paste0("grad", output_suffix, ".flt")
    )
    plan_path <- file.path(
      output_dir,
      paste0("plan", output_suffix, ".flt")
    )
    bcon_path <- file.path(
      output_dir,
      paste0("bcon", output_suffix, ".flt")
    )
    if (!file.exists(grad_path)) {
      missing_metrics <- c(missing_metrics, "grad")
    }
    if (!file.exists(plan_path)) {
      missing_metrics <- c(missing_metrics, "plan")
    }
    if (!file.exists(bcon_path)) {
      missing_metrics <- c(missing_metrics, "bcon")
    }

    if (length(missing_metrics) > 0) {
      # Run MakeGrids for all missing metrics
      twi_makeGrids_inputFile_path <-
        normalizePath(file.path(output_dir, "twi_input_makeGrids.txt"))

      write_input_file_MakeGrids(
        DEM_path = DEM_path,
        length_scale = adjusted_length,
        output_dir = output_dir,
        grad = "grad" %in% missing_metrics,
        plan = "plan" %in% missing_metrics,
        prof = FALSE,
        bcon = "bcon" %in% missing_metrics,
        filename = twi_makeGrids_inputFile_path,
        output_file_extension = output_suffix
      )

      makeGrids <- file.path(executable_path, "MakeGrids.exe")
      command <- paste(makeGrids, twi_makeGrids_inputFile_path, sep = " ")
      # Need wd to be scratch dir because that is where files are written
      setwd(output_dir)
      output <- system(command,
        wait = TRUE
      )
      setwd(wd)
    }

    buildGrids_inputFile_path <-
      normalizePath(file.path(output_dir, "input_buildGrids.txt"))

    write_input_file_buildGrids(
      DEM_path = DEM_path,
      length_scale = adjusted_length,
      output_dir = output_dir,
      grad_path = grad_path,
      plan_path = plan_path,
      bcon_path = bcon_path,
      filename = buildGrids_inputFile_path,
      output_file_extension = output_suffix
    )


    buildGrids <- file.path(executable_path, "BuildGrids.exe")
    command <- paste(buildGrids, buildGrids_inputFile_path, sep = " ")
    setwd(output_dir)
    output <- system(command,
      wait = TRUE
    )
    setwd(wd)
    if (output != 0) {
      warning("Problem calculating partial contributing area: error ", output)
    }
  }

  # ----- Partial Contributing Area ----- #
  # TODO: Need to add Partial.exe to ExecutableFiles.zip
  if (FALSE) {
    partial_inputFile_path <-
      normalizePath(file.path(output_dir, "input_partial.txt"))

    write_input_file_Partial(
      DEM_path = DEM_path,
      length_scale = length_scale,
      duration = pca_hours,
      conductivity = pca_conductivity,
      output_dir = ,
      filename = partial_inputFile_path,
      output_file_extension = output_suffix
    )

    Partial <- paste0(executable_path, "\\Partial.exe")
    command <- paste(Partial, partial_inputFile_path, sep = " ")
    setwd(output_dir)
    output <- system(command,
      wait = TRUE
    )
    setwd(wd)
    if (output != 0) {
      warning("Problem calculating partial contributing area: error ", output)
    }
  }

  # Reformat .flt files as .tif
  for (metric in metrics) {
    raster <- terra::rast(file.path(
      output_dir,
      paste0(metric, output_suffix, ".flt")
    ))
    terra::writeRaster(raster, file.path(
      output_dir,
      paste0(metric, output_suffix, ".tif")
    ))
  }
}
