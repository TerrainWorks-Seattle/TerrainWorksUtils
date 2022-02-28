#' Calculate surface metrics
#' @export
calculate_surface_metrics <- function(
  metrics = c("grad", "plan", "prof", "bcon", "dev", "twi"),
  DEM_path,
  scratch_folder = getwd(),
  length_scale = 15,
  output_suffix = paste0("_", length_scale),
  dev_resample = 1,
  dev_interval = 1,
  pca_hours = 48,
  pca_conductivity = 1
) {

  if (!all(metrics) %in% c("grad",
                           "plan",
                           "prof",
                           "bcon",
                           "dev",
                           "twi",
                           "pca")) {
    stop("Invalid surface metrics specified")
  }

  executable_path <- get_executable_path()

  # TODO: Check for executable files in executable_path
  # TODO: add "overwrite" parameter, and if FALSE, don't re-calculate
  # existing grids

  makeGrids_inputFile_path <-
    normalizePath(file.path(scratch_folder, "input_makeGrids.txt"))

  # Convert input length to meters
  dem <- terra::rast(DEM_path)
  DEM_units <- terra::linearUnits(dem)
  adjusted_length <- config$length_scale/DEM_units

  # --- makeGrids ---

  write_input_file_MakeGrids(
    DEM_path = DEM_path,
    length_scale = adjusted_length,
    scratch_folder = scratch_folder,
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
  setwd(scratch_folder)
  output <- system(command,
                   wait = TRUE)
  setwd(wd)
  if (output != 0) {
    warning("Problem calculating partial contributing area: error ", output)
  }

  # --- localRelief (DEV) ---

  if ("dev" %in% metrics) {
    localRelief_inputFile_path <-
      normalizePath(file.path(scratch_folder, "input_localRelief.txt"))

    write_input_file_localRelief(
      DEM_path = DEM_path,
      length_scale = adjusted_length,
      scratch_folder = scratch_folder,
      resample = dev_resample,
      interval = dev_interval,
      filename = localRelief_inputFile_path,
      output_file_extension = output_suffix
    )

    localRelief <- file.path(executable_path, "LocalRelief.exe")
    command <- paste(localRelief, localRelief_inputFile_path, sep = " ")
    setwd(scratch_folder)
    output <- system(command,
                     wait = TRUE)
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
    grad_path <- file.path(scratch_folder,
                           paste0("grad", output_suffix, ".flt"))
    plan_path <- file.path(scratch_folder,
                           paste0("plan", output_suffix, ".flt"))
    bcon_path <- file.path(scratch_folder,
                           paste0("bcon", output_suffix, ".flt"))
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
        normalizePath(file.path(scratch_folder, "twi_input_makeGrids.txt"))

      write_input_file_MakeGrids(
        DEM_path = DEM_path,
        length_scale = adjusted_length,
        scratch_folder = scratch_folder,
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
      setwd(scratch_folder)
      output <- system(command,
                       wait = TRUE)
      setwd(wd)
    }

    buildGrids_inputFile_path <-
      normalizePath(file.path(scratch_folder, "input_buildGrids.txt"))

    write_input_file_buildGrids(
      DEM_path = DEM_path,
      length_scale = adjusted_length,
      scratch_folder = scratch_folder,
      grad_path = grad_path,
      plan_path = plan_path,
      bcon_path = bcon_path,
      filename = buildGrids_inputFile_path,
      output_file_extension = output_suffix
    )


    buildGrids <- file.path(executable_path, "BuildGrids.exe")
    command <- paste(buildGrids, buildGrids_inputFile_path, sep = " ")
    setwd(scratch_folder)
    output <- system(command,
                     wait = TRUE)
    setwd(wd)
    if (output != 0) {
      warning("Problem calculating partial contributing area: error ", output)
    }
  }

  # ----- Partial Contributing Area ----- #
  # TODO: Need to add Partial.exe to ExecutableFiles.zip
  if (FALSE) {
    partial_inputFile_path <-
      normalizePath(file.path(scratch_folder, "input_partial.txt"))

    write_input_file_Partial(
      DEM_path = DEM_path,
      length_scale = length_scale,
      duration = pca_hours,
      conductivity =pca_conductivity,
      scratch_folder = config$scratch_folder,
      filename = partial_inputFile_path,
      output_file_extension = config$output_suffix
    )

    Partial <- paste0(executable_path, "\\Partial.exe")
    command <- paste(Partial, partial_inputFile_path, sep = " ")
    setwd(config$scratch_folder)
    output <- system(command,
                     wait = TRUE)
    setwd(wd)
    if (output != 0) {
      warning("Problem calculating partial contributing area: error ", output)
    }
  }

  # Reformat .flt files as .tif
  for (metric in metrics) {
    raster <- terra::rast(paste0(metric, output_suffix, ".flt"))
    terra::writeRaster(raster, paste0(metric, output_suffix, ".tif"))

    # Convert GDAL BIL flt header file to ESRI FLT header so it can
    # be read by ArcGIS
    convert_hdr(paste0(metric, config$output_suffix, ".hdr"))
  }

}
