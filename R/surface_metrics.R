# Functions for elevation derivatives and other things calculated from a DEM

#----------------------------------------------------------------
#' Elevation Derivatives
#'
#' Provide \code{SpatRasters} of elevation derivatives.
#' Elev_deriv operates in one of three modes, depending on the input arguments:
#' \enumerate{
#'   \item As a wrapper for the Fortran makegrids executable,
#'   with an existing makegrids input_file.
#'   \item As a wrapper for makegrids, but with the makegrids input file
#'   constructed by elev_deriv.
#'   \item To read existing raster files from disk.
#' }
#' In modes 1 and 2, elev_deriv calls makegrids which creates the requested
#' rasters and writes them to disk as floating point binary files. These are
#' then read and returned by elev_deriv as a \code{SpatRaster} object.
#' In mode 3, existing raster files are read directly from disk and returned
#' as a \code{SpatRaster} object.
#'
#' @param input_file: Character string; a makegrids input file (optional).
#'   If no input file is specified and no other arguments are present,
#'   a Windows Explorer window opens for file selection.
#' @param rasters: A character vector. Each element contains two strings
#'   separated by a comma. The first specifies the type of derivative,
#'   the second specifies the file location. These may be either input files
#'   to read or output files to write. Available elevation derivatives are:
#'   \enumerate{
#'     \item GRADIENT
#'     \item PLAN CURVATURE
#'     \item PROFILE CURVATURE
#'     \item NORMAL SLOPE CURVATURE
#'     \item TANGENTIAL CURVATURE
#'   }
#' @param length_scale: A numeric (dbl) value specifying the diameter
#'   in meters over which to measure the requested derivatives. Used for
#'   construction of a makegrids input file.
#' @param dem: The file name (full path) of the dem (elevation raster) for
#'   construction of a makegrids input file.
#' @param scratch_dir: A scratch directory where temporary files are written.
#'   If a makegrids input file is created, it is written here.
#'
#' @return \code{SpatRaster}
#' (see \href{https://rspatial.org/terra/pkg/index.html}{terra}),
#' one layer for each requested elevation derivative.
#'
#' @export
#'
elev_deriv <- function(input_file = "nofile",
                       rasters = vector("list", 0),
                       length_scale = 0.,
                       dem = "none",
                       scratch_dir = "none") {

  if (length(rasters) > 0) {

    # create a list of the file names specified in rasters
    file_list <- list()
    type_list <- list()
    for (i in 1:length(rasters)) {
      loc <- str_locate(rasters[[i]], ",")
      type_name <- str_sub(rasters[[i]], 1, loc[1,1] - 1)
      file_name <- str_sub(rasters[[i]], loc[1,1] + 1, -1)
      if (str_detect(file_name, ".flt") == FALSE) { # currently, makegrids
        file_name <- paste0(file_name, ".flt")      # only reads .flt
      }
      file_list <- c(file_list, file_name)
      type_list <- c(type_list, type_name)
    }

    if (length_scale == 0.) {
    # read existing raster files in file_list
      run_makegrids <- FALSE

    } else {
    # create a new makegrids input file
      makegrids_input(dem,
                      length_scale,
                      scratch_dir,
                      rasters)
      input_file <- paste0(scratch_dir, "\\makegrids_input.txt")
      run_makegrids = TRUE
    }

  } else {
    # use an existing makegrids ASCII input file
    if (str_detect(input_file, "nofile")) {
      input_file <- file.choose()
      infile <- tibble(readLines(input_file))
    } else {
      if (!file.exists(input_file)) {
        stop("Input file not found")
      } else {
        infile <- tibble(readLines(input_file))
      }
    }
    if (nrow(infile) == 0) {
      stop("Input file is empty")
    }
    # Get a list of output rasters
    file_list <- list()
    type_list <- list()
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "GRID") == TRUE) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument, 2)
        file_list <- c(file_list, param_value[[2]])
        type_list <- c(type_list, trimws(parse_arg(argument, 1)[[2]]))
      }
    }
    run_makegrids <- TRUE
  }

  if (run_makegrids == TRUE) {
    # Get the location of the Fortran compiled code for makegrids.exe
    executable_path <- get_executable_path()

    makeGrids <- file.path(executable_path, "MakeGrids.exe")
    command <- paste(makeGrids, input_file, sep = " ")
    output <- system(command, wait = TRUE)
    if (output != 0) {
      stop("Problem calculating elevation derivatives: error ", output)
    }
  }

  # Create a spatraster with one layer for each output grid
  out_grid <- rast(file_list[[1]])
  if (length(file_list) > 1) {
    for (i in 2:length(file_list)) {
    out_grid <- c(out_grid, rast(file_list[[i]]))
    }
  }
  names(out_grid) <- type_list # name each layer with the derivative type

  return(out_grid)
}
#---------------------------------------------------------
#' Contributing area for a storm of fixed duration
#'
#' Provide a \code{SpatRaster} giving the upslope contributing area to each
#' cell of a DEM for shallow subsurface groundwater flow to the cell.
#' D-infinity flow paths are traced upslope from each cell using a spatially
#' variable Darcy velocity (v = k*sin(gradient)) dependent on the specified
#' (spatially uniform) saturated hydraulic conductivity and the gradient of
#' each DEM cell.
#'
#' Contributing_area operates in one of three modes, depending on the
#' input arguments:
#' \enumerate{
#'   \item As a wrapper for the Fortran "partial" executable,
#'   with an existing "partial" input_file.
#'   \item As a wrapper for program partial, but with the input file
#'   constructed by contributing_area.
#'   \item To read existing raster files from disk.
#' }
#' In modes 1 and 2, contributing_area calls program partial, which creates the
#' requested rasters and writes them to disk as floating point binary files.
#' These are then read and returned by contributing_area as a \code{SpatRaster}
#' object. In mode 3, existing raster files are read directly from disk and
#' returned as a \code{SpatRaster} object.
#'
#' @param input_file Character string: a "partial" input file (optional).
#'   If no input file is specified and no other arguments are present,
#'   a Windows Explorer window opens for file selection.
#' @param raster Character string: file name (full path) for an existing
#'   contributing-area raster to read from disk (optional).
#' @param dem Character string: The file name (full path) of the dem
#'   (elevation raster) for construction of an input file.
#' @param length_scale Numeric (dbl): Length scale in meters over which to
#'   smooth the DEM. This is the length used to measure gradient, curvature,
#'   and guide flow directions.
#' @param k Numeric (dbl): Uniform saturated hydraulic conductivity for
#'   construction of an input file.
#' @param d Numeric (dbl): Storm duration (dbl) in hours. Used for construction
#'   of an input file for program partial.
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#'
#' @return A \code{SpatRaster} of contributing area for a storm of
#'   duration d hours.
#'
#' @export
#'
contributing_area <- function(input_file = "nofile",
                              raster = "nofile",
                              dem = 'none',
                              length_scale = 0.,
                              k = 0.,
                              d = 0.,
                              scratch_dir = "none") {

  if (!str_detect(raster, "nofile")) { # there is a raster file specified

    if (str_detect(dem, "none")) { # read an existing raster
      run_partial <- FALSE
    } else {
     # need to build input file and run partial
      err <- 0
      if (!str_detect(dem, ".flt")) {
        dem <- paste0(dem, ".flt")
      }
      if (!file.exists(dem)) {
        message("Cannot find the DEM file")
        err <- -1
      }
      if (k <= 0.) {
        message("Saturated hydraulic conductivity not specified")
        err <- -1
      }
      if (d <= 0.) {
        message("Storm duration not specified")
        err <- -1
      }
      if (length_scale <= 0.) {
        message("length_scale not specified")
        err <- -1
      }
      if (str_detect(scratch_dir, "none")) {
        message("Scratch directory not specified")
        err <- -1
      }
      if (str_detect(raster, "nofile")) {
        message("No output raster file specified")
        err <- -1
      }
      if (err == -1) {
        stop("Inconsistent arguments")
      }
      accum_input(dem,
                  k,
                  d,
                  length_scale,
                  scratch_dir,
                  raster)
      input_file <- paste0(scratch_dir, "\\partial_input.txt")
      if (!str_detect(raster, ".flt")) {
        raster <- paste0(raster, ".flt")
      }
      run_partial <- TRUE

    }
  } else {
  # read existing input_file and run partial
    if (str_detect(input_file, "nofile")) {
      input_file <- file.choose()
      infile <- tibble(readLines(input_file))
    } else {
      if (!file.exists(input_file)) {
        stop("Input file not found")
      } else {
        infile <- tibble(readLines(input_file))
      }
    }
    if (nrow(infile) == 0) {
      stop("Input file is empty")
    }
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "OUTPUT RASTER")) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument)
        raster <- param_value[[2]]
        if (!str_detect(raster, ".flt")) {
          raster <- paste0(raster, ".flt")
        }
      }
    }
    run_partial <- TRUE
  }

  if (run_partial) {
   # Get the location of the Fortran compiled code for makegrids.exe
    executable_path <- get_executable_path()

    partial <- file.path(executable_path, "Partial.exe")
  #  command <- paste(partial, input_file, sep = "  ")
    command <- paste0(partial, " ", input_file)
    output <- system(command, wait = TRUE)
    if (output != 0) {
      stop("Problem calculating contributing area: error ", output)
    }
  }
  # Create a spatraster with one layer for each output grid
  out_grid <- rast(raster)
  return(out_grid)
}

