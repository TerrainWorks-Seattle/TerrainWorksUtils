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
#'   constructed by elev_deriv.test
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
#'   the second specifies the file name. These may be either input files
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
                       dem = "none",
                       length_scale = 0.,
                       scratch_dir = "none") {

  if (str_detect(input_file, "nofile")) { # build a new input file

    if (str_detect(dem, "none")) { # read an existing raster
      if (!(length(rasters) > 0)) {
        stop("Must provide a DEM or an existing raster file to read")
      }

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
        if (!file.exists(file_name)) {
          message <- paste0(file_name, " does not exist.")
          stop(message)
        }
        file_list <- c(file_list, file_name)
        type_list <- c(type_list, type_name)
      }

      run_makegrids <- FALSE
    } else { # write to rasters

      # check arguments
      if (length_scale <= 0.) {
        stop("Length scale not specified or out-of-bounds")
      }
      if (!file.exists(dem)) {
        print("Cannot find specified DEM")
        print(dem)
        stop()
      }
      if (!dir.exists(scratch_dir)) {
        stop("Scratch directory does not exist")
      }
      if (!(length(rasters) > 0)) {
        stop("Must provide at least one derivative to calculate")
      }

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

      # create a new makegrids input file
      makegrids_input(dem,
                      length_scale,
                      scratch_dir,
                      rasters)
      input_file <- paste0(scratch_dir, "\\makegrids_input.txt")
      run_makegrids = TRUE
    }

  } else { # use an existing makegrids ASCII input file

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
    dem_found <- FALSE
    dir_found <- FALSE
    scale_found <- FALSE
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "DEM") == TRUE) {
        dem_found <- TRUE
      } else if (str_detect(keyword, "SCRATCH DIRECTORY") == TRUE) {
        dir_found <- TRUE
      } else if (str_detect(keyword, "LENGTH SCALE") == TRUE) {
        scale_found <- TRUE
      } else if (str_detect(keyword, "GRID") == TRUE) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument, 2)
        file_list <- c(file_list, param_value[[2]])
        type_list <- c(type_list, trimws(parse_arg(argument, 1)[[2]]))
      }
    }
    if (!dem_found | !dir_found | !scale_found) {
      stop("Bad input file format")
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

  if (str_detect(input_file, "nofile")) {
    # there is a raster file specified

    if (str_detect(dem, "none")) { # read an existing raster
      if (str_detect(raster, "nofile") | !file.exists(raster)) {
        stop("Must provide a DEM or an existing raster file to read")
      }
      run_partial <- FALSE
    } else {
     # need to build input file and run partial
      err <- 0
      if (!str_detect(dem, ".flt")) {
        dem <- paste0(dem, ".flt")
      }
      if (!file.exists(dem)) {
        stop("DEM does not exist")
        # err <- -1
      }
      if (k <= 0.) {
        stop("Saturated hydraulic conductivity not specified or out-of-bounds")
        # err <- -1
      }
      if (d <= 0.) {
        stop("Storm duration not specified or out-of-bounds")
        # err <- -1
      }
      if (length_scale <= 0.) {
        stop("Length scale not specified or out-of-bounds")
        # err <- -1
      }
      if (str_detect(scratch_dir, "none") | !dir.exists(scratch_dir)) {
        stop("Scratch directory does not exist or is not specified")
        # err <- -1
      }
      if (str_detect(raster, "nofile")) {
        stop("No output raster file specified")
        # err <- -1
      }
      # if (err == -1){
      #   stop("Inconsistent arguments")
      # }

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
    dem_found <- FALSE
    dir_found <- FALSE
    scale_found <- FALSE
    dur_found <- FALSE
    con_found <- FALSE
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "DEM") == TRUE) {
        dem_found <- TRUE
      } else if (str_detect(keyword, "SCRATCH DIRECTORY") == TRUE) {
        dir_found <- TRUE
      } else if (str_detect(keyword, "LENGTH SCALE") == TRUE) {
        scale_found <- TRUE
      } else if (str_detect(keyword, "DURATION") == TRUE) {
        dur_found <- TRUE
      } else if (str_detect(keyword, "CONDUCTIVITY") == TRUE) {
        con_found <- TRUE
      } else if (str_detect(keyword, "OUTPUT RASTER")) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument)
        raster <- param_value[[2]]
        if (!str_detect(raster, ".flt")) {
          raster <- paste0(raster, ".flt")
        }
      }
    }
    if (!dem_found | !dir_found | !scale_found | !dur_found | !con_found) {
      stop("Bad input file format")
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
#---------------------------------------------------------
#' Total contributing area
#'
#' Provide a \code{SpatRaster} giving the total upslope contributing area to
#' each cell of a DEM. D-infinity flow paths are traced upslope from each cell.
#'
#' Contributing_area operates in one of three modes, depending on the
#' input arguments:
#' \enumerate{
#'   \item As a wrapper for the Fortran "bldgrds" executable,
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
#' @param aspect_length Numeric (dbl): Length in meters over which
#'   aspect is measured.
#' @param plan_length Numeric (dbl): Length in meters over which
#'   plan curvature is measured.
#' @param grad_length Numeric (dbl): Length in meters over which
#'   gradient is measured.
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#'
#' @return A \code{SpatRaster} of total contributing area.
#'
#' @export
#'
bldgrds_nochannels <- function(input_file = "nofile",
                               raster = "nofile",
                               dem = 'none',
                               aspect_length = 0.,
                               plan_length = 0.,
                               grad_length = 0.,
                               scratch_dir = "none") {

  if (str_detect(input_file, "nofile")) {
    # there is a raster file specified

    if (str_detect(dem, "none")) { # read an existing raster
      if (str_detect(raster, "nofile") | !file.exists(raster)) {
        stop("Must provide a DEM or an existing raster file to read")
      }
      run_bldgrds <- FALSE
    } else {
      # need to build input file and run partial
      err <- 0
      if (!str_detect(dem, ".flt")) {
        dem <- paste0(dem, ".flt")
      }
      if (!file.exists(dem)) {
        stop("DEM does not exist")
        # err <- -1
      }
      if (aspect_length <= 0.) {
        stop("Aspect_length not specified or out-of-bounds")
        # err <- -1
      }
      if (plan_length <= 0.) {
        stop("Plan_length not specified or out-of-bounds")
        # err <- -1
      }
      if (grad_length <= 0.) {
        stop("Gradient length not specified or out-of-bounds")
        # err <- -1
      }
      if (str_detect(scratch_dir, "none") | !dir.exists(scratch_dir)) {
        stop("Scratch directory does not exist or is not specified")
        # err <- -1
      }
      if (str_detect(raster, "nofile")) {
        stop("No output raster file specified")
        # err <- -1
      }
      # if (err == -1){
      #   stop("Inconsistent arguments")
      # }

      bldgrds_nochannels_input(dem,
                               aspect_length,
                               plan_length,
                               grad_length,
                               raster,
                               scratch_dir)

      input_file <- paste0(scratch_dir, "/input_bldgrds_nochannels.txt")
      if (!str_detect(raster, ".flt")) {
        raster <- paste0(raster, ".flt")
      }
      run_bldgrds <- TRUE

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
    dem_found <- FALSE
    dir_found <- FALSE
    aspect_found <- FALSE
    plan_found <- FALSE
    grad_found <- FALSE
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "DEM") == TRUE) {
        dem_found <- TRUE
      } else if (str_detect(keyword, "SCRATCH DIRECTORY") == TRUE) {
        dir_found <- TRUE
      } else if (str_detect(keyword, "USE SMOOTHED ASPECT: LENGTH SCALE") == TRUE) {
        aspect_found <- TRUE
      } else if (str_detect(keyword, "PLAN CURVATURE LENGTH SCALE") == TRUE) {
        plan_found <- TRUE
      } else if (str_detect(keyword, "GRADIENT LENGTH SCALE") == TRUE) {
        grad_found <- TRUE
      } else if (str_detect(keyword, "OUTPUT FLOW ACCUMULATION RASTER")) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument)
        raster <- param_value[[2]]
        if (!str_detect(raster, ".flt")) {
          raster <- paste0(raster, ".flt")
        }
      }
    }
    if (!dem_found | !dir_found | !aspect_found | !plan_found | !grad_found) {
      stop("Bad input file format")
    }
    run_bldgrds <- TRUE
  }

  if (run_bldgrds) {
    # Get the location of the Fortran compiled code for makegrids.exe
    executable_path <- get_executable_path()

    bldgrds <- file.path(executable_path, "bldgrds.exe")
    #  command <- paste(partial, input_file, sep = "  ")
    command <- paste0(bldgrds, " ", input_file)
    output <- system(command, wait = TRUE)
    if (output != 0) {
      stop("Problem calculating contributing area: error ", output)
    }
  }
  # Create a spatraster with one layer for each output grid
  out_grid <- rast(raster)
  return(out_grid)
}

#---------------------------------------------------------
#' Distance to the nearest road in meters
#'
#' Provide a \code{SpatRaster} giving the distance to the nearest road for each
#' cell of a DEM.
#'
#' distance_to_road() operates in one of three modes, depending on the
#' input arguments:
#' \enumerate{
#'   \item As a wrapper for the Fortran "distanceToRoad" executable,
#'   with an existing "distanceToRoad" input_file.
#'   \item As a wrapper for program distanceToRoad, but with the input file
#'   constructed by distance_to_road.
#'   \item To read existing raster files from disk.
#' }
#' In modes 1 and 2, distance_to_road calls program distanceToRoad, which creates
#' the requested rasters and writes them to disk as floating point binary files.
#' These are then read and returned by distance_to_road as a \code{SpatRaster}
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
#' @param road_shapefile Character string: The file name (full path) to a
#'   polyline shapefile for roads.
#' @param radius double: Distance in meters to extend the search for a road.
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#'
#' @return A \code{SpatRaster} of distance to the closest road for each DEM grid point.
#'
#' @export
#'
distance_to_road <- function(input_file = "nofile",
                             raster = "nofile",
                             dem = 'none',
                             road_shapefile = "nofile",
                             radius = 0.,
                             scratch_dir = "none") {

  if (str_detect(input_file, "nofile")) {
    # there is a raster file specified

    if (str_detect(dem, "none")) { # read an existing raster
      if (str_detect(raster, "nofile") | !file.exists(raster)) {
        stop("Must provide a DEM or an existing raster file to read")
      }
      run_distToRoad <- FALSE
    } else {
      # need to build input file and run distanceToRoad
      err <- 0
      if (!str_detect(dem, ".flt")) {
        dem <- paste0(dem, ".flt")
      }
      if (!file.exists(dem)) {
        print("DEM does not exist")
        err <- -1
      }
      if (str_detect(scratch_dir, "none") | !dir.exists(scratch_dir)) {
        print("Scratch directory does not exist or is not specified")
        err <- -1
      }
      if (str_detect(raster, "nofile")) {
        print("No output raster file specified")
        err <- -1
      }
      if (radius == 0.) {
        print("Radius not specified")
        err <- -1
      }
      if (err < 0) {
        stop("Error with input arguments")
      }

      distanceToRoad_input(dem,
                           radius,
                           road_shapefile,
                           raster,
                           scratch_dir)

      input_file <- paste0(scratch_dir, "\\input_distanceToRoad.txt")
      if (!str_detect(raster, ".flt")) {
        raster <- paste0(raster, ".flt")
      }
      run_distanceToRoad <- TRUE

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
    dem_found <- FALSE
    dir_found <- FALSE
    radius_found <- FALSE
    shapefile_found <- FALSE
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "DEM") == TRUE) {
        dem_found <- TRUE
      } else if (str_detect(keyword, "SCRATCH DIRECTORY") == TRUE) {
        dir_found <- TRUE
      } else if (str_detect(keyword, "RADIUS") == TRUE) {
        radius_found <- TRUE
      } else if (str_detect(keyword, "ROAD SHAPEFILE") == TRUE) {
        shapefile_found <- TRUE
      } else if (str_detect(keyword, "OUTPUT RASTER")) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument)
        raster <- param_value[[2]]
        if (!str_detect(raster, ".flt")) {
          raster <- paste0(raster, ".flt")
        }
      }
    }
    if (!dem_found | !dir_found | !radius_found | !shapefile_found) {
      stop("Bad input file format")
    }
    run_distanceToRoad <- TRUE
  }

  if (run_distanceToRoad) {
    # Get the location of the Fortran compiled code for makegrids.exe
    executable_path <- get_executable_path()

    distanceToRoad <- file.path(executable_path, "distanceToRoad.exe")
    command <- paste0(distanceToRoad, " ", input_file)
    output <- system(command, wait = TRUE)
    if (output != 0) {
      stop("Problem calculating distanceToRoad: error ", output)
    }
  }
  # Create a spatraster with one layer for each output grid
  out_grid <- rast(raster)
  return(out_grid)
}

#---------------------------------------------------------
#' Function PFA_debris_flow;
#' terrain attributes along debris-flow runout path.
#' A wrapper for Fortran program PFA_debris_flow.
#'
#' Provides a csv file that can be read to a dataframe for a Cox survival model.
#'
#' A list of multinomial-logistic-regression-model coefficients for probability
#' of scour and deposition are required; these are hard wired into this code.
#'
#' @param input_file Character string: a "partial" input file (optional).
#'   If no input file is specified and no other arguments are present,
#'   a Windows Explorer window opens for file selection.
#' @param dem Character string: The file name (full path) of the dem
#'   (elevation raster) for construction of an input file.
#' @param init_points: string: File name for initiation-point shapefile.
#' @param geo_poly: string: File name for rock-type polygon shapefile.
#' @param stand_age: string: File name for LEMMA stand-age flt raster.
#' @param tracks: string: File name for DOGAMI debris-flow-track polyline shapefile.
#' @param radius: dbl: Search radius in meters for matching DEM flow path to DOGAMI track.
#' @param length_scale: dbl: Length in meters to measure elevation derivatives.
#' @param bulk_coef: dbl: Coefficient for linear bulking equation
#' @param alpha: dbl: Proportion of debris-flow cross-sectional volume deposited per unit length.
#' @param radius double: Distance in meters to extend the search for a road.
#' @param uncensored string: if "TRUE", interpret all end-point tracks as uncensored, even without mapped deposition
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#'
#' @return A csv data file.
#'
#' @export
#'
PFA_debris_flow <- function(dem = 'none',
                            init_points = "nofile",
                            geo_poly = "nofile",
                            stand_age = "nofile",
                            tracks = "nofile",
                            radius = 0.,
                            initRadius = 0.,
                            length_scale = 0.,
                            slope_intercept = 0.,
                            slope_coef = 0.,
                            bulk_coef = 0.,
                            init_width = 0.,
                            init_length = 0.,
                            DF_width = 0.,
                            alpha = 0.,
                            uncensored,
                            scratch_dir = "none") {

  err <- 0
  if (!str_detect(dem, ".flt")) {
    dem <- paste0(dem, ".flt")
  }
  if (!file.exists(dem)) {
    print("DEM does not exist")
    err <- -1
  }
  if (str_detect(scratch_dir, "none") | !dir.exists(scratch_dir)) {
    print("Scratch directory does not exist or is not specified")
    err <- -1
  }
  if (str_detect(init_points, "nofile")) {
    print("No output raster file specified")
    err <- -1
  }
  if (str_detect(geo_poly, "nofile")) {
    print("No geo_poly polygon shapefile specified")
    err <- -1
  }
  if (str_detect(stand_age, "nofile")) {
    print("No stand-age raster specified")
    err <- -1
  }
  if (str_detect(tracks, "nofile")) {
   print("No debris-flow track shapefile specified")
   err <- -1
  }
  if (radius == 0.) {
   print("Radius not specified")
   err <- -1
  }
  if (length_scale == 0.) {
    print("Length scale not specified")
    err <- -1
  }
  if (initRadius == 0.) {
    print("Initiation point radius not specified")
    err <- -1
  }

  if (alpha == 0.) {
    print("Alpha not specified")
    err <- -1
  }
  if (err < 0) {
    stop("Error with input arguments")
  }

# Coefficient values are hardwired here, but could be read from an input file.
  coef <- c(-5.590219, # scour intercept
          12.124613, # scour gradient
          -25.90498, # scour normal curvature
          62.17922, # scour tangent curvature
          -0.0018062539, # scour stand age
          0., # scour sedimentary
          0.8585555, # scour volcanic
          0.5779966, # scour igneous-metamorphic
          1.89509936, # scour volcaniclastic
          1.160043335, # scour unconsolidated
          -1.632, # transitional intercept
          2.313637, # transitional gradient
          10.49279, # transitional normal curvature
          35.33434, # transitional tangent curvature
          -0.0006935715, # transitional stand age
          0., # transitional sedimentary
          -0.1992108, # transitional volcanic
          -1.0804838, # transitional igneous-metamorphic
          0.05590894, # transitional volcaniclastic
          -0.006225561) # transitional unconsolidated

  out_surv <- paste0(scratch_dir, "\\out_surv.csv")
  out_point <- paste0(scratch_dir, "\\out_point")
  out_kaplanMeier <- paste0(scratch_dir, "\\out_KaplanMeier.csv")

  PFA_debris_flow_input(dem,
                        init_points,
                        geo_poly,
                        stand_age,
                        tracks,
                        radius,
                        initRadius,
                        length_scale,
                        slope_intercept,
                        slope_coef,
                        bulk_coef,
                        init_width,
                        init_length,
                        DF_width,
                        alpha,
                        uncensored,
                        scratch_dir,
                        out_surv,
                        out_point,
                        out_kaplanMeier,
                        coef)
  input_file <- paste0(scratch_dir, "\\input_PFA_debris_flow.txt")

  # Get the location of the Fortran compiled code for makegrids.exe
  executable_path <- get_executable_path()

  PFA_debris_flow <- file.path(executable_path, "PFA_debris_flow.exe")
  command <- paste0(PFA_debris_flow, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    stop("Problem calculating PFA_debris_flow: error ", output)
  }

  data_file <- read.csv(out_surv)
}

#---------------------------------------------------------
#' Deviation from mean elevation
#'
#' Provide a \code{SpatRaster} giving the deviation from mean elevation (DEV)
#' over a specified radius for each point of a DEM. DEV is defined for the
#' ith DEM grid point as
#' DEVi = (ei - mean(e))/sd(e), where ei is the elevation of the ith point,
#' mean(e) is the mean elevation of all DEM points within the specified radius,
#' and sd(e) is the standard deviation of those elevations.
#'
#' DEV() operates in one of three modes, depending on the
#' input arguments:
#' \enumerate{
#'   \item As a wrapper for the Fortran "LocalRelief" executable,
#'   with an existing "LocalRelief" input_file.
#'   \item As a wrapper for program LocalRelief, but with the input file
#'   constructed by DEV.
#'   \item To read existing raster files from disk.
#' }
#' In modes 1 and 2, DEV calls program LocalRelief, which creates
#' the requested rasters and writes them to disk as floating point binary files.
#' These are then read and returned by DEV as a \code{SpatRaster}
#' object. In mode 3, existing raster files are read directly from disk and
#' returned as a \code{SpatRaster} object.
#'
#' @param input_file Character string: a "LocalRelief" input file (optional).
#'   If no input file is specified and no other arguments are present,
#'   a Windows Explorer window opens for file selection.
#' @param raster Character string: file name (full path) for an existing
#'   DEV raster to read from disk (optional).
#' @param dem Character string: The file name (full path) of the dem
#'   (elevation raster) for construction of an input file.
#' @param radius double: Radius in meters to calculate DEV.
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#'
#' @return A \code{SpatRaster} of distance to the closest road for each DEM grid point.
#'
#' @export
#'
DEV <- function(input_file = "nofile",
                raster = "nofile",
                dem = 'none',
                radius = 0.,
                scratch_dir = "none") {

  if (str_detect(input_file, "nofile")) {
    # there is a raster file specified

    if (str_detect(dem, "none")) { # read an existing raster
      if (str_detect(raster, "nofile") | !file.exists(raster)) {
        stop("Must provide a DEM or an existing raster file to read")
      }
      run_DEV <- FALSE
    } else {
      # need to build input file and run distanceToRoad
      err <- 0
      if (!str_detect(dem, ".flt")) {
        dem <- paste0(dem, ".flt")
      }
      if (!file.exists(dem)) {
        print("DEM does not exist")
        err <- -1
      }
      if (str_detect(scratch_dir, "none") | !dir.exists(scratch_dir)) {
        print("Scratch directory does not exist or is not specified")
        err <- -1
      }
      if (str_detect(raster, "nofile")) {
        print("No output raster file specified")
        err <- -1
      }
      if (radius == 0.) {
        print("Radius not specified")
        err <- -1
      }
      if (err < 0) {
        stop("Error with input arguments")
      }

      DEV_input(dem,
                radius,
                raster,
                scratch_dir)

      input_file <- paste0(scratch_dir, "\\input_DEV.txt")
      if (!str_detect(raster, ".flt")) {
        raster <- paste0(raster, ".flt")
      }
      run_DEV <- TRUE

    }
  } else {
    # read existing input_file and run DEV
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
    dem_found <- FALSE
    dir_found <- FALSE
    radius_found <- FALSE
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "DEM") == TRUE) {
        dem_found <- TRUE
      } else if (str_detect(keyword, "SCRATCH DIRECTORY") == TRUE) {
        dir_found <- TRUE
      } else if (str_detect(keyword, "RADIUS") == TRUE) {
        radius_found <- TRUE
      } else if (str_detect(keyword, "OUTPUT RASTER")) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument)
        raster <- param_value[[2]]
        if (!str_detect(raster, ".flt")) {
          raster <- paste0(raster, ".flt")
        }
      }
    }
    if (!dem_found | !dir_found | !radius_found) {
      stop("Bad input file format")
    }
    run_DEV <- TRUE
  }

  if (run_DEV) {
    # Get the location of the Fortran compiled code for makegrids.exe
    executable_path <- get_executable_path()

    localRelief <- file.path(executable_path, "LocalRelief.exe")
    command <- paste0(localRelief, " ", input_file)
    output <- system(command, wait = TRUE)
    if (output != 0) {
      stop("Problem calculating DEV: error ", output)
    }
  }
  # Create a spatraster with one layer for each output grid
  out_grid <- rast(raster)
  return(out_grid)
}

#---------------------------------------------------------
