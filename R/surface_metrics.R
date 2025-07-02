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
#'     \item MEAN CURVATURE (Normal + Tangential)*0.5
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
#' @return A \code{SpatRaster} of DEV values.
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

    localRelief <- file.path(executable_path, "DEV.exe")
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
#' Resample
#'
#' Read an input DEM (.flt or .tif) and output a DEM of lower resolution.
#'
#' Resample operates as a wrapper for program resample.
#'
#' @param in_raster Character string: the DEM to be down sampled
#' @param skip Integer, the output cell length is skip * input cell length.
#' So if the input DEM has cell size 1m and skip is 2, the output DEM
#' has cell size 2m. If skip is 5, the output DEM has cellsize 5m.
#' @param out_raster The output DEM, written as a .flt file.
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#'
#' @return A .flt raster written to disk.
#'
#' @export
#'
resample <- function(in_raster = "nofile",
                skip = 0,
                out_raster = 'none',
                scratch_dir = "none") {

  err = 0

  if (!file.exists(in_raster)) {
        print("Input raster does not exist")
        err <- -1
      }

  if (skip == 0.) {
    print("skip value not specified")
    err <- -1
  }

  if (err < 0) {
    stop("Error with input arguments")
  }

  resample_input(in_raster,
                skip,
                out_raster,
                scratch_dir)

  input_file <- paste0(scratch_dir, "\\input_resample.txt")

  # Get the location of the Fortran compiled code for makegrids.exe
  executable_path <- get_executable_path()

  resample <- file.path(executable_path, "resample.exe")
  command <- paste0(resample, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    stop("Problem resampling")
  }
}
#---------------------------------------------------------
#' Align
#'
#' Coregister two DTMs.
#'
#' @param refDTM Character: The file name (full path) for the input 
#' reference DTM.
#' @param alignDTM Character: The file name (full path) for the input
#' DTM to align with the reference DTM.
#' @param refDSM Character: The file name (full path) for the input
#' reference DSM (digital surface model).
#' @param alignDSM Character: The file name (full path) for the input
#' DSM to align with the reference DTM.
#' @param iterations Numeric (int): Number of iterations to solve for the shift
#' @param dampener Numeric (dbl): Dampener for the shift, 1.0 or less
#' @param k Numeric (dbl): The number of inter-quartile ranges from q1 and q3
#' to use as a Tukey's fence for outlier removal
#' @param outDTM Character: File name (full path) for the output aligned DTM
#' @param tileNx (int): number of tiles in the x direction
#' @param tilesNy (int): number of tiles in the y direction
#' @param overlap (dbl): overlap between tiles
#' @param radius (dbl): radius in meters for measuring slope and aspect
#' @param nslope (int): number of gradient bins
#' @param maxSlope (dbl): maximum gradient for binning
#' @param nAzimuth (int): number of azimuth bins
#' @param outbins Character: output csv file of elevation differences
#' binned by slope and aspect
#' @param outDif Character: output elevation difference raster (.flt)
#' @param outOutlier Character: output outlier raster (.flt)
#' @param scratch_dir Character string: A scratch directory where temporary
#'   files are written. If an input file for program partial is created,
#'   it is written here.
#' @param executable_dir Character: The directory where the executable
#' file is located.
#'   
#' @return error code
#'
#' @export
#'
align <- function(refDTM = "nofile",
                  alignDTM = "nofile",
                  refDSM = "nofile",
                  alignDSM = "nofile",
                  iterations = 5,
                  k = 0,
                  dampener,
                  outDTM = "nofile",
                  tileNx = 0,
                  tileNy = 0,
                  overlap = 0.5,
                  radius = 15,
                  nslope = 7,
                  maxSlope = 1.0,
                  nAzimuth = 8,
                  outbins = "nofile",
                  outDif = "nofile",
                  outOutlier = "nofile",
                  scratch_dir = "none",
                  executable_dir = "none",
                  program_name = "align") {
  
  err = 0
  returnCode = 0
  
  if (!file.exists(refDTM)) {
    print("Input reference DTM does not exist")
    err <- -1
  }
  
  if (!file.exists(alignDTM)) {
    print("Input DTM to align does not exist")
    err <- -1
  }
  
  if (k < 0) {
    print("Tukey's fence parameter not specified")
    err <- -1
  }
  
  if (outDTM == "nofile") {
    print("Output DTM not specified")
    err <- -1
  }
  
  if (outbins == "nofile") {
    print("Output csv file not specified")
    err <- -1
  }
  
  if (outDif == "nofile") {
    print("Output difference raster not specified")
    err <- -1
  }
  
  if (outOutlier == "nofile") {
    print("Output outlier raster not specified")
    err <- -1
  } 
  
  if (scratch_dir == "none") {
    print("Scratch directory not specified")
    err <- -1
  }
  
  if (executable_dir == "none") {
    print("Executable directory not specified")
    err <- -1
  }
  
  if (err < 0) {
    returnCode = -1
    return(returnCode)
    stop("Error with input arguments")
  }
  
  TerrainWorksUtils::align_input(refDTM,
              alignDTM,
              refDSM,
              alignDSM,
              iterations,
              k,
              dampener,
              outDTM,
              tileNx,
              tileNy,
              overlap,
              radius,
              nslope,
              maxSlope,
              nAzimuth,
              outbins,
              outDif,
              outOutlier,
              scratch_dir)
  
  input_file <- paste0(scratch_dir, "/input_align.txt")
  
  align <- file.path(executable_dir, paste0(program_name, ".exe"))
  command <- paste0(align, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    returnCode = -1
    return(returnCode)
    stop("Problem aligning")
  }
  return(returnCode)
}

#---------------------------------------------------------
#' HuntLS
#' Program HuntLS reads an "outlier" raster created by program Align.
#' The "outlier" k values indicate the number of interquartile ranges
#' (q3-q1) a DoD de (delta elevation) value falls outside of the 
#' interquartile range: k = (de-q1)/(q3-a1) if k is less than q1 
#' and k = (de-q3)/(q3-q1) if k is greater than q3. Values of de
#' between q1 and q3 are given k values of zero. HuntLS associates 
#' k patches with field-mapped landslide locations surveyed for the
#' Post Mortem (Stewart et al., 2013) study. 
#' 
#' @param DEM Character: The reference DTM used for program Align.
#' @param Outlier Character: The input outlier raster (.flt) created by Align.
#' @param DoD Character: Input DoD raster (.flt) created by Align.
#' @param Accum Character: Input flow accumulation raster created by bldgrds.
#' @param AccumThreshold Numeric (dbl): Contributing area above which landslide
#' patches are precluded.
#' @param LSpnts Character: Input point shapefile of mapped landslide points.
#' @param IDfield Character: LSpnts attributes-table field name for record ID.
#' @param Radius Numeric (dbl): Search radius in meters for matching landslide
#' points to k-value patches.
#' @param AspectLength Numeric (dbl): Length in meters to calculate aspect.
#' @param GradLength Numeric (dbl): Length in meters to calculate gradient.
#' @param OutlierThreshold Numeric (dbl): Minimum absolute k value for a patch.
#' @param ScratchDir Character: Scratch directory.
#' @param OutPatch Character: Output patch raster (.flt).
#' @param OutGrad Character: Output gradient raster (.flt).
#' @param Outcsv Character: Output comma-delimited table.
#' @param executable_dir Character: The directory where the executable
#' file is located. 
#' 
#' @return returnCode, a value of zero indicates success
#' @export
#' 
huntLS <- function(DEM = "nofile",
                   Outlier = "nofile",
                   DoD = "nofile",
                   Accum = "nofile",
                   AccumThreshold = -9999.,
                   LSpnts = "nofile",
                   IDfield = "none",
                   Radius = -9999.,
                   AspectLength = -9999.,
                   GradLength = -9999.,
                   OutlierThreshold = -9999.,
                   ScratchDir = "nofile",
                   OutPatch = "nofile",
                   OutGrad = "nofile",
                   Outcsv = "nofile",
                   executable_dir = "none") {
  
  returnCode <- -1
  program_name <- "huntLS"
  
  if (!dir.exists(ScratchDir)) {
    stop("invalid scratch folder: ", ScratchDir)
  }
  
  err = 0

  if (!file.exists(DEM)) {
    print("Input DEM not found")
    err <- -1
  }   
  
  if (!file.exists(Outlier)) {
    print("Input Outlier raster not found")
    err <- -1
  }
  
  if (!file.exists(Accum)) {
    print("Input flow accumulation raster not found")
    err <- -1
  }
  
  if (!file.exists(LSpnts)) {
    print("Landslide point shapefile not found")
    print(LSpnts)
    err <- -1
  }
  
  if (IDfield == "none") {
    print("LSpnts IDfield not specified")
    err <- -1
  }

  if (AccumThreshold < 0) {
    print("Accumulation threshold not specified")
    err <- -1
  }
  
  if (Radius < 0.) {
    print("Search radius not specifie")
    err <- -1
  }
  
  if (AspectLength < 0.) {
    print("Aspect scale length not specified")
    err <- -1
  }
  
  if (GradLength < 0.) {
    print("Gradient scale length not specified")
    err <- -1
  }
  
  if (OutlierThreshold == -9999.) {
    print("Outlier threshold not specified")
    err <- -1
  }
  
  if (OutPatch == "nofile") {
    print("Output patch raster not specified")
    err <- -1
  }
  
  if (OutGrad == "nofile") {
    print("Output gradient raster not specified")
    err <- -1
  }
  
  if (Outcsv == "nofile") {
    print("Output csv table not specifie")
    err <- -1
  }
  
  if (executable_dir == "none") {
    print("Executable directory not specified")
    err <- -1
  }

  if (err < 0) {
    returnCode = -1
    return(returnCode)
    stop("Error with input arguments")
  }
  
  returnCode <- TerrainWorksUtils::huntLSinput(DEM,
                                 Outlier,
                                 DoD,
                                 Accum,
                                 AccumThreshold,
                                 LSpnts,
                                 IDfield,
                                 Radius,
                                 AspectLength,
                                 GradLength,
                                 OutlierThreshold,
                                 ScratchDir,
                                 OutPatch,
                                 OutGrad,
                                 Outcsv)

  if (returnCode != 0) {
    return(returnCode)
    stop("Error writing input file")
  }
  
  input_file <- paste0(ScratchDir, "/input_huntLS.txt")
  
  huntLS <- file.path(executable_dir, paste0(program_name, ".exe"))
  command <- paste0(huntLS, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    returnCode <- -1
    return(returnCode)
    stop("Problem hunting")
  }
  returnCode <- 0
  return(returnCode)
}
#---------------------------------------------------------
#' LShunter
#' 
#' Program LShunter identifies potential landslide sites using 
#' "outlier patches" on a DoD (DEM of Difference) delineated on
#' an "outlier" raster created by Program Align. 
#' 
#' @param DoD Character: The reference DoD (DEM of Difference) raster (.flt) created by Align.
#' @param Outlier Character: The outlier raster (.flt) created by Align.
#' @param threshold1 Numeric (dbl): Outlier threshold for the 1st round.
#' @param threshold2 Numeric (dbl): Outlier threshold for the 2nd round.
#' @param Gradient Character: An input gradient raster (.flt) created by program HuntLS.
#' @param min1 Numeric (dbl): Minimum gradient for the 1st round.
#' @param min2 Numeric (dbl): Minimum gradient for the 2nd round.
#' @param Accum Character: Input flow accumulation raster (.flt) created by bldgrds.
#' @param maxAccum1 Numeric (dbl): Maximum flow accumulation for the 1st round.
#' @param maxAccum2 Numeric (dbl): Maximum flow accumulation for the 2nd round.
#' @param MinSize Numeric (dbl): Minimum size of a patch in square meters.
#' @param OutPatch Character: Output patch raster (.flt)
#' @param ScratchDir Character: Scratch directory.
#' @param Executable_dir Character: The directory where the executable file is located
#' 
#' @return returnCode, a value of zero indicates success
#' @export
#' 
LShunter <- function(
    DoD = "nofile",
    Outlier = "nofile",
    threshold1 = -9999.,
    threshold2 = -9999.,
    Gradient = "nofile",
    min1 = -9999.,
    min2 = -9999.,
    Accum = "nofile",
    maxAccum1 = -9999.,
    maxAccum2 = -9999.,
    minSize = -9999.,
    OutPatch = "nofile",
    ScratchDir = "none",
    Executable_dir = "none") {
  
  returnCode = -1
  program_name <- "LShunter"
  
  if (!dir.exists(ScratchDir)) {
    stop("invalid scratch folder: ", ScratchDir)
  }
  
  err = 0
  
  if (!file.exists(DoD)) {
    print("Input DoD not found")
    err <- -1
  }
  
  if (!file.exists(Outlier)) {
    print("Input Outlier raster not found")
    err <- -1
  }
  
  if (threshold1 == -9999.) {
    print("Threshold1 not specified")
    err <- -1
  }
  
  if (threshold2 == -9999.) {
    print("Threshold2 not specified")
    err <- -1
  }
  
  if (!file.exists(Gradient)) {
    print("Input Gradient raster not found")
    err <- -1
  }
  
  if (min1 <= 0.) {
    print("Minimum round1 gradient not specified")
    err <- -1
  }
  
  if (min2 <= 0.) {
    print("Minimum round2 gradient not specified")
    err <- -1
  }
  
  if (!file.exists(Accum)) {
    print("Input flow accumulation raster not found")
    err <- -1
  }
  
  if (maxAccum1 <= 0.) {
    print("Maximum round1 flow accumulation not specified")
    err <- -1
  }
  
  if (maxAccum2 <= 0.) {
    print("Maximum round2 flow accumulation not specified")
    err <- -1
  }
  
  if (minSize < 0.) {
    print("Minimum patch size not specified")
    err <- -1
  }
  
  if (OutPatch == "nofile") {
    print("Output patch raster not specified")
    err <- -1
  }
  
  if (Executable_dir == "nofile") {
    print("Executable directory not specified")
    err <- -1
  }
  
  if (err < 0) {
    returnCode = -1
    return(returnCode)
    stop("Error with input arguments")
  }
  
  returnCode <- TerrainWorksUtils::LShunterInput(
    DoD,
    Outlier,
    threshold1,
    threshold2,
    Gradient,
    min1,
    min2,
    Accum,
    maxAccum1,
    maxAccum2,
    minSize,
    OutPatch,
    ScratchDir)
 
  if (returnCode != 0) {
    return(returnCode)
    stop("Error writing input file")
  }
  
  input_file <- paste0(ScratchDir, "/input_LShunter.txt")
  
  LShunter <- file.path(Executable_dir, paste0(program_name, ".exe"))
  command <- paste0(LShunter, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    returnCode <- -1
    return(returnCode)
    stop("Problem hunting")
  }
  returnCode <- 0
  return(returnCode)
  
}
#---------------------------------------------------------
#' LS_poly
#' 
#' LS_poly reads a polygon shapefile of landslide scars. 
#' It generates centerlines through each polygon and builds a 
#' linked-node list through the centerlines. A likely initiation zone
#' is delineated from the upslope end of each polygon, extending downslope
#' from the top of the polygon a distance equal to the average polygon width.
#' Statistics are calculated for the initiation zones, including the metrics
#' for gradient, tangential curvature, profile curvature, and Factor-of-Safety
#' (FoS). Gradient and curvature rasters can be read from disk or calculated.
#' If calculated by LS_poly, radius values must be provided. The calculated
#' rasters may be written to disk for use on subsequent runs of LS_poly.
#' A FoS input raster must be specified. This may be generated with the 
#' FoS program.
#' 
#' @param DEM Character: input DEM (.flt or .tif), full path name
#' @param polyFile Character: input landslide polygon shapefile
#' @param polyID Character: name of the ID field for the input polygons
#' @param inGrad Character: input gradient raster (.flt or .tif), full path name
#' @param inTan Character: input tangential curvature raster (.flt or .tif), full path name
#' @param inProf Character: input profile curvature raster (.flt or .tif), full path name
#' @param inFoS Character: input factor of safety (FoS) raster (.flt or .tif), full path name
#' @param outGrad Character: output gradient raster (.flt or .tif), full path name
#' @param gradRadius Numeric (dbl): radius in meters for calculating gradient
#' @param outTan Character: output tangential curvature raster (.flt or .tif), full path name
#' @param tanRadius Numeric (dbl): radius in meters for calculating tangential curvature
#' @param outProf Character: output profile curvature raster (.flt or .tif), full path name
#' @param profRadius Numeric (dbl): radius in meters for calculating profile curvature
#' @param outNodes Character: output node point shapefile (.shp), full path name
#' @param outCsv Character: output csv file with patch statistics, full path name
#' @param outInit Character: output initiation zone raster (.flt or .tif), full path name
#' @param scratchDir Character: scratch directory, full path name
#' @returns returnCode, a value of zero indicates success
#' @export
#' 
LS_poly <- function(
  DEM = "nofile",
  polyFile = "nofile",
  polyID = "none",
  inGrad = "nofile",
  inTan = "nofile",
  inProf = "nofile",
  inFoS = "nofile",
  outGrad = "nofile",
  gradRadius = -9999.,
  outTan = "nofile",
  tanRadius = -9999.,
  outProf = "nofile",
  profRadius = -9999.,
  outNodes = "nofile",
  outCsv = "nofile",
  outInit = "nofile",
  scratchDir = "none",
  executableDir = "none") {
  
  returnCode <- -1
  
  if (!dir.exists(scratchDir)) {
    stop("invalid scratch folder: ", scratchDir)
  }
  
  err <- 0
  
  if (!file.exists(DEM)) {
    print("Input DEM not found")
    err <- -1
  }
  
  if (!file.exists(polyFile)) {
    print("Input polygon shapefile not found")
    err <- -1
  }
  
  if (polyID == "none") {
    print("PolyID not specified")
    err <- -1
  }
  
  if (inGrad != "nofile") {
    if (!file.exists(inGrad)) {
      print("Input gradient raster not found")
      err <- -1
    }
  }
  
  if (inTan != "nofile") {
    if (!file.exists(inTan)) {
      print("Input tangential curvature raster not found")
      err <- -1
    }
  }
  
  if (inProf != "nofile") {
    if (!file.exists(inProf)) {
      print("Input profile curvature raster not found")
      err <- -1
    }
  } 
  
  if (!file.exists(inFoS)) {
    print("Input factor of safety raster not found")
    err <- -1
  }
  
  if (outGrad != "nofile") {
    if (gradRadius <= 0.) { 
      print("Gradient radius not specified")
      err <- -1
    }
  }
  
  if (outTan != "nofile") {
    if (tanRadius <= 0.) {
      print("Tangential curvature radius not specified")
      err <- -1
    }
  }
  
  if (outProf != "nofile") {
    if (profRadius <= 0.) {
      print("Profile curvature radius not specified")
      err <- -1
    }
  }
  
  if (err != 0) {
    returnCode <- -1
    return(returnCode)
    stop("Error with input arguments")
  }
  
  returnCode <- TerrainWorksUtils::LS_poly_input(
    DEM,
    polyFile,
    polyID,
    inGrad,
    inTan,
    inProf,
    inFoS,
    outGrad,
    gradRadius,
    outTan,
    tanRadius,
    outProf,
    profRadius,
    outNodes,
    outCsv,
    outInit,
    scratchDir)
  
  if (returnCode != 0) {
    return(returnCode)
    stop("Error writing input file")
  } 
  
  input_file <- paste0(scratchDir, "/input_LS_poly.txt")
  
  LS_poly <- file.path(paste0(executableDir, "\\LS_poly.exe"))
  command <- paste0(LS_poly, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    returnCode <- -1
    return(returnCode)
    stop("LS_poly aborted")
  }
  
  returnCode <- 0 
}
#--------------------------------------------------------
#' samplePoints
#' 
#' SamplePoints is written to generate point samples for zones inside and
#' outside of mapped landslide initiation zones. It is written specifically
#' for working with output files from LS_poly, which I've been developing for 
#' analysis of the Tongass landslide inventory on Wrangell Island. 
#' The primary output from LS_poly used here is a raster of landslide 
#' initiation zones. These are used with a series of specified input rasters
#' to define the range of predictor values found within the initiation patches.
#' A raster mask is then defined to delineate areas within that range. 
#' Random point samples are then generated inside the initiation zones and 
#' outside the zones within the area of the mask. A buffer is placed around
#' each point to preclude point placements too close to each other. 
#' Bins are generated for increments of each predictor for the entire 
#' initiation zone and mask area and for the inside and outside samples. 
#' These are used to compare the frequency distribution of values both 
#' between inside and outside the initiation zones and between the total area
#' and the sample points.
#' 
#' @param inRaster Character: input initiation zone raster (.flt) generated by LS_poly.
#' @param areaPerSample Numeric (dbl): area in square meters for each sample point. This
#' determines the target number of points inside initiation zones.
#' @param buffer_in Numeric (dbl): distance in meters to buffer around each point inside initiation zones.
#' @param buffer_out Numeric (dbl): distance in meters to buffer around each point outside initiation zones.
#' @param margin Numeric (dbl): distance in meters to preclude point placement from the
#' edge of the initiation zones. 
#' @param ratio Numeric (dbl): ratio of the number of points outside the initiation zones
#' to the number inside, i.e., ratio = #outside/#inside.
#' @param nbins Numeric (int): number of bins for each predictor.
#' @param R4rasters (list): a list of real single-precision rasters to use as predictors. 
#' Each element of the list includes a name to assign the raster, the raster-file, 
#' and lower and upper cutoff values.
#' @param I4rasters (list): a list of integer rasters to use as predictors. Each integer
#' represents a nominal class, e.g., landform type.
#' @param minPatch Numeric (dbl): minimum size in square meters for patches delineated in the raster mask.
#' @param inPoints Character: output shapefile name (full path) for the inside points.
#' @param outPoints Character: output shapefile name (full path) for the outside points.
#' @param outMask Character: output raster (.flt) name (full path) for the mask.
#' @param outInit Character: output raster (.flt) name (full path) for the initiation zones sampled.
#' @param table Character: output table name (full path) for the binning results.
#' @param scratchDir Character: scratch directory.
#' @param executableDir Character: The directory where the executable file is located.
#' 
#' @returns returnCode, a value of zero indicates success
#' @export 
#' 
samplePoints <- function(
    inRaster = "nofile",
    areaPerSample = -9999.,
    buffer_in = -9999.,
    buffer_out = -9999.,
    margin = -9999.,
    ratio = -9999.,
    nbins = -9999,
    R4rasters,
    I4rasters,
    minPatch = -9999.,
    inPoints = "nofile",
    outPoints = "nofile",
    outMask = "nofile",
    outInit = "nofile",
    table = "nofile",
    scratchDir = "nofile",
    executableDir = "none") {
  
  returnCode <- -1
  
  if (!dir.exists(scratchDir)) {
    stop("invalid scratch folder: ", scratchDir)
  }
  err <- 0
  
  if (!file.exists(inRaster)) {
    if (!file.exists(paste0(inRaster, ".flt"))) {
      print("Input initiation zone raster not found")
      err <- -1
    }
  }
  if (areaPerSample <= 0.) {
    print("Area per sample not specified")
    err <- -1
  }
  if (buffer_in < 0.) {
    print("Buffer in not specified")
    err <- -1
  }
  if (buffer_out < 0.) {
    print("Buffer out not specified")
    err <- -1
  }
  if (margin < 0.) {
    print("Margin not specified")
    err <- -1
  }
  if (ratio <= 0.) {
    print("Ratio not specified")
    err <- -1
  }
  if (nbins <= 0) {
    print("Number of bins not specified")
    err <- -1
  }
  if (minPatch <= 0.) {
    print("Minimum patch size not specified")
    err <- -1
  }
  if (inPoints == "nofile") {
    print("Output inside points shapefile not specified")
    err <- -1
  }
  if (outPoints == "nofile") {
    print("Output outside points shapefile not specified")
    err <- -1
  }
  if (outMask == "nofile") {
    print("Output mask raster not specified")
    err <- -1
  }
  if (outInit == "nofile") {
    print("Output initiation zones raster not specified")
    err <- -1
  }
  if (table == "nofile") {
    print("Output table not specified")
    err <- -1
  }
  if (executableDir == "none") {
    print("Executable directory not specified")
    err <- -1
  }
  
  if (err != 0) {
    returnCode <- -1
    return(returnCode)
    stop("Error with input arguments")
  }
  
  returnCode <- TerrainWorksUtils::samplePointInput(
    inRaster,
    areaPerSample,
    buffer_in,
    buffer_out,
    margin,
    ratio,
    nbins,
    R4rasters,
    I4rasters,
    minPatch,
    inPoints,
    outPoints,
    outMask,
    outInit,
    table,
    scratchDir)
  
  if (returnCode != 0) {
    return(returnCode)
    stop("Error writing input file")
  }

  input_file <- paste0(scratchDir, "/input_samplePoints.txt")
  
  samplePoints <- file.path(paste0(executableDir, "/samplePoints.exe"))
  command <- paste0(samplePoints, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    returnCode <- -1
    return(returnCode)
    stop("Problem sampling")
  }
  returnCode <- 0
}   
#-------------
#' Quantiles
#' 
#' Program Quantiles reads a raster file and computes quantiles
#' over a moving circular window. For each iteration of the window,
#' the interquartile range is determined and outliers, based
#' on a Tukey's fence with k = 1.5, are removed. Quartiles are then
#' recalculated using the remaining values. Each pixel of the raster with
#' a value z less than q1 is assigned a value (z-q1)/(q3-q1) and each
#' pixel with a value greater than q3 is assigned a value of (z-q3)/(q3-q1).
#' This shows how many interquartile ranges the pixel value is from the first
#' or third quartile and serves as a measure of how extreme the value is.
#' Quantiles will also output z scores for each pixel.
#' @param in_raster Character: The file name (full path) for the input raster.
#' @param radius Numeric (dbl): radius in meters for the moving window
#' @param buffer Numeric (dbl): spacign between moving window center points,
#' in raster cells.
#' @param out_outlier Character: output file name (full path) for the 
#' outlier raster (optional)
#' @param out_q1 Character: output file name (full path) for the
#' first-quartile raster (optional)
#' @param out_q2 Character: output file name (full path) for the
#' median raster (optional)
#' @param out_q3 Character: output file name (full path) for the
#' third-quartile raster (optional)
#' @param out_mean Character: output file name (full path) for the mean raster
#' @param out_zscore Character: output file name (full path) for the 
#' z-score raster
#' @param out_prob Character: output file name (full path) for the
#' probability raster (optional)
#' @param scratch_dir Charcter: scratch directory
#' @param executable_dir Character: directory where the executable is located
#' @param program_name Character: name of the executable
#'   
#' @return error code
#'
#' @export
#'
quantiles <- function(in_raster = "nofile",
                      radius = 0.,
                      buffer = 0,
                      out_outlier = "nofile",
                      out_q1 = "nofile",
                      out_q2 = "nofile",
                      out_q3 = "nofile",
                      out_mean = "nofile",
                      out_zscore = "nofile",
                      out_prob = "nofile",
                      scratch_dir = "none",
                      executable_dir = "none",
                      program_name = "quantiles") {
  
  err = 0
  returnCode = 0
  
  if (!file.exists(in_raster)) {
    print("Input raster does not exist")
    err <- -1
  }
  
  if (radius == 0.) {
    print("Radius not specified")
    err <- -1
  }
  
  if (buffer == 0) {
    print("Buffer not specified")
    err <- -1
  }
  
  if (executable_dir == "none") {
    print("Executable directory not specified")
    err <- -1
  }
  
  if (scratch_dir == "none") {
    print("Scratch directory not specified")
    err <- -1
  }
  
  if (err < 0) {
    returnCode = -1
    return(returnCode)
    stop("Error with input arguments")
  }
  
  quantiles_input(in_raster,
                  radius,
                  buffer,
                  out_outlier,
                  out_q1,
                  out_q2,
                  out_q3,
                  out_mean,
                  out_zscore,
                  out_prob,
                  scratch_dir)
  
  input_file <- paste0(scratch_dir, "/input_quantiles.txt")
  
  quantiles <- file.path(executable_dir, paste0(program_name, ".exe"))
  command <- paste0(quantiles, " ", input_file)
  output <- system(command, wait = TRUE)
  if (output != 0) {
    returnCode = -1
    return(returnCode)
    stop("Quantiles failed")
  }
  return(returnCode)
}

#---------------------------------------------------------
