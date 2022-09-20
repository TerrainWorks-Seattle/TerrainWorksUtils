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
#' @param input_file: Character string; makegrids input file. If no input file
#'   is specified and no other arguments are present,
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
#' @param length_scale: A numeric (double) value specifying the diameter
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

# if input rasters, check for file extension.
# rast fails without extension.
  if (length(rasters) > 0) {

    # create a list of the file names specified in rasters
    file_list <- list()
    for (i in 1:length(rasters)) {
      loc <- str_locate(rasters[[i]], ",")
      file_name <- str_sub(rasters[[i]], start = loc[1,1] + 1, end = -1)
      file_list <- c(file_list, file_name)
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
    for (i in 1:nrow(infile)) {
      keyword <- get_keyword(infile, i)
      if (is.na(keyword)) {
        next
      }
      if (str_detect(keyword, "GRID") == TRUE) {
        argument <- get_args(infile, i)
        param_value <- parse_arg(argument, 2)
        file_list <- c(file_list, param_value[[2]])
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

  return(out_grid)
}
